(** [Cairos_engine] — vectorized backtest loop for the Cairos time-series
    library. *)

module Trade : sig
  (** A round-trip trade record produced by [Backtest.run]. *)

  type t = private {
    entry_timestamp : Ptime.t;
    exit_timestamp : Ptime.t;
    instrument : string;
    entry_price : float;
    exit_price : float;
    pnl : float;
    holding_period_bars : int;
  }
  (** [entry_price] and [exit_price] are the inception and final-resolution
      prices of the round-trip trade. Same-direction size-adjustment prices
      mid-holding are absorbed into [pnl] and not surfaced as separate fields.

      [pnl] is the realised round-trip P&L net of all entry, mid-segment, and
      exit costs paid during the holding period. For a position with
      same-direction size adjustments mid-holding, [pnl] is the sum of
      per-segment contributions; it is not
      [(exit_price -. entry_price) *. weight].

      For a position still open at the last bar of the price [Frame],
      [exit_timestamp] is the last bar's timestamp and [exit_price] is the last
      bar's close (not the next bar's open — there is no next bar). No exit cost
      is deducted on a force-close. *)
end

module Backtest : sig
  (** [Backtest.run] — the vectorized backtest loop entrypoint. *)

  (** {1 Errors}

      [run] returns a structured error so callers can pattern-match on the
      failure mode and recover the offending timestamps, columns and values
      without scanning error strings. *)

  type calendar_violation =
    | Precedes_first_bar of { timestamp : Ptime.t }
        (** Rebalance date earlier than the price frame's first bar. *)
    | No_matching_row of { timestamp : Ptime.t }
        (** Rebalance date matches no row in the price frame. *)
    | Last_bar_no_next_open of { timestamp : Ptime.t }
        (** Rebalance date is the price frame's last bar, so there is no T+1
            open to execute against. *)

  type err =
    | Index_mismatch
        (** Price and signal frames carry different [Index.t] values. *)
    | Column_mismatch of { price : string list; signal : string list }
        (** Price and signal frames carry different columns, or the same columns
            in a different order. Both lists are given in frame order. *)
    | Empty_rebalance_index  (** The rebalance index carries no timestamps. *)
    | Calendar_violations of calendar_violation Cairos.Nonempty.t
        (** Every offending rebalance date, in rebalance-index order — the
            calendar tier aggregates rather than failing on the first. *)
    | No_nonzero_target_weight
        (** No (rebalance date, instrument) pair carries a non-zero target
            weight, so the backtest would produce no trades. *)
    | Nan_signal_at_rebalance of {
        cells : (Ptime.t * string) Cairos.Nonempty.t;
      }
        (** One or more signal cells on a rebalance row are NaN. [cells] lists
            every offending (timestamp, instrument) pair, in rebalance-index
            order then frame-column order. The check is scoped to rebalance rows
            — the loop reads signal values nowhere else, so a NaN off a
            rebalance date is not an error. Reported ahead of
            {!No_nonzero_target_weight}: an all-NaN rebalance row has no
            non-zero weight either, and the NaN is the cause. *)
    | Invalid_price of { cells : (Ptime.t * string * float) Cairos.Nonempty.t }
        (** One or more price cells the loop reads at non-zero exposure are not
            strictly positive and finite — NaN, either infinity, [0.0] and
            negatives all fail the one predicate. [cells] lists every offending
            (timestamp, instrument, rejected value) triple in bar order then
            frame-column order; the rejected value is carried so the caller need
            not re-scan the frame.

            The check is scoped to the cells the loop actually reads at non-zero
            exposure, so a frame may carry an instrument that never trades with
            arbitrary prices. A cell is in scope when either:

            - it is the T+1 execution price of an instrument whose weight delta
              at a rebalance date is non-zero; or
            - [held.(t) <> 0.0 || held.(t-1) <> 0.0], where [held] is the target
              weight carried forward between rebalance dates. The clause is
              two-sided because mark-to-market at bar [t] applies [held.(t-1)]
              to both [price.(t)] and [price.(t-1)] — the bar an instrument
              exits on carries zero held weight yet is still read at full
              exposure. *)

  val err_to_string : err -> string
  (** Render [err] as a human-readable message. Multi-line for
      {!Calendar_violations}, which lists one violation per line. *)

  type 'freq result = private {
    equity_curve : ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t;
    returns : ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t;
    weights : 'freq Cairos.Frame.t;
    trades : Trade.t Cairos.Nonempty.t;
  }
  (** [equity_curve], [returns], and [weights] all share the price [Frame]'s
      [Index.t] (one row per price-frame bar). [weights] carries the held
      weights at the end of each price-frame bar — constant between rebalance
      dates, jumping to the new target weights at each rebalance bar.

      [trades] is non-empty: under the entrypoint precondition that at least one
      rebalance row carries a non-zero target weight, the loop produces at least
      one [Trade.t]. *)

  val run :
    price_frame:'freq Cairos.Frame.t ->
    signal_frame:'freq Cairos.Frame.t ->
    rebalance_index:'rebal Cairos.Index.t ->
    commission:float ->
    slippage:float ->
    ('freq result, err) Stdlib.result
  (** [Backtest.run ~price_frame ~signal_frame ~rebalance_index ~commission
       ~slippage] runs the vectorized backtest loop and returns a
      [Backtest.result] or an [Error] enumerating any entrypoint-validation
      failures.

      [commission] and [slippage] are dimensionless fractions (e.g. [0.001] is
      10 basis points). Both are required: there is no default. The
      per-rebalance per-instrument cost is
      [(commission +. slippage) *. abs(weight_delta) *. nav], where [nav] is the
      pre-cost NAV at the rebalance bar (post-MTM, pre-deduction). Cost is thus
      a pure function of turnover as a fraction of NAV, independent of price
      level; the total is deducted from NAV before new weights are applied.

      [rebalance_index]'s frequency is independent of [price_frame]'s frequency;
      entrypoint validation enforces calendar alignment.

      Funding costs (carry on short positions, financing on leveraged longs) are
      not modelled. See Post-MVP v1 for event-driven execution with financing.
  *)
end
