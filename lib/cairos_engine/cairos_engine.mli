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
    ('freq result, string) Stdlib.result
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
