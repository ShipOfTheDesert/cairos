# /// script
# requires-python = ">=3.12"
# dependencies = ["nautilus_trader==1.230.0"]
# ///
"""Nautilus oracle: the shared scenarios run through an order-level backtest engine.

Second of the two third-party oracles. The engine is the system under test;
`reference.py`'s clean-room `backtest_reference` proves conformance to the
specification; this script and `vectorbt_oracle.py` are independently authored
backtesters that can indicate the specification itself is wrong. Output is
committed under `oracle_fixtures/` and compared by
`test/unit/cairos_engine/cross_validate_oracles.exe`.

    uv run validation/nautilus_oracle.py            # write the fixture family
    uv run validation/nautilus_oracle.py --probe     # re-run the timing probe

Pinned to an isolated per-script environment by the PEP 723 header above.
`nautilus_trader==1.230.0` resolves in 14 packages and its wheels are published
for CPython 3.12, 3.13 and 3.14 — all three verified — so unlike the vectorbt
oracle this script needs no upper Python bound: nothing here goes through numba.
Nothing here is installed by the default gate, by `just validate`, or by CI.

Not validated against Cairos anywhere in this file: comparing an oracle to the
system it exists to check would recreate the circularity the whole exercise
removes. Its trustworthiness rests on being third-party code, on scenarios small
enough to check by hand, and on a third system disagreeing when one of the two
is wrong.


TRANSLATION
-----------
Identical in substance to the vectorbt oracle's, because it expresses one
property of the model difference rather than a per-library workaround.
`oracle_scenarios.py` documents both halves in full:

  1. MODEL TRANSLATION (a claim about equivalence, and testable): the held target
     weight is re-issued as a market order at EVERY bar, not only on rebalance
     bars. An order-driven engine holds a constant share count between orders, so
     its weights drift with prices; re-issuing restores the target weight each
     bar, which is what the engine's mark-to-market assumes. Nautilus still
     computes its own fills, share counts, cash balance, commissions and equity
     path — this configures which book it simulates, it does not hand it any
     answers.

  2. SCENARIO ARRANGEMENT (not a claim about equivalence): the price paths are
     shaped so two genuine convention differences — post-cost-NAV vs
     pre-cost-value targeting basis, and nominal vs drifted turnover basis — have
     nothing to act on at any measurement point. Neither is bridged here, and
     neither is confirmed by these fixtures.

Cost is charged by `RebalanceTagFeeModel` below: `(commission + slippage)` times
the absolute filled notional, on rebalance-bar orders only. The engine charges
cost only at rebalance bars, so the every-bar realignment has to be free —
pricing those tiny orders would invent turnover cost the engine never charges.
Charging on filled notional rather than marking the fill price up is also the
engine's own rule: its cost is additive on notional, whereas a slippage model
that moves the fill price is a different quantity.

At a rebalance bar the filled notional *is* the engine's cost basis, not merely
something close to it. The position carried into rebalance bar `r` is the
previous target re-issued at bar `r-1`, and the step into `r` is flat by
constraint (b), so the position at `r` is `w_old * nav_r / p_r` exactly; the
order that moves it to `w_new * nav_r / p_r` therefore has notional
`|w_new - w_old| * nav_r`, which is the engine's `|dw| * nav`.


EXECUTION TIMING — ESTABLISHED BY PROBE, NOT ASSUMED
-----------------------------------------------------
Assuming this engine's bar-execution timing is the single failure that would
make the oracle agree for the wrong reason, so it is read off Nautilus's own
fill records instead. `--probe` runs five bars at strictly distinct prices
(100, 110, 120, 130, 140) and submits one market buy from inside `on_bar` for
bar index 1, so the fill price alone identifies the bar. Recorded output, pasted
verbatim from `uv run validation/nautilus_oracle.py --probe` (the leading
`Pandas4Warning` line, which `engine.run()` emits on stderr, is the only thing
omitted):

    BAR PRICES: {0: 100.0, 1: 110.0, 2: 120.0, 3: 130.0, 4: 140.0}
    timestamps: ['2024-01-01T00:00:00Z', '2024-01-02T00:00:00Z', '2024-01-03T00:00:00Z', '2024-01-04T00:00:00Z', '2024-01-05T00:00:00Z']
    on_bar   i=0 close=  100.0 ts=2024-01-01T00:00:00Z net_pos=0 equity=1000000.000000000 CASH
    on_bar   i=1 close=  110.0 ts=2024-01-02T00:00:00Z net_pos=0 equity=1000000.000000000 CASH
      -> submit MARKET BUY 0.1 during on_bar i=1
      after submit_order: net_pos=0 equity=1000000.000000000 CASH
      FILLED last_px=110.0 qty=0.100000000 ts_event=1704153600000000000 commission=0.000000000 CASH
    on_bar   i=2 close=  120.0 ts=2024-01-03T00:00:00Z net_pos=0.100000000 equity=1000001.000000000 CASH
    on_bar   i=3 close=  130.0 ts=2024-01-04T00:00:00Z net_pos=0.100000000 equity=1000002.000000000 CASH
    on_bar   i=4 close=  140.0 ts=2024-01-05T00:00:00Z net_pos=0.100000000 equity=1000003.000000000 CASH

`ts_event` is Nautilus's own raw UNIX-nanosecond field, printed unformatted:
1704153600000000000 ns is 2024-01-02T00:00:00Z, bar 1's timestamp, not bar 2's.

Three facts follow, and all three are load-bearing:

  (i) An order submitted during `on_bar(i)` fills at bar `i`'s own price with bar
      `i`'s timestamp — 110.0 at 2024-01-02, not 120.0 at 2024-01-03. So the
      position is in force for the step `i -> i+1` and earns `p_{i+1}/p_i - 1`,
      which is exactly what the engine's held weight does: it takes effect at
      rebalance bar `r` and governs the step `r -> r+1`. The correct alignment is
      therefore an order at bar `i` here against a rebalance at bar `i` in
      Cairos, with NO signal shift. Shifting by one bar — the translation an
      order-driven system usually needs — would misalign every scenario. The
      engine's "filled at bar r+1's open" convention governs trade *record*
      prices, which these fixtures do not carry.

  (ii) The fill is not synchronous inside `submit_order`: `net_pos` and `equity`
      read immediately after submitting are still pre-trade. The fill is
      delivered before the next bar reaches `on_bar`, and while the mark price is
      still bar `i`'s. That is why equity for bar `i` is recorded from inside
      `on_order_filled` rather than at the end of `on_bar` — the engine's equity
      at a rebalance bar is net of that bar's cost, so a pre-trade read would sit
      above it by the whole cost, ~1.5e-3 on a NAV of ~1.

  (iii) Equity read at `on_bar(i)` is already marked at bar `i`'s price
      (1000001.0 = 1000000 + 0.1 * (120 - 110) at bar 2), so the venue processes
      the bar before the strategy sees it. Reading the mark is not deferred a bar.

Fact (i) is re-asserted on every fill in every scenario, not just in the probe:
each order records the price of the bar that submitted it, and `on_order_filled`
fails if the fill came back at any other price. A future version that queued
orders to the next bar would redden every scenario rather than quietly shifting
the fixtures by one bar.


PRECISION AND SCALING
---------------------
Cairos normalises equity to 1.0 at bar 0; Nautilus works in real currency units
at a fixed decimal precision. Running literally at an initial balance of 1.0
would size a 0.8 target at price 100 to 0.008 units, and rounding that to the
9-decimal quantity precision is a relative error of ~1e-7 — three orders of
magnitude above the comparison tolerance. So the account starts at `SCALE`
= 1_000_000 and the recorded path is divided by it. Quantities are then ~1e4,
where 9 decimals is ~1e-13 relative, and the cash currency is declared at
9 decimals so the equity path itself is not rounded to cents. Rounding does not
accumulate across bars: each bar targets from the *actual* position, so the
position lands within one increment of its target every bar rather than drifting.
"""

import sys
from datetime import datetime
from decimal import Decimal

from nautilus_trader.backtest.engine import BacktestEngine, BacktestEngineConfig
from nautilus_trader.backtest.models import FeeModel
from nautilus_trader.config import LoggingConfig, RiskEngineConfig
from nautilus_trader.model.data import Bar, BarSpecification, BarType
from nautilus_trader.model.enums import (
    AccountType,
    AggregationSource,
    BarAggregation,
    BookType,
    CurrencyType,
    OmsType,
    OrderSide,
    OtoTriggerMode,
    PriceType,
)
from nautilus_trader.model.events import OrderDenied, OrderRejected
from nautilus_trader.model.identifiers import InstrumentId, Symbol, Venue
from nautilus_trader.model.instruments import CurrencyPair
from nautilus_trader.model.objects import Currency, Money, Price, Quantity
from nautilus_trader.trading.strategy import Strategy

import oracle_scenarios as scen

VENUE = Venue("SIM")

# See PRECISION AND SCALING above: the account runs at 1e6 and the recorded
# equity path is divided back down, so quantity rounding stays ~1e-13 relative
# instead of ~1e-7.
SCALE = 1_000_000

# Nautilus's standard build is fixed-point at 9 decimals. Declaring the cash
# currency at that precision is what keeps the equity path from being rounded to
# cents. The price precision is not restated here: `oracle_scenarios.validate()`
# checks every scenario price is exact at it, so a later price needing a third
# decimal fails there rather than being rounded silently into a fixture that is
# wrong and still green.
DECIMALS = 9
PRICE_PRECISION = scen.PRICE_DECIMALS
SIZE_INCREMENT = Decimal(1).scaleb(-DECIMALS)
PRICE_INCREMENT = Decimal(1).scaleb(-PRICE_PRECISION)

CASH = Currency("CASH", DECIMALS, 0, "CASH", CurrencyType.CRYPTO)
Currency.register(CASH)

# Rebalance orders are tagged, and the fee model charges only tagged orders. The
# alternative — a mutable "is the current bar a rebalance bar" flag consulted at
# fill time — would be correct only as long as fills are delivered before the
# next bar advances the flag, which is a timing assumption this file exists to
# avoid making. The tag travels with the order.
REBALANCE_TAG = "REBALANCE"


def _base_currency(name: str) -> Currency:
    """A synthetic base currency per instrument, so each scenario instrument is a
    spot pair quoted in CASH. Fractional position sizes are the reason for a
    currency pair rather than an equity: the engine's weights are continuous and
    a whole-share instrument cannot express them."""
    code = f"X{name}"
    ccy = Currency(code, DECIMALS, 0, code, CurrencyType.CRYPTO)
    Currency.register(ccy)
    return ccy


def _instrument(name: str) -> CurrencyPair:
    symbol = Symbol(f"X{name}/CASH")
    return CurrencyPair(
        instrument_id=InstrumentId(symbol, VENUE),
        raw_symbol=symbol,
        base_currency=_base_currency(name),
        quote_currency=CASH,
        price_precision=PRICE_PRECISION,
        size_precision=DECIMALS,
        price_increment=Price(float(PRICE_INCREMENT), PRICE_PRECISION),
        size_increment=Quantity(float(SIZE_INCREMENT), DECIMALS),
        ts_event=0,
        ts_init=0,
        # Zero margin requirement plus the high default leverage below: the
        # engine imposes no margin constraint on a weight vector, so a margin
        # rejection here would be an artefact of the oracle's account model
        # rather than anything the scenario asks about.
        margin_init=Decimal(0),
        margin_maint=Decimal(0),
        # Commission comes from the fee model, which is bar-conditional; a flat
        # per-instrument taker fee would charge the free realignment orders too.
        maker_fee=Decimal(0),
        taker_fee=Decimal(0),
    )


def _bar_type(instrument_id: InstrumentId) -> BarType:
    return BarType(
        instrument_id,
        BarSpecification(1, BarAggregation.DAY, PriceType.LAST),
        AggregationSource.EXTERNAL,
    )


def _nanos(timestamp: str) -> int:
    """RFC 3339 with a trailing Z to UNIX nanoseconds. Whole seconds throughout,
    so this is exact rather than a float scaling of a large magnitude."""
    return int(datetime.fromisoformat(timestamp).timestamp()) * 1_000_000_000


def _bars(bar_type: BarType, timestamps: list[str], prices: list[float]) -> list[Bar]:
    """One bar per (bar, instrument) with open = high = low = close.

    The scenarios carry a single price per bar and instrument, serving as both
    the bar's close for marking and its fill price. A degenerate OHLC is how that
    is expressed to a bar-execution venue: there is no intrabar path for a fill
    to land anywhere else.
    """
    out = []
    for timestamp, price in zip(timestamps, prices, strict=True):
        nanos = _nanos(timestamp)
        out.append(
            Bar(
                bar_type=bar_type,
                open=Price(price, PRICE_PRECISION),
                high=Price(price, PRICE_PRECISION),
                low=Price(price, PRICE_PRECISION),
                close=Price(price, PRICE_PRECISION),
                # Far above any order these scenarios place, so bar liquidity
                # never partially fills one. A partial fill would leave the book
                # off target and produce a wrong fixture that still looked green;
                # the strategy's fill assertions catch it if it ever happens.
                volume=Quantity(SCALE, DECIMALS),
                ts_event=nanos,
                ts_init=nanos,
            )
        )
    return out


class RebalanceTagFeeModel(FeeModel):
    """`rate * |filled notional|` on rebalance-tagged orders, zero on the rest.

    This is the engine's cost rule stated in an order engine's terms: additive on
    the notional actually traded, charged only where the engine charges it. See
    the TRANSLATION section for why the every-bar realignment must be free and
    why the tagged notional equals the engine's `|dw| * nav`.
    """

    def __init__(self, rate: Decimal):
        super().__init__()
        self._rate = rate

    def get_commission(self, order, fill_qty, fill_px, instrument):
        if not order.tags or REBALANCE_TAG not in order.tags:
            return Money(0, instrument.quote_currency)
        notional = abs(fill_qty.as_decimal() * fill_px.as_decimal())
        return Money(self._rate * notional, instrument.quote_currency)


class TargetWeightStrategy(Strategy):
    """Re-issues the held target weight as a market order at every bar.

    Records the equity path as Nautilus reports it, one value per bar, marked at
    that bar's price and net of that bar's commission. Every structural
    assumption the translation rests on is asserted rather than trusted: bar
    ordering against the scenario's own timestamps, the fill price against the
    submitting bar's price, and that no order was denied or rejected.
    """

    def __init__(self, scenario: scen.Scenario):
        super().__init__()
        self._scenario = scenario
        self._instruments = [_instrument(name) for name in scenario.instruments]
        self._bar_types = [_bar_type(i.id) for i in self._instruments]
        self._timestamps = [_nanos(t) for t in scenario.timestamps()]
        self._held = scenario.held_weights()

        self.equity: dict[int, float] = {}
        self.failures: list[str] = []
        self._bar_index = 0
        self._current_ts: int | None = None
        self._arrived: dict[InstrumentId, Bar] = {}
        # client order id -> (bar index, the submitting bar's price, order size)
        self._submitted: dict[object, tuple[int, float, Quantity]] = {}
        self._filled: set[object] = set()

    # -- setup ----------------------------------------------------------------

    def instruments(self) -> list[CurrencyPair]:
        return self._instruments

    def bar_types(self) -> list[BarType]:
        return self._bar_types

    def on_start(self) -> None:
        for bar_type in self._bar_types:
            self.subscribe_bars(bar_type)

    # -- helpers --------------------------------------------------------------

    def _fail(self, detail: str) -> None:
        self.failures.append(f"{self._scenario.scenario_id}: {detail}")

    def _equity_now(self) -> float:
        balances = self.portfolio.equity(VENUE)
        if CASH not in balances:
            self._fail(f"no {CASH} equity reported at bar {self._bar_index}")
            return float("nan")
        return float(balances[CASH])

    # -- data -----------------------------------------------------------------

    def on_bar(self, bar: Bar) -> None:
        """Nautilus delivers one bar per instrument, so a multi-instrument
        scenario reaches here more than once per timestamp. Acting on the last
        arrival of a timestamp is what makes every instrument's mark current
        before the targets are sized against one shared equity value — the engine
        differences every target against a single pre-cost NAV too."""
        if bar.ts_event != self._current_ts:
            if self._arrived and len(self._arrived) != len(self._instruments):
                self._fail(
                    f"timestamp {self._current_ts} delivered "
                    f"{len(self._arrived)} of {len(self._instruments)} bars"
                )
            self._current_ts = bar.ts_event
            self._arrived = {}
        self._arrived[bar.bar_type.instrument_id] = bar
        if len(self._arrived) < len(self._instruments):
            return

        index = self._bar_index
        self._bar_index += 1
        if index >= len(self._timestamps):
            self._fail(f"bar index {index} past the scenario's {len(self._timestamps)} bars")
            return
        if bar.ts_event != self._timestamps[index]:
            # Row alignment verified against the scenario's own timestamps rather
            # than assumed from arrival order.
            self._fail(
                f"bar {index} has timestamp {bar.ts_event}, "
                f"scenario says {self._timestamps[index]}"
            )
            return

        # Provisional: correct as it stands for a bar that trades nothing, and
        # overwritten by each fill this bar produces. Fills arrive while the mark
        # is still this bar's price (probe fact (ii)), so the last write is the
        # value net of this bar's commission.
        self.equity[index] = self._equity_now()

        is_rebalance = index in self._scenario.rebalance_bars
        tags = [REBALANCE_TAG] if is_rebalance else None
        equity = self.equity[index]

        for instrument, weight in zip(self._instruments, self._held[index], strict=True):
            price = float(self._arrived[instrument.id].close)
            target = Decimal(str(weight * equity / price))
            delta = target - self.portfolio.net_position(instrument.id)
            if abs(delta) < SIZE_INCREMENT:
                # Already on target to within one quantity increment — the usual
                # case on a flat step. Submitting a zero-size order is an error
                # in Nautilus, not a no-op.
                continue
            order = self.order_factory.market(
                instrument_id=instrument.id,
                order_side=OrderSide.BUY if delta > 0 else OrderSide.SELL,
                quantity=Quantity(abs(delta), DECIMALS),
                tags=tags,
            )
            self._submitted[order.client_order_id] = (index, price, order.quantity)
            self.submit_order(order)

    # -- events ---------------------------------------------------------------

    def on_order_filled(self, event) -> None:
        submission = self._submitted.get(event.client_order_id)
        if submission is None:
            self._fail(f"fill for an order this strategy never submitted: {event}")
            return
        index, price, quantity = submission
        # The timing claim, re-checked on every fill: a bar's order fills at that
        # bar's own price. A queued-to-next-bar engine reddens here rather than
        # silently shifting the fixture by a bar.
        if float(event.last_px) != price:
            self._fail(
                f"bar {index} order filled at {float(event.last_px)}, "
                f"that bar's price is {price} — the same-bar fill timing this "
                f"oracle's alignment depends on no longer holds"
            )
            return
        if event.last_qty != quantity:
            # A partial fill leaves the book off target, so the next bar's
            # mark-to-market runs on a weight the scenario never asked for. Bar
            # volume is far above any order placed and liquidity consumption is
            # off, so this cannot currently happen — asserted rather than assumed
            # because the failure would be a silently wrong fixture, and because
            # "the order filled" is otherwise recorded on the first partial fill.
            self._fail(
                f"bar {index} order for {quantity} filled only {event.last_qty}"
            )
            return
        self._filled.add(event.client_order_id)
        self.equity[index] = self._equity_now()

    def on_event(self, event) -> None:
        if isinstance(event, (OrderDenied, OrderRejected)):
            # A denied or rejected order leaves the book off target, which would
            # produce a fixture that is wrong by roughly the size of the order
            # while still looking like a clean run.
            self._fail(f"order not accepted: {event}")

    # -- result ---------------------------------------------------------------

    def equity_path(self) -> list[float]:
        n_bars = self._scenario.n_bars
        if self._bar_index != n_bars:
            self._fail(f"saw {self._bar_index} bars, scenario has {n_bars}")
        missing = [i for i in range(n_bars) if i not in self.equity]
        if missing:
            self._fail(f"no equity recorded for bars {missing}")
        unfilled = sorted(
            index
            for coid, (index, _, _) in self._submitted.items()
            if coid not in self._filled
        )
        if unfilled:
            self._fail(f"orders submitted at bars {unfilled} never filled")
        if self.failures:
            raise AssertionError("\n".join(self.failures))
        return [self.equity[i] / SCALE for i in range(n_bars)]


def _engine(rate: Decimal) -> BacktestEngine:
    """Build the venue with every simulation parameter passed explicitly.

    Including those whose Nautilus default already happens to be the value
    wanted. An oracle that inherits a default is an oracle whose model can be
    changed by an upgrade to the library it is pinned against, silently and in a
    direction nobody chose. The values below are the ones actually in force under
    `nautilus_trader==1.230.0`; making them explicit is a no-op today and a guard
    tomorrow. Two are load-bearing enough to have been verified by mutation —
    `latency_model` and `bar_execution` — and the rest are annotated by group.
    """
    engine = BacktestEngine(
        config=BacktestEngineConfig(
            logging=LoggingConfig(bypass_logging=True),
            # The risk engine stays ON: it is what turns an order this oracle
            # could not fill into an OrderDenied the strategy's guard reports,
            # rather than a quietly missing fill. Its limits are pinned at the
            # values in force so a future tightened default cannot start denying
            # orders that this fixture set depends on. Neither can bind here —
            # at most two orders per bar, bars a simulated day apart, and no
            # per-instrument notional cap.
            risk_engine=RiskEngineConfig(
                bypass=False,
                max_order_submit_rate="100/00:00:01",
                max_order_modify_rate="100/00:00:01",
                max_notional_per_order={},
                debug=False,
            ),
        ),
    )
    engine.add_venue(
        venue=VENUE,
        # One net position per instrument, matching a weight vector: a hedging
        # OMS would open a second position on a sign flip and the flip's cost
        # would be charged against two records instead of one.
        oms_type=OmsType.NETTING,
        # A margin account with zero requirement and high leverage, so short
        # weights are expressible and no position is ever blocked for margin.
        # A cash account would refuse to short without holding the base currency.
        account_type=AccountType.MARGIN,
        base_currency=CASH,
        starting_balances=[Money(SCALE, CASH)],
        default_leverage=Decimal(100),
        leverages=None,
        margin_model=None,
        modules=None,
        fee_model=RebalanceTagFeeModel(rate),
        # No fill model, so no probabilistic slippage, partial filling or
        # rejection is introduced; degenerate OHLC means the only available fill
        # price is the bar's single price anyway.
        fill_model=None,
        # No latency model. This one is not decoration: a latency model long
        # enough to cross a bar boundary moves every fill to a later bar and
        # breaks the same-bar alignment the whole translation rests on. Verified
        # by mutation — inserting a 1.5-day latency reddens the strategy's
        # per-fill price assertion at the first rebalance bar ("bar 1 order
        # filled at 100.0, that bar's price is 125.0") and aborts there.
        latency_model=None,
        # Fills come from the bars themselves. Also load-bearing: without bar
        # execution there is no book for a market order to hit at all.
        bar_execution=True,
        # Degenerate OHLC (open = high = low = close) means there is no intrabar
        # path, so high/low ordering has nothing to reorder — pinned anyway
        # because it silently decides fill sequencing when a bar does have range.
        bar_adaptive_high_low_ordering=False,
        trade_execution=True,
        book_type=BookType.L1_MBP,
        # Order sizing here is unconditioned by book depth: the bar volume is far
        # above any order placed, and consuming liquidity or modelling queue
        # position would introduce partial fills the translation forbids.
        liquidity_consumption=False,
        queue_position=False,
        # Cash never goes negative — every scenario's long leg is below full
        # investment and short legs raise cash — so borrowing is not needed, and
        # leaving it off means a sizing bug shows up as a denial rather than as a
        # silently financed position.
        allow_cash_borrowing=False,
        # The account must update on fills; a frozen account would report the
        # starting balance forever and produce a flat equity path.
        frozen_account=False,
        # Deterministic identifiers, so a regenerated fixture family is
        # byte-identical rather than merely numerically equal.
        use_random_ids=False,
        use_position_ids=True,
        use_message_queue=True,
        use_market_order_acks=False,
        routing=False,
        # The group below cannot act on this oracle: it submits nothing but
        # GTC market orders, so stop handling, GTD expiry, contingent/OTO
        # order groups, reduce-only semantics, price protection bands and
        # settlement prices have no order to apply to. Pinned so that stays a
        # checkable claim rather than an inherited accident.
        use_reduce_only=True,
        reject_stop_orders=True,
        support_gtd_orders=True,
        support_contingent_orders=True,
        oto_trigger_mode=OtoTriggerMode.PARTIAL,
        price_protection_points=None,
        settlement_prices=None,
    )
    return engine


def run_scenario(s: scen.Scenario) -> list[float]:
    """Run one scenario and return its per-bar equity path, normalised to 1.0.

    Every scenario goes through this one path: the per-scenario differences are
    entirely in the data, so there is one translation to check by hand rather
    than three. `oracle_scenarios.py` records how each price path satisfies the
    flat-step constraints.
    """
    rate = Decimal(str(s.commission)) + Decimal(str(s.slippage))
    engine = _engine(rate)
    try:
        strategy = TargetWeightStrategy(s)
        for instrument in strategy.instruments():
            engine.add_instrument(instrument)
        timestamps = s.timestamps()
        for column, bar_type in enumerate(strategy.bar_types()):
            engine.add_data(
                _bars(bar_type, timestamps, [row[column] for row in s.prices])
            )
        engine.add_strategy(strategy)
        engine.run()
        equity = strategy.equity_path()
    finally:
        engine.dispose()

    if equity[0] != 1.0:
        # Bar 0 precedes the first rebalance in every scenario, so nothing has
        # traded yet and the normalised starting NAV must survive intact. A
        # failure here means row alignment is off by a bar.
        raise AssertionError(f"{s.scenario_id}: equity[0] is {equity[0]!r}, expected 1.0")
    return equity


def probe() -> None:
    """The execution-timing probe whose output the TRANSLATION section quotes.

    Five bars at strictly distinct prices and a single market buy submitted from
    inside `on_bar` for bar 1, so the fill price alone says which bar filled it.
    Shipped runnable rather than described, so the quoted numbers can be
    reproduced from this tree instead of taken on trust.
    """
    prices = [100.0, 110.0, 120.0, 130.0, 140.0]
    submit_at = 1
    log: list[str] = []
    instrument = _instrument("PROBE")
    bar_type = _bar_type(instrument.id)
    timestamps = [f"2024-01-{i + 1:02d}T00:00:00Z" for i in range(len(prices))]

    class ProbeStrategy(Strategy):
        def __init__(self):
            super().__init__()
            self.n = 0

        def on_start(self):
            self.subscribe_bars(bar_type)

        def on_bar(self, bar):
            index = self.n
            self.n += 1
            log.append(
                f"on_bar   i={index} close={float(bar.close):>7} "
                f"ts={timestamps[index]} "
                f"net_pos={self.portfolio.net_position(instrument.id)} "
                f"equity={self.portfolio.equity(VENUE)[CASH]}"
            )
            if index == submit_at:
                log.append(f"  -> submit MARKET BUY 0.1 during on_bar i={index}")
                self.submit_order(
                    self.order_factory.market(
                        instrument_id=instrument.id,
                        order_side=OrderSide.BUY,
                        quantity=Quantity(0.1, DECIMALS),
                    )
                )
                log.append(
                    f"  after submit_order: "
                    f"net_pos={self.portfolio.net_position(instrument.id)} "
                    f"equity={self.portfolio.equity(VENUE)[CASH]}"
                )

        def on_order_filled(self, event):
            log.append(
                f"  FILLED last_px={float(event.last_px)} qty={event.last_qty} "
                f"ts_event={event.ts_event} commission={event.commission}"
            )

    engine = _engine(Decimal(0))
    try:
        engine.add_instrument(instrument)
        engine.add_data(_bars(bar_type, timestamps, prices))
        engine.add_strategy(ProbeStrategy())
        engine.run()
    finally:
        engine.dispose()

    print("BAR PRICES:", dict(enumerate(prices)))
    print("timestamps:", timestamps)
    for line in log:
        print(line)


def main() -> None:
    if "--probe" in sys.argv[1:]:
        probe()
        return
    scen.validate()
    for s in scen.SCENARIOS:
        equity = run_scenario(s)
        scen.write_series_csv(
            f"nautilus_{s.scenario_id}_equity", s.timestamps(), equity
        )
        print(f"{s.scenario_id}: final NAV {equity[-1]:.17g}")
        print("  " + " ".join(f"{v:.12f}" for v in equity))


if __name__ == "__main__":
    main()
