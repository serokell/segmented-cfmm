// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if TYPES_MLIGO
#else
#define TYPES_MLIGO

(* Keeps a positive value with -2^80 precision. *)
type x80n = { x80 : nat }

(* Keeps a positive value with -2^128 precision. *)
type x128n = { x128 : nat }

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type balance_nat = {x : nat ; y : nat}
type balance_nat_x128 = {x : x128n ; y : x128n}

(* Information stored for every initialized tick. *)
type tick_state = {
    (* Index of the previous initialized tick.
        Here we diverge from the article, and effectively store a doubly-linked
        list of initialized ticks for speed-up
        (while the article proposes storing a bitmap for this purpose).
    *)
    prev : tick_index ;

    (* Index of the next initialized tick. *)
    next : tick_index ;

    (* Total amount of liquidity to add to the contract's global liquidity when
        this tick is crossed going up.
        (i.e. when the current tick index `i_c` becomes greater than this tick),
        or subtracted when the tick is crossed going down.
    *)
    liquidity_net : int ;

    (* Numbers of positions with an edge at the given tick.
        Used for garbage collection.
    *)
    n_positions : nat ;

    (* When the current tick index `i_c` is below this tick, this field tracks
        the overall number of seconds `i_c` spent above or at this tick.
        When `i_c` is above or equal to this tick, it tracks the number of
        seconds `i_c` spent below this tick.

        This field is updated every time `i_c` crosses this tick.

        Here we assume that, during all the time since Unix epoch start till
        the moment of tick initialization, i_c was below this tick
        (see equation 6.25 of the uniswap v3 whitepaper).

        As example, let's say the tick was initialized at 1628440000 timestamp;
        then `seconds_outside` will be initialized with the same timestamp.
        If i_c crossed this tick 5 seconds later, this `seconds_outside` will
        be set respectively to 5.
        If i_c crossed this tick back 3 seconds later, we will get
        `1628440000 + 3 = 1628440003`
        (effectively this will be computed as `cur_time - last seconds_outside =
        1628440008 - 5 = 1628440003`).

        This field helps to evaluate, for instance, how many seconds i_c
        has spent in an any given ticks range.
    *)
    seconds_outside : nat ;

    (* Overall number of fees f_o that were accumulated during the period
        when the current tick index i_c was below (or above) this tick.

        For intuition for "outside" word, see `seconds_outside`.
       *)
    fee_growth_outside : balance_nat_x128 ;

    (* Seconds-weighted 1/L value accumulator s_lo, it accounts only for
        "outside" periods. For intuition for "outside" word, see `seconds_outside`.

        This helps us to implement liquidity oracle.
    *)
    seconds_per_liquidity_outside : x128n ;

    (* sqrt(P) = sqrt(X/Y) associated with this tick. *)
    sqrt_price : x80n
}

type tick_map = (tick_index, tick_state) big_map

(* Position types, representing LP positions. *)
type position_index = {
    owner : address ;
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index
}

type position_state = {
    (* Position's liquidity. *)
    liquidity : nat ;
    (* Total fees earned by the position at the moment of last fees collection for this position.
        This helps to evaluate the next portion of fees to collect.
    *)
    fee_growth_inside_last : balance_nat_x128 ;
}

type position_map = (position_index, position_state) big_map

// TZIP-16 metadata map
type metadata_map = (string, bytes) big_map

type timed_ic_sum = { ic_sum : int; time : timestamp }

// Extendable ring buffer with time-weighted geometric mean price.
type time_weighted_ic_sums_buffer = {
    // For each index this stores:
    // 1. Sum of tick indices for every second in the history of the contract
    //    till specific moment of time.
    // 2. Timestamp when this sum was registered.
    //    This allows for bin search by timestamp and for interpolation.
    //
    // Invariants:
    // a. The set of indices that have an associated element with them is continuous;
    // b. Timestamps in values grow strictly monotonically (as well as `ic_sum`s ofc);
    buffer : (nat, timed_ic_sum) big_map ;

    // Index of the oldest stored value.
    first : nat ;

    // Index of the most recently stored value.
    last : nat ;

    // Number of actually allocated slots.
    //
    // This value is normally equal to `last - first`.
    // However, in case recently there was a request to extend the set of
    // stored values, this var will keep the demanded number of stored values,
    // while values in the map past `last` will be initialized with garbage.
    //
    // We need to have initialized slots with trash because when the size of
    // the map increases, someone has to pay for the storage diff.
    // And we want it to be payed by the one who requested the extension.
    reserved_length : nat ;
}

let init_time_weighted_ic_sums_buffer (now : timestamp) : time_weighted_ic_sums_buffer =
    { buffer = Big_map.literal [(0n, { ic_sum = 0; time = now })]
    ; first = 0n
    ; last = 0n
    ; reserved_length = 1n
    }

type storage = {
    (* Virtual liquidity, the value L for which the curve locally looks like x * y = L^2. *)
    liquidity : nat ;

    (* Square root of the virtual price, the value P for which P = x / y. *)
    sqrt_price : x80n ;

    (* Index of the highest tick corresponding to a price less than or equal to sqrt_price^2,
        does not necessarily corresponds to a boundary.
        Article's notation: i_c, tick.
    *)
    cur_tick_index : tick_index ;

    (* The highest initialized tick lower than or equal to i_c. *)
    cur_tick_witness : tick_index ;

    (* The total amount of fees that have been earned per unit of virtual liquidity (L),
        over the entire history of the contract.
    *)
    fee_growth : balance_nat_x128 ;

    (* Tokens' amounts. *)
    balance : balance_nat ;

    (* States of all initialized ticks. *)
    ticks : tick_map ;

    (* States of positions (with non-zero liquidity). *)
    positions : position_map ;
    (* Cumulative time-weighted sums of i_c. Only some recent values are stored.
        This is needed to evaluate time weighted geometric mean price
        over various time periods.
    *)
    time_weighted_ic_sums : time_weighted_ic_sums_buffer ;

    (* Cumulative time-weighted sum of 1/L. *)
    seconds_per_liquidity_cumulative : x128n ;

    (* TZIP-16 metadata. *)
    metadata : metadata_map ;
}

(* Entrypoints types *)
type set_position_param = {
    (* Lower tick. *)
    lower_tick_index : tick_index ;
    (* Upper tick. *)
    upper_tick_index : tick_index ;
    (* Lower tick's witness calculated offchain.

        A witness of tick T is some (preferably highest) _initialized_ tick
        with index lower than or equal to T. Finding a witness in our linked
        list with ticks is too expensive to be done on-chain.
    *)
    lower_tick_witness : tick_index ;
    (* Upper tick's witness calculated offchain. *)
    upper_tick_witness : tick_index ;
    (* How to change the liquidity of the existing position.
        The contract behaves like if any possible position was already there,
        but most of them had 0 liquidity.

        If adding a delta (that can be negative) would result in a negative liquidity value,
        the call will abort.
    *)
    liquidity_delta : int ;
    (* Where to send the freed X tokens, if any. *)
    to_x : address ;
    (* Where to send the freed Y tokens, if any. *)
    to_y : address ;
}

type x_to_y_param = {
    (* X tokens to sell. *)
    dx : nat ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The transaction won't be executed if buying less than the given amount of Y tokens. *)
    min_dy : nat ;
    (* Recipient of dy. *)
    to_dy : address ;
}

type x_to_y_rec_param = {s : storage ; dx : nat ; dy : nat}

type y_to_x_param = {
    (* Y tokens to sell. *)
    dy : nat ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The transaction won't be executed if buying less than the given amount of X tokens. *)
    min_dx : nat ;
    (* Recipient of dx. *)
    to_dx : address ;
}

type y_to_x_rec_param = x_to_y_rec_param

type get_time_weighted_sum_mode
    = Get_latest_time_weighted_sum
    | Get_time_weighted_sum_at of timestamp

type result = (operation list) * storage

#endif
