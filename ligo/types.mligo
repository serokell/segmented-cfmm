// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if TYPES_MLIGO
#else
#define TYPES_MLIGO

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type balance_nat = {x : nat ; y : nat}
type balance_int = {x : int ; y : int}

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
    *)
    liquidity_net : int ;

    (* Numbers of positions with an edge at the given tick.
        Used for garbage collection.
    *)
    n_positions : nat ;

    (* Overall number of seconds s_o that the current tick index i_c
        has spent below (above) this tick by the moment of the last cross with it,
        assuming that now i_c is also below (above) this tick.

        Here we assume that, during all the time since Unix epoch start till
        the moment of tick initialization, i_c was set to ∞.

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
    fee_growth_outside : balance_nat ;

    (* Seconds-weighted 1/L value accumulator s_lo, it accounts only for
        "outside" periods. For intuition for "outside" word, see `seconds_outside`.

        This helps us to implement liquidity oracle.
    *)
    seconds_per_liquidity_outside : nat ;

    (* sqrt(P) = sqrt(X/Y) associated with this tick. *)
    sqrt_price : nat
}

type tick_map = (tick_index, tick_state) big_map

(* Position types, representing LP positions. *)
type position_index = {owner : address ; lo : tick_index ; hi : tick_index}
type position_state = {
    (* Position's liquidity. *)
    liquidity : nat ;
    (* Total fees earned by the position at the moment of last fees collection for this position.
        This helps to evaluate the next portion of fees to collect.
    *)
    fee_growth_inside_last : balance_nat ;
    (* TODO: fill description once this field is used. *)
    seconds_per_liquidity_inside : nat ;
    }

type position_map = (position_index, position_state) big_map

// TZIP-16 metadata map
type metadata_map = (string, bytes) big_map

type storage = {
    (* Virtual liquidity, the value L for which the curve locally looks like x * y = L^2. *)
    liquidity : nat ;
    (* Square root of the virtual price, the value P for which P = x / y. *)
    sqrt_price : nat ;
    (* Index of the highest tick corresponding to a price less than or equal to sqrt_price^2,
        does not necessarily corresponds to a boundary.
    *)
    i_c : int ;
    (* The highest initialized tick lower than or equal to i_c. *)
    lo : tick_index ;
    (* The total amount of fees that have been earned per unit of virtual liquidity (L),
        over the entire history of the contract.
    *)
    fee_growth : balance_nat ;
    (* Tokens' amounts. *)
    balance : balance_nat ;
    (* States of all initialized ticks. *)
    ticks : tick_map ;
    (* States of positions (with non-zero liquidity). *)
    positions : position_map ;
    (* Cumulative time-weighted sum of the i_c.
        This is needed to evaluate time weighted geometric mean price
        over various time periods.
    *)
    time_weighted_ic_sum : int ;
    (* Last time `time_weighted_ic_sum` was updated. *)
    last_ic_sum_update : timestamp ;
    (* Cumulative time-weighted sum of 1/L. *)
    seconds_per_liquidity_cumulative : nat ;

    (* TZIP-16 metadata. *)
    metadata : metadata_map ;
}

(* Entrypoints types *)
type set_position_param = {
    (* Lower tick. *)
    i_l : tick_index ;
    (* Upper tick. *)
    i_u : tick_index ;
    (* Lower tick's witness calculated offchain.

        A witness of tick T is some (preferably highest) _initialized_ tick
        with index lower than or equal to T. Finding a witness in our linked
        list with ticks is too expensive to be done on-chain.
    *)
    i_l_l : tick_index ;
    (* Upper tick's witness calculated offchain. *)
    i_u_l : tick_index ;
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

type result = (operation list) * storage

#endif
