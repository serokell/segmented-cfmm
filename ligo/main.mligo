// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"
#include "helpers.mligo"
#include "transfers.mligo"
#include "math.mligo"
#include "swaps.mligo"

(* TODO: make positions into an FA2 *)

#if !DUMMY_PRAGMA1
This is an example of conditionally present code, remove it once normal pragmas are set.
#endif

let rec initialize_tick ((ticks, i, i_l,
    initial_fee_growth_outside,
    initial_seconds_outside,
    initial_seconds_per_liquidity_outside) : tick_map * tick_index * tick_index * balance_nat_x128 * nat * x128n) : tick_map =
    if Big_map.mem i ticks then
        ticks
    else if i_l.i > i.i then
        (failwith invalid_witness_err : tick_map)
    else
        let tick = get_tick ticks i_l tick_not_exist_err in
        let i_next = tick.next in
        if i_next.i > i.i then
            let tick_next = get_tick ticks i_next internal_tick_not_exist_err in
            let ticks = Big_map.update i_l (Some {tick with next = i}) ticks in
            let ticks = Big_map.update i_next (Some {tick_next with prev = i}) ticks in
            let ticks = Big_map.update i (Some {
                prev = i_l ;
                next = i_next ;
                liquidity_net = 0 ;
                n_positions = 0n ;
                fee_growth_outside = initial_fee_growth_outside;
                seconds_outside = initial_seconds_outside;
                seconds_per_liquidity_outside = initial_seconds_per_liquidity_outside;
                sqrt_price = half_bps_pow i.i}) ticks in
            ticks
        else
            initialize_tick (ticks, i, i_next, initial_fee_growth_outside, initial_seconds_outside, initial_seconds_per_liquidity_outside)

let incr_n_positions (ticks : tick_map) (i : tick_index) (incr : int) =
    let tick = get_tick ticks i internal_tick_not_exist_err in
    let n_pos = assert_nat (tick.n_positions + incr, internal_position_underflow_err) in
    if n_pos = 0n then
        (*  Garbage collect the tick.
            The largest and smallest tick are initialized with n_positions = 1 so they cannot
            be accidentally garbage collected. *)
        let prev = get_tick ticks tick.prev internal_tick_not_exist_err in
        let next = get_tick ticks tick.next internal_tick_not_exist_err in
        (* prev links to next and next to prev, skipping the deleted tick *)
        let prev = {prev with next = tick.next} in
        let next = {next with prev = tick.prev} in
        let ticks = Big_map.update i (None : tick_state option) ticks in
        let ticks = Big_map.update tick.prev (Some prev) ticks in
        let ticks = Big_map.update tick.next (Some next) ticks in
        ticks
    else
        Big_map.update i (Some {tick with n_positions = n_pos}) ticks

let collect_fees (s : storage) (key : position_index) : storage * balance_nat =
    let position = match Big_map.find_opt key s.positions with
    | None -> (failwith "position does not exist" : position_state) // TODO: [TCFMM-16] This error is a bug.
    | Some position -> position in
    let tick_lo = get_tick s.ticks key.lower_tick_index internal_tick_not_exist_err in
    let tick_hi = get_tick s.ticks key.upper_tick_index internal_tick_not_exist_err in
    let f_a = if s.cur_tick_index.i >= key.upper_tick_index.i then
        { x = {x128 = assert_nat (s.fee_growth.x.x128 - tick_hi.fee_growth_outside.x.x128, internal_311)};
          y = {x128 = assert_nat (s.fee_growth.y.x128 - tick_hi.fee_growth_outside.y.x128, internal_311)}}
    else
        tick_hi.fee_growth_outside in
    let f_b = if s.cur_tick_index.i >= key.lower_tick_index.i then
        tick_lo.fee_growth_outside
    else
        { x = {x128 = assert_nat (s.fee_growth.x.x128 - tick_lo.fee_growth_outside.x.x128, internal_312)} ;
          y = {x128 = assert_nat (s.fee_growth.y.x128 - tick_lo.fee_growth_outside.y.x128, internal_312)} } in
    let fee_growth_inside = {
        x = {x128 = assert_nat (s.fee_growth.x.x128 - f_a.x.x128 - f_b.x.x128, internal_314)} ;
        y = {x128 = assert_nat (s.fee_growth.y.x128 - f_a.y.x128 - f_b.y.x128, internal_315)} } in
    let fees = {
        x = Bitwise.shift_right ((assert_nat (fee_growth_inside.x.x128 - position.fee_growth_inside_last.x.x128, internal_316)) * position.liquidity) 128n;
        y = Bitwise.shift_right ((assert_nat (fee_growth_inside.y.x128 - position.fee_growth_inside_last.y.x128, internal_317)) * position.liquidity) 128n} in
    let position = {position with fee_growth_inside_last = fee_growth_inside} in
    let positions = Big_map.update key (Some position) s.positions in
    ({s with positions = positions}, fees)


let set_position (s : storage) (i_l : tick_index) (i_u : tick_index) (i_l_l : tick_index) (i_u_l : tick_index) (liquidity_delta : int) (to_x : address) (to_y : address) : result =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = if s.cur_tick_index.i >= i_l.i then
        initialize_tick (ticks, i_l, i_l_l, s.fee_growth, assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err), s.seconds_per_liquidity_cumulative)
    else
        initialize_tick (ticks, i_l, i_l_l, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in
    let ticks = if s.cur_tick_index.i >= i_u.i then
        initialize_tick (ticks, i_u, i_u_l, s.fee_growth, assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err), s.seconds_per_liquidity_cumulative)
    else
        initialize_tick (ticks, i_u, i_u_l, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in

    (* Form position key. *)
    let position_key = {owner=Tezos.sender ; lower_tick_index=i_l; upper_tick_index=i_u} in
    (* Grab existing position or create an empty one *)
    let (position, is_new) = match (Big_map.find_opt position_key s.positions) with
    | Some position -> (position, false)
    | None -> ({liquidity = 0n ; fee_growth_inside_last = {x = {x128 = 0n}; y = {x128 = 0n}}}, true) in
    (* Get accumulated fees for this position. *)
    let s, fees = collect_fees s position_key in
    (* Update liquidity of position. *)
    let liquidity_new = assert_nat (position.liquidity + liquidity_delta, internal_liquidity_below_zero_err) in
    let position = {position with liquidity = liquidity_new} in
    (* Reference counting the positions associated with a tick *)
    let ticks = (if liquidity_new = 0n then
        if is_new then
            ticks
        else
            let ticks = incr_n_positions ticks i_l (-1) in
            let ticks = incr_n_positions ticks i_u (-1) in
            ticks
    else
        if is_new then
            let ticks = incr_n_positions ticks i_l (1) in
            let ticks = incr_n_positions ticks i_u (1) in
            ticks
        else
            ticks) in
    (* delete the position if liquidity has fallen to 0 *)
    let position_entry : position_state option = if liquidity_new = 0n then None else Some {position with liquidity = liquidity_new} in
    let positions = Big_map.update position_key position_entry s.positions in
    (* Compute how much should be deposited / withdrawn to change liquidity by liquidity_net *)

    (* Grab cached prices for the interval *)
    let tick_u = get_tick ticks i_u internal_tick_not_exist_err in
    let tick_l = get_tick ticks i_l internal_tick_not_exist_err in
    let srp_u = tick_u.sqrt_price in
    let srp_l = tick_l.sqrt_price in

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.cur_tick_index.i < i_l.i then
        (s, {
            (* If I'm adding liquidity, x will be positive, I want to overestimate it, if x I'm taking away
                liquidity, I want to to underestimate what I'm receiving. *)
            x = ceildiv_int (liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - srp_l.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (srp_l.x80 * srp_u.x80)) ;
            y = 0})
    else if i_l.i <= s.cur_tick_index.i && s.cur_tick_index.i < i_u.i then
        (* update interval we are in, if need be ... *)
        let s = {s with cur_tick_witness = if i_l.i > s.cur_tick_witness.i then i_l else s.cur_tick_witness ; liquidity = assert_nat (s.liquidity + liquidity_delta, internal_liquidity_below_zero_err)} in
        (s, {
            x = ceildiv_int (liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - s.sqrt_price.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (s.sqrt_price.x80 * srp_u.x80)) ;
            y = shift_int (liquidity_delta * (s.sqrt_price.x80 - srp_l.x80)) (-80)
            })
    else (* cur_tick_index >= i_u *)
        (s, {x = 0 ; y = shift_int (liquidity_delta * (srp_u.x80 - srp_l.x80)) (-80) }) in

    (* Collect fees to increase withdrawal or reduce required deposit. *)
    let delta = {x = delta.x - fees.x ; y = delta.y - fees.y} in

    let op_x = if delta.x > 0 then
        x_transfer Tezos.sender Tezos.self_address (abs delta.x)
    else
        x_transfer Tezos.self_address to_x (abs delta.x) in

    let op_y = if delta.y > 0 then
        y_transfer Tezos.sender Tezos.self_address (abs delta.y)
    else
        y_transfer Tezos.self_address to_y (abs delta.y) in

    ([op_x ; op_y], {s with positions = positions; ticks = ticks})

// Allocate slots in a buffer in [start, to_stop) indices range.
// These slots will be filled with dummy values.
let rec allocate_buffer_slots (ic_sums, start, to_stop : time_weighted_ic_sums_buffer * nat * nat) : time_weighted_ic_sums_buffer =
    if start >= to_stop
    then ic_sums
    else
      let new_ic_sums = {ic_sums with buffer = Big_map.add start {ic_sum = 0; time = (0 : timestamp)} ic_sums.buffer}
      in allocate_buffer_slots(new_ic_sums, start + 1n, to_stop)

// Increase number of stored ic_sum accumulators.
let increase_observation_count (s, new_val : storage * nat) : result =
    let ic_sums = s.time_weighted_ic_sums in
    if ic_sums.reserved_length >= new_val
    then (failwith already_observe_more_err : result)
    else
      let new_ic_sums = allocate_buffer_slots(s.time_weighted_ic_sums, ic_sums.last + 1n, ic_sums.first + ic_sums.reserved_length)
      in (([] : operation list), {s with time_weighted_ic_sums = new_ic_sums})

let get_time_weighted_ic_value_unsafe (ic_sums : time_weighted_ic_sums_buffer) (i : nat) : timed_ic_sum =
    match Big_map.find_opt i ic_sums.buffer with
        | None -> (failwith internal_bad_access_to_observation_buffer : timed_ic_sum)
        | Some v -> v

let get_last_time_weighted_ic_value (ic_sums : time_weighted_ic_sums_buffer) : timed_ic_sum =
    get_time_weighted_ic_value_unsafe ic_sums ic_sums.last

let rec get_time_weighted_ic_value_at_iter (ic_sums, t, l_i, r_i, l_v, r_v : time_weighted_ic_sums_buffer * timestamp * nat * nat * timed_ic_sum * timed_ic_sum) : int =
    // Binary search, invariant: buffer[l].time <= t && t < buffer[r].time
    if l_i + 1n < r_i
    then
      let m_i = ceildiv (l_i + r_i) 2n in
      let m_v = get_time_weighted_ic_value_unsafe ic_sums m_i in
      if m_v.time > t
      then get_time_weighted_ic_value_at_iter (ic_sums, t, l_i, m_i, l_v, m_v)
      else get_time_weighted_ic_value_at_iter (ic_sums, t, m_i, r_i, m_v, r_v)
    else
      // Between two adjacent registered values ic_sum grows linearly (since i_c stays constant),
      // interpolating to get the value in-between.
      ceildiv_int ((t - l_v.time) * r_v.ic_sum + (r_v.time - t) * l_v.ic_sum) (r_v.time - l_v.time)

let get_time_weighted_ic_value_at (ic_sums, t : time_weighted_ic_sums_buffer * timestamp) : int =
    let l_i = ic_sums.first in
    let r_i = ic_sums.last in
    let l_v = get_time_weighted_ic_value_unsafe ic_sums l_i in
    let r_v = get_time_weighted_ic_value_unsafe ic_sums r_i in

    if t < l_v.time then (failwith observe_outdated_timestamp_err : int) else
    if t > r_v.time then (failwith observe_future_timestamp_err : int) else
    get_time_weighted_ic_value_at_iter (ic_sums, t, l_i, r_i, l_v, r_v)

type views =
    | IC_sum of int

let get_time_weighted_sum (s : storage) (m : get_time_weighted_sum_mode) (c : views contract) : result =
    let ic_sum = match m with
        | Get_latest_time_weighted_sum ->
            let value = get_last_time_weighted_ic_value s.time_weighted_ic_sums
            in value.ic_sum
        | Get_time_weighted_sum_at t ->
            get_time_weighted_ic_value_at (s.time_weighted_ic_sums, t)
        in
    ([Tezos.transaction (IC_sum ic_sum) 0mutez c], s)

type parameter =
| X_to_Y of x_to_y_param
| Y_to_X of y_to_x_param
| Set_position of set_position_param (* TODO add deadline, maximum tokens contributed, and maximum liquidity present *)
| X_to_X_prime of address (* equivalent to token_to_token *)
| Increase_observation_count of nat
| Get_time_weighted_sum of (get_time_weighted_sum_mode * views contract)

let update_time_weighted_sum (s : storage) : storage =
    let ic_sums = s.time_weighted_ic_sums in

    let last_value = get_last_time_weighted_ic_value ic_sums in
    (* Update not more often than once per block *)
    if last_value.time = Tezos.now then s
    else
        let new_sum = last_value.ic_sum + (Tezos.now - last_value.time) * s.cur_tick_index.i in
        let new_last = ic_sums.last + 1n in
        let new_value = { ic_sum = new_sum; time = Tezos.now } in
        let new_first =
            if ic_sums.last - ic_sums.first < ic_sums.reserved_length - 1
            // preserve the oldest element if reserves allow this
            then ic_sums.first else ic_sums.first + 1n in

        let is_sums = {
            buffer = Big_map.add new_last new_value ic_sums.buffer ;
            last = new_last ;
            first = new_first ;
            reserved_length = ic_sums.reserved_length ;
        }
        in {s with time_weighted_ic_sums = is_sums}

let main ((p, s) : parameter * storage) : result =
(* start by updating the time weighted price oracle *)
let s = update_time_weighted_sum s in
(* dispatch call to the proper entrypoint *)
 match p with
| X_to_Y p -> x_to_y s p
| Y_to_X p -> y_to_x s p
| Set_position p -> set_position s p.lower_tick_index p.upper_tick_index p.lower_tick_witness p.upper_tick_witness p.liquidity_delta p.to_x p.to_y
| Get_time_weighted_sum p -> get_time_weighted_sum s p.0 p.1
| X_to_X_prime _ -> (failwith "not implemented" : result) (*TODO implement iff Y is FA12 *)
| Increase_observation_count n -> increase_observation_count(s, n)
