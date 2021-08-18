// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"

// ----------------------------------------------------------------------------
// Pow function, implemented from haskell (^)
// ----------------------------------------------------------------------------

let quot (a, b: int * int): int =
  match ediv a b with
      Some (quotient, _rem) -> quotient
    | None -> (failwith("`b` can't be 0."): int)

let rec pow_g (x, y, z: int * int * int): int =
      if (y mod 2 = 0n) then pow_g (x * x, quot (y, 2), z)
      else if (y = 1) then x * z
      else pow_g (x * x, quot (y, 2), x * z)

let rec pow_f (x, y: int * int): int =
      if (y mod 2 = 0n) then pow_f (x * x, y / 2)
      else if (y = 1) then x
      else pow_g ( x * x, quot (y, 2), x)

let pow (x, y: int * int) : int =
  if (y < 0) then (failwith("Negative exponent"): int)
  else if (y = 0) then 1
  else pow_f (x, y)

// ----------------------------------------------------------------------------


let default_storage : storage =
  let infinity : int = pow (10, 100) in

  // Note: Taken form ligo/tests/tests.mligo
  let infinity_tick =
    { prev = { i = -infinity }
    ; next = { i = infinity }
    ; liquidity_net = 0
    ; seconds_outside = 0n
    ; seconds_per_liquidity_outside = 0n
    ; n_positions = 0n
    ; fee_growth_outside = {x = 0n; y = 0n}
    ; sqrt_price = 0n
    } in

  let ticks = Big_map.literal [
      ({ i = -infinity }, infinity_tick);
      ({ i = infinity }, infinity_tick)
  ] in

  // Note: Taken from `python/scfmm/__init__.py`
  { liquidity = 0n
  ; sqrt_price = 0n
  ; i_c = 0
  ; lo  = { i = -infinity }
  ; fee_growth = { x = 0n ; y = 0n }
  ; balance = { x = 0n ; y = 0n }
  ; ticks = ticks
  ; positions = (Big_map.empty : position_map)
  ; time_weighted_ic_sum = 0
  ; last_ic_sum_update = epoch_time
  ; seconds_per_liquidity_cumulative = 0n
  ; metadata = (Big_map.empty : metadata_map)
  }
