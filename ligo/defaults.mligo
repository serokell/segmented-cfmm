// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"

let default_storage : storage =
  let infinity : int = 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 in

  let infinity_tick =
    { prev = { i = -infinity }
    ; next = { i = infinity }
    ; liquidity_net = 0
    ; n_positions = 0n
    ; seconds_outside = 0n
    ; fee_growth_outside = { x = { x128 = 0n }; y = { x128 = 0n } }
    ; seconds_per_liquidity_outside = { x128 = 0n }
    ; sqrt_price = { x80 = 0n }
    } in

  let ticks = Big_map.literal [
      ({ i = -infinity }, infinity_tick);
      ({ i = infinity }, infinity_tick)
  ] in

  // Note: Taken from `python/scfmm/__init__.py`
  { liquidity = 0n
  ; sqrt_price = { x80 = 0n }
  ; cur_tick_index = { i = 0 }
  ; cur_tick_witness  = { i = -infinity }
  ; fee_growth = { x = { x128 = 0n }; y = { x128 = 0n } }
  ; balance = { x = 0n ; y = 0n }
  ; ticks = ticks
  ; positions = (Big_map.empty : position_map)
  ; time_weighted_ic_sum = 0
  ; last_ic_sum_update = epoch_time
  ; seconds_per_liquidity_cumulative = { x128 = 0n }
  ; metadata = (Big_map.empty : metadata_map)
  }
