// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "../types.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let get_position (position_id, store : position_id * storage) : position_state =
  match (Big_map.find_opt position_id store.positions) with
    | Some position -> position
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> position_state)]
          ("FA2_TOKEN_UNDEFINED", ()) : position_state)

[@inline]
let check_sender (from_ , token_id, store : address * token_id * storage): address =
  if (Tezos.sender = from_) then from_
  else
    let key: operator_param = { owner = from_; operator = Tezos.sender; token_id = token_id } in
    if Big_map.mem key store.operators then
      from_
    else
     ([%Michelson ({| { FAILWITH } |} : string * unit -> address)]
        ("FA2_NOT_OPERATOR", ()) : address)

(* A function that combines the usual FA2's `debit_from` and `credit_to`. *)
[@inline]
let change_position_owner (from_, tx, store: address * transfer_destination * storage): storage =
  if tx.amount = 0n then
    store // We allow 0 transfer
  else
    let position = get_position(tx.token_id, store) in

    // Ensure `from_` is the owner of the position.
    let owned_amount = if position.owner = from_ then 1n else 0n in
    let _ : unit =
        if (owned_amount = 1n && tx.amount = 1n) then unit
        else
          ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> unit)]
            ("FA2_INSUFFICIENT_BALANCE", (tx.amount, owned_amount))) in

    let new_position = { position with owner = tx.to_ } in
    let positions = Big_map.add tx.token_id new_position store.positions in
    { store with positions = positions }


// -----------------------------------------------------------------
// Transfer
// -----------------------------------------------------------------

let transfer_item (store, ti : storage * transfer_item): storage =
  let transfer_one (store, tx : storage * transfer_destination): storage =
    let valid_from = check_sender (ti.from_, tx.token_id, store) in
    change_position_owner (valid_from, tx, store)
  in List.fold transfer_one ti.txs store

let transfer (params, store : transfer_params * storage): result =
  let store = List.fold transfer_item params store in
  (([] : operation list), store)


// -----------------------------------------------------------------
// Balance of
// -----------------------------------------------------------------

let balance_of (params, store : balance_request_params * storage): result =
  let check_one (req : balance_request_item): balance_response_item =
    let pos_index = get_position(req.token_id, store) in
    let bal = if req.owner = pos_index.owner then 1n else 0n in
    { request = req; balance = bal } in
  let result = List.map check_one params.requests in
  let transfer_operation = Tezos.transaction result 0mutez params.callback in
  (([transfer_operation] : operation list), store)

// -----------------------------------------------------------------
// Update operators entrypoint
// -----------------------------------------------------------------

let update_one (store, param: storage * update_operator): storage =
  let (operator_update, operator_param) =
    match param with
    | Add_operator p -> (Some unit, p)
    | Remove_operator p -> ((None : unit option), p) in
  if (Tezos.sender = operator_param.owner) then
    let updated_operators = Big_map.update operator_param operator_update store.operators
    in  { store with
          operators = updated_operators
        }
  else
     ([%Michelson ({| { FAILWITH } |} : string * unit -> storage)]
        ("FA2_NOT_OWNER", ()) : storage)

let update_operators (params, store : update_operators_param * storage):result =
  let store = List.fold update_one params store in
  (([] : operation list), store)


let call_fa2 (store : storage) (param : fa2_parameter) : result =
  match param with
  | Transfer (p) -> transfer (p, store)
  | Balance_of (p) -> balance_of(p, store)
  | Update_operators (p) -> update_operators(p, store)
