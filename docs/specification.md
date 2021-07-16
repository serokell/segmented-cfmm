

# Segmented CFMM

This contract implements a Constant Function Market Maker (CFMM) in Michelson inspired by [Uniswap v3][uniswap-v3].

# Background

In a traditional exchange, users will put in buy orders and sell orders.
The exchange will find a match between a buy order and a sell order and conduct the transaction.
If there aren't enough users, it might be difficult or even impossible to find a match and execute trades.
When that happens, the asset being traded is said to have low liquidity, i.e., it's hard to liquidate the asset.

A Market Maker (MM) is an entity that brings liquidity to a market, by constantly buying and selling assets from/to users.

In a Decentralized Exchange (DEX), users don't buy from/sell to each other directly.
Instead, they only buy from/sell to an Automated Market Maker (AMM).
The AMM manages one liquidity pool (or more), a pot made of funds deposited by those who wish to contribute - the Liquidity Providers (LPs).

For example, say we have two tokens, `x` and `y`, and a liquidity pool that maintains reserves of both tokens.
LPs can deposit their own tokens in this pool to increase its reserves.
Users can then sell their `x` tokens and get `y` tokens in return (swapping `x` for `y`),
or buy `x` tokens in exchange for their `y` tokens (swapping `y` for `x`).

## Overview

This contract is a Constant Function Market Maker (CFMM), a class of AMMs,
and is capable of managing a pool of any two FA1.2/FA2 tokens.

This CFMM uses a _function_ to ensure that, after every swap, the product of the
reserves `x` and `y` is always equal to some _constant_ `k`, i.e., `x * y = k`.

For example, say `k = 15600`, and our pool contains 1560 `x` tokens and 10 `y` tokens,
satisfying the `x * y = k` requirement.

If a user sells 3 `y` tokens, the pool's `y` reserves would increase to 13.
Since the product of the reserves must remain equal to `k` at all times, it follows that the `x`
reserves must now be `x = k / y = 15600 / 13 = 1200`.
Thus, the user would get `1560 - 1200 = 360` `x` tokens in exchange for their 3 `y` tokens.

A constant product ensures that the price of an asset goes up or down according to demand.
The more a token's reserves are depleted, the more expensive it'll be for a user to buy more of those tokens.
If a user were to sell 3 more `y` tokens, they would now receive _only_ `x - (k / y) = 1200 - (15600 / 16) = 225` `x` tokens.

## Positions

TODO

## Swaps

TODO

## Fees

At the beginning of every swap, the contract subtracts a _swap fee_ from the tokens sent in,
_before_ all other calculations. Whatever remains, is deposited into the liquidity pool and
exchanged for the other asset.

For example, if the swap fee is 0.3% and the user sends in 10000 `y` tokens, then:
* 30 `y` tokens will be collected as swap fees and set aside,
* 9970 `y` tokens will be deposited into the liquidity pool and exchanged for `x` tokens
  according to the _constant function_ calculations.

Furthermore, a _governance fee_ is subtracted from the swap fee.
The governance fee is either expressed as a `1 / N` fraction or 0,
and can be claimed by the contract's governance.
Reusing the example above, if the governance fee were `1/5`, then `30/5 = 6` tokens would
be subtracted from the swap fee.

When an LP updates their position, they are rewarded with part of the swap fees paid by users,
in both `x` and `y` tokens.
This reward is proportional to:
* the position's size,
* and the amount of swap fees collected while the position remained _active_ since the last
  time the position was updated/created.

Both the swap fee and the governance fee percentages are initialized when the contract is
originated and immutable thereafter.



 [uniswap-v3]: https://uniswap.org/whitepaper-v3.pdf
