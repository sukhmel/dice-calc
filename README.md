[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

# dice-calc

The plan is to have a calculator allowing for 
 * calculating dice throws (like `3d6` or `4d{1,3,5}`),
 * composing them with some operations (like `+`, `-`, `*`, `/` etc),
 * filtering outcomes (like `highest N`, `lowest N`, `duplicates` etc), 
 * and possibly some other stuff like floating point dice sides. 
This is supposed to calculate everything by computing all possible outcomes and the amount of events that lead to this outcome with bigints, so memory consumption is going to be quite high, maybe there will be a way to optimize it somehow.

## Terminology
 * a **throw** is a specific event with given amount of dice, each having a specific side, e.g. [1, 2, 1, 1]. Since order is not preserved (for now?) those will always be ordered to simplify joining same throws
 * an **event** is a throw with the count of how many outcomes are calculated to lead to this event
 * a **configuration** is a full set of events, that allows to calculate probabilities by dividing event outcome count by total outcome count

## Current plan
 * `N` for N sides (short for `{1..N}`)
 * `M..N` for slice of sides (?) (short for `{M..N}`)
 * `{K..L,M,N,...}` for set of sides
 * infix `d` for throw of specified dice
 * infix `+`, '-', `*` for mathematical operations
 * `.filter(...)` for filtering
 * filters:
   * `high N` for N highest results
   * `low N` for N lowest results
   * `> N` for results bigger than N
   * `< N` for results smaller than N
   * `in {...}` for results equalling to any value from a set
   * `except {...}` for results not from given set (maybe it's worth making a negation for a filter instead)
   * `duplicate N` for results that are duplicated at least N times (maybe there should be syntax for specifying several values and/or filters like `<` and `>`)
   * `unique` for unique results
 * `throw(...).until(...).limit(...)` for a limited series of throws that are performed until some condition is met
 * `.count` for counting the amount of dice in a resultant events
 * `.retain(..)` for reducing the amount of dice in events by limiting the maximum
 * `.remove(..)` for reducing the amount of dice by removing a specified amount
 * `.sample(..)` for getting specified amount of throws from given configuration

## Not planned (for now)
 * preserving dice order and functions that will depend on this
   * `first N`
   * `last N`
   * `permutation N`
 * floating point sides, as rational sides are possible by dividing a throw by a specific value

## Some thoughts
 * `N d {S}` is equivalent to `S * 1 d {S}`
 * `<events> <math operation> <number>` seems to have `<number>` equivalent to `<number> d 1`
   * it's quite possible that a `number` is in any context equivalent to the `number d 1`, so wherever a number is expected, a set of events should also fit. This allows for greater flexibility but allows to increase event count dramatically in a very few operations.
