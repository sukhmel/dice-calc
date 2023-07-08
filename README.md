[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

# dice-calc

The plan is to have a calculator allowing for 
 * calculating dice throws (like `3d6` or `4d{1,3,5}`),
 * composing them with some operations (like `+`, `-`, `*`, `/` etc),
 * filtering outcomes (like `highest N`, `lowest N`, `duplicates` etc), 
 * and possibly some other stuff like floating point dice sides. 
This is supposed to calculate everything by computing all possible outcomes and the amount of events that lead to this outcome with bigints, so memory consumption is going to be quite high, maybe there will be a way to optimize it somehow.

## Terminology
 * a **throw** is a vector of dice sides, each representing a different die, e.g. [1, 2, 1, 1]. Since order is not preserved (for now?) those will always be ordered to simplify joining same throws
 * an **outcome** is a throw with the count of how many outcomes are calculated to lead to this event
 * a **configuration** is a full set of events, that allows to calculate probabilities by dividing event outcome count by total outcome count

## Current plan
 * `N` for N sides (short for `{1..N}`) — this may be a problem, likely this will need to be implemented in AST processing
 * `M..N` for slice of sides (?) (short for `{M..N}`)
 * `{K..L;M;N}` for set of sides
 * `V x N` for repeated side value, i.e. `1 x 3` or `{1 x 3}` is the same as `{1; 1; 1}`
 * infix `d` for throw of specified dice
 * infix `+`, `-`, `*`, `/` for mathematical operations
 * negation with `-` (?)
 * `.filter(...)` for filtering
 * filters:
   * Basic:
     * `> N` for results bigger than N
     * `< N` for results smaller than N
     * `= N` for results equal to N
     * `in {...}` for results equalling to any value from a set
     * `not`, `and`, `or` for combining filters
   * Complex
     * `duplicated [basic filter]` for results that are duplicated once or times fitting basic filter
     * `unique [basic filter]` for unique results, occurring no more than once or times fitting basic filter
     * `times basic filter` for results that occur exactly the amount of times satisfying basic filter
 * `.max(N)` for all dice with N highest ranks, e.g. [1,1,2,3,3].max(1) becomes [3,3]
 * `.min(N)` same but N lowest ranks, e.g. [1,1,2,3,3].min(1) becomes [1,1]
 * `.high(N)` for N highest results, e.g. [1,1,2,3,3].high(1) becomes [3]
 * `.low(N)` for N lowest results, e.g. [1,1,2,3,3].low(1) becomes [1]
 * `.deduplicate(N)` for removing excessive duplicates, maybe that should be a filter?
 * `throw(...).until(...).limit(...)` for a limited series of throws that are performed until some condition is met
 * reducing operations:
   * `.count` for counting the amount of dice in a resultant events
   * `.sum` for summing all dice values
   * `.mul` for multiplying all dice values (?)
 * `.retain(..)` for reducing the amount of dice in events by limiting the maximum
 * `.remove(..)` for reducing the amount of dice by removing a specified amount
 * `.union(..)` for uniting the throws in two configurations
 * `.intersection(..)` for intersecting the throws in two configurations
 * `.difference(..)` for subtracting one configuration's throws from another configuration's
 * `.sample(N)` for getting specified amount of throws from given configuration

 Also features that I'd like to try implementing:
 * Tab completion for calculator expression input
 * colorization of inputted keywords with cursor manipulation and rewriting the input
 * string sides, that'd be cool to have, although it would limit operations possible on configurations involving string dice

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
