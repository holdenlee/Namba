Instructions
============

Create the numbers on the stack on the right (top first).

Controls:
* LEFT/RIGHT: Move Pete
* UP: Pick up block
* DOWN: Drop block
* 'a': Activate block (you need to activate an operation before it will operate on numbers)
* SPACE: Receive another block

Log
===

7-1-15:
* Stacks receive new blocks from the bottom. Basic game works, but there is no game-over condition. No higher-order function blocks yet.
* Problem: It's very hard to use the new blocks because they come from the bottom. This means I try to shift entire stacks of blocks in a panicky way as the stacks fill up.
* I think having the stacks overflow is a distraction. Rather make the "next integers" stack be the one that keeps increasing and overflows.
* How can I keep new blocks coming in? Coming in from the bottom doesn't work. Coming in from the top covers up the work Pete has just done. How about coming in from a separate stack?
* Idea: Be able to use the basic operations anytime (have an unlimited supply of them).

