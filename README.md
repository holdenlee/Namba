Instructions
============

Create the numbers on the stack on the right (top first).

Controls:
* LEFT/RIGHT: Move Pete
* UP: Pick up block
* DOWN: Drop block
* 'a': Activate block (you need to activate an operation before it will operate on numbers)
* SPACE: Drop block from input stack (automatically activates)
* 1,2,3,4: Pick up a +, -, \*, \/.

Log
===

7-2-15:
* Changed the game so that incoming blocks can be placed at Pete's leisure using SPACE, and the basic operation blocks +, -, \*, / can be accessed using 1, 2, 3, 4. New output blocks come in every n seconds where n starts out being 10. Game ends when output blocks overflow.
* Added randomness (uses seed based on time).
* TODO: Game over screen, make it easier to activate blocks (right now it's very clunky). E.g., use Shift-# or Shift-SPACE.

7-1-15:
* Stacks receive new blocks from the bottom. Basic game works, but there is no game-over condition. No higher-order function blocks yet.
* Problem: It's very hard to use the new blocks because they come from the bottom. This means I try to shift entire stacks of blocks in a panicky way as the stacks fill up.
* I think having the stacks overflow is a distraction. Rather make the "next integers" stack be the one that keeps increasing and overflows.
* How can I keep new blocks coming in? Coming in from the bottom doesn't work. Coming in from the top covers up the work Pete has just done. How about coming in from a separate stack?
* Idea: Be able to use the basic operations anytime (have an unlimited supply of them).

