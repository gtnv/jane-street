# My Jane-Street Puzzle Solutions

These are my solutions to the monthly Jane-Street puzzles. I attempt to increase the difficulty by solving them only in Haskell.

## August 2026 — Andy's Afternoon Amble

Lift the truncated tetrahedron's four hexagons onto the floor, reducing Andy's remembered turns to an exact exit problem on a six-cycle.
The solver finds the discovery probability **11/20**; run it with `runghc andys_afternoon_amble.hs`.

## July 2026 — ‘Pent-Up’ Frustration 3 / Knight Moves 7

Reconstruct a 3-D knight's path across a towered 8x8 board from intermittent score clues.
The solver derives `K = 7`, a 54-move path, and answer **33609**; run it with `runghc knight_moves.hs`.
