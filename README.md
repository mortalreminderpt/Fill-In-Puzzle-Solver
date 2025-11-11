# Fill-In Puzzle Solver

A logic-based fill-in puzzle solver implemented in **Prolog**, designed to fill crossword-style grids using a provided list of words.

## Overview

This solver defines the predicate:

```prolog
puzzle_solution(+Puzzle, +WordList)
```

It succeeds when `Puzzle` is a solved fill-in puzzle that correctly places all words from `WordList`.

### Example

```prolog
?- puzzle_solution([[#, _, #],
                    [_, _, _],
                    [#, _, #]],
                   [[h, a, t], [b, a, g]]).
true.
```

## Key Features

* **Automatic Slot Extraction**
  Finds all fillable rows and columns from the puzzle grid (ignores `#` cells).
* **Smart Word Matching**
  Chooses the slot with the fewest candidate words first to reduce backtracking.
* **Deterministic Search**
  Stops after finding the first valid solution.
* **Lightweight & Pure Prolog**
  Uses only standard libraries (`clpfd`).

## File Structure

```
puzzle_solver.pl   # Main solver implementation
README.md          # Project documentation
```

## How It Works

1. **Extract Slots** – Find consecutive cells (non-`#`) in rows and columns.
2. **Filter Short Slots** – Keep only those with length > 1.
3. **Find Minimal Slot** – Choose the slot with the fewest matching words.
4. **Recursive Matching** – Fill slots until all words are placed.
