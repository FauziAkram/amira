# Task Proposals from Codebase Review

## 1) Typo fix task
**Task:** Replace the README hero image alt text `APRECI~1` with a meaningful label (for example, `Amira Chess Engine banner`).

**Why:** `APRECI~1` looks like an accidental 8.3 filename fragment and reads like a typo in rendered docs.

**Evidence:** `README.md` image line.

## 2) Bug fix task
**Task:** Replace `std::memset`-based clearing of `PawnCacheEntry` and `TTEntry` vectors with type-safe clearing (`std::fill`, assignment to default-initialized objects, or a loop).

**Why:** The build emits `-Wclass-memaccess` warnings because these are non-trivial structs. Using `memset` here is fragile and can become undefined behavior if members evolve.

**Evidence:** `clear_pawn_cache()` and `clear_tt()` in `main.cpp`.

## 3) Comment / documentation discrepancy task
**Task:** Fix the malformed Markdown in the Compilation section by closing the code fence after the `g++` command and keeping the explanatory text/`Usage` section outside the fence.

**Why:** The command fence is currently left open, so the rest of the README is rendered as code, which conflicts with intended section formatting.

**Evidence:** `README.md` around the Compilation and Usage sections.

## 4) Test improvement task
**Task:** Add an automated UCI regression test script (or CI step) that launches the engine, sends `uci`, `isready`, `position`, and `go depth N`, then asserts `uciok`, `readyok`, and a legal `bestmove` format.

**Why:** The project claims correctness/reliability goals, but there is no test harness in the repo to continuously verify protocol compliance and legal move output.

**Evidence:** README goals and UCI loop implementation in `main.cpp`.
