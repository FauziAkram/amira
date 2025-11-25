![APRECI~1](https://github.com/user-attachments/assets/1216b195-2f3a-4b05-aa7c-e27297a8d0aa)
# Amira Chess Engine

**Author:** ChessTubeTree (Fauzi)
**Version:** 1.78

## Description

Amira is a UCI (Universal Chess Interface) chess engine written entirely in a single C++ file (Might be divided into multiple files soon). It began as a simple engine and has evolved to include a sophisticated evaluation function and advanced search algorithms, significantly boosting its playing strength. The project's core philosophy remains: create a strong, dependency-free engine that is understandable and contained within a single file.

This engine is the result of a collaborative effort between human and LLM, developed as an exercise in creating a competitive chess program from its initial creation to its subsequent enhancements. While the single-file structure has been a core part of its identity, this constraint may be lifted in future versions to facilitate more complex development.
## YouTube

The original episode where Amira was created is part of the "Coding a Chess Engine" series on the ChessTubeTree channel. Watch the first episode here:
*   **First Episode:** [https://www.youtube.com/watch?v=UiWe2E56PWI](https://www.youtube.com/watch?v=UiWe2E56PWI)
*   **Second Episode:** [https://www.youtube.com/watch?v=KCu174tZb3g](https://www.youtube.com/watch?v=KCu174tZb3g)

## Key Features

### Core Engine
*   **Board Representation:** Efficient bitboards for all pieces and colors.
*   **Move Generation:** Fast, standard algorithms for all pieces, including promotions, en passant, and castling with full legality and safety checks.
*   **Zobrist Hashing:** Used for the transposition table, repetition detection, and a separate key for the pawn evaluation cache.
*   **Pawn Evaluation Cache:** A dedicated hash table to store and retrieve pawn structure evaluations, speeding up the overall evaluation process significantly.

### Search Algorithm
*   **Iterative Deepening:** To manage time and improve move ordering.
*   **Aspiration Windows:** Narrows the search window on subsequent iterative deepening searches for significant speed gains at higher depths.
*   **Principal Variation Search (PVS):** A powerful alpha-beta pruning variant.
*   **Quiescence Search:** To ensure tactical stability and avoid the horizon effect.
*   **Transposition Table (TT):** Caches search results to avoid re-calculating known positions.
*   **Advanced Pruning Techniques:**
    *   Null Move Pruning (NMP)
    *   Late Move Reductions (LMR) with adaptive, table-driven reduction amounts.
    *   Reverse Futility Pruning (RFP)
    *   Razoring
    *   Tactical Lookahead Pruning
    *   Check Extensions
*   **Advanced Move Ordering:**
    *   TT Move (PV Move)
    *   Static Exchange Evaluation (SEE) based capture ordering.
    *   Killer Moves (two per ply).
    *   Counter-Move Heuristic.
    *   History Heuristic for quiet moves.

### Evaluation Function
The evaluation is tapered, blending middlegame (MG) and endgame (EG) scores based on the game phase. It includes a rich set of features:
*   **Material & Piece-Square Tables:** Separate PSTs for MG and EG, especially for the king.
*   **Pawn Structure (with Caching):**
    *   Isolated, Doubled, Connected, and Backward pawns.
    *   **Passed Pawns:** With bonuses increasing by rank and based on the distance to the enemy king.
*   **Piece-Specific Features:**
    *   Bishop Pair Bonus.
    *   Rooks on **open and semi-open files**.
    *   **Knight & Bishop Outposts:** Bonuses for well-supported minor pieces on strong squares.
    *   Mobility scoring for all major and minor pieces.
*   **King Safety:**
    *   **Pawn Shield:** Evaluates the pawn cover in front of a castled (or non-castled) king.
    *   **King Attack Score:** A penalty system based on the number and type of enemy pieces attacking the king's zone.
*   **Threat Evaluation:** A dedicated module to reward tactical opportunities, including:
    *   Attacks on weak, hanging, or under-defended pieces.
    *   Bonuses for safe pawn attacks on enemy pieces.
    *   Recognition of pawn push threats that create tactical problems for the opponent.
*   **Endgame Scaling:** The evaluation is scaled in endgames to better recognize drawish positions, such as those with opposite-colored bishops or a minimal material advantage.
*   **Tempo Bonus:** A small bonus for the side to move.

### Game Rules & Time Management
*   **Full Rules Adherence:** Detects draws by 3-fold repetition, the 50-move rule, and **insufficient material**.
*   **Smart Time Management:** Implements robust time allocation for UCI `go` commands, with different logic for sudden death (`movestogo`) and increment-based time controls.

## Compilation

The engine is designed to be compiled with g++ using standard C++17 or newer. No external libraries are required.

To compile for maximum performance:
```bash
g++ -o Amira main.cpp -std=c++17 -O3 -march=native -flto


(You can use a newer C++ standard like -std=c++20 if preferred, and adjust optimization flags as desired. -O3 is highly recommended for performance.)

Usage
Amira is a UCI engine and requires a UCI-compatible chess GUI (Graphical User Interface) to be played, such as:
Arena
Cute Chess
ChessBase
Scid vs. PC
Banksia GUI
And many others...

To use Amira:
Compile the main.cpp file to create an executable (e.g., Amira or Amira.exe).
Open your preferred chess GUI.
Go to the engine management section (e.g., "Engines" -> "Manage Engines").
Add Amira by pointing the GUI to the compiled executable file. The GUI will automatically detect it as a UCI engine.
You can now select Amira to play against or to analyze positions. The engine supports the setoption name Hash value <MB> command to configure its transposition table size.

Project Goals
Strength: Continuously improve playing strength by implementing modern and effective chess programming techniques.
Correctness: Always play legal moves and correctly adhere to all chess rules and the UCI protocol.
Simplicity: Maintain a clean and understandable codebase. While historically this has meant keeping the engine in a single file, this constraint may be lifted in the future to allow for easier expansion.
Reliability: Always return a valid move within the allocated time.

Constraints
Single-File: Currently, all C++ code is in `main.cpp`. This may change in a future release to improve modularity.
No NNUE: Does not use a Neural Network-based evaluation.
No External Dependencies: Relies only on standard C++ libraries.
g++ Compatible: Must compile cleanly with g++.

Enjoy playing with Amira!
