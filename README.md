![APRECI~1](https://github.com/user-attachments/assets/1216b195-2f3a-4b05-aa7c-e27297a8d0aa)
# Amira Chess Engine

**Author:** ChessTubeTree (Fauzi)
**Version:** 1.59

## Description

Amira is a UCI (Universal Chess Interface) chess engine written entirely in a single C++ file. It began as a simple engine and has evolved to include a sophisticated evaluation function and advanced search algorithms, significantly boosting its playing strength. The project's core philosophy remains: create a strong, dependency-free engine that is understandable and contained within a single file.

This engine was developed as an exercise in creating a competitive chess program, using the Vibe-Coding methodology with Gemini 2.5 Pro for both its initial creation and subsequent enhancements.

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
    *   Late Move Reductions (LMR) with adaptive reduction amounts.
    *   Reverse Futility Pruning (RFP)
    *   Razoring
    *   Check Extensions
*   **Advanced Move Ordering:**
    *   TT Move (PV Move)
    *   MVV-LVA (Most Valuable Victim - Least Valuable Aggressor) for captures.
    *   Killer Moves (two per ply).
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
*   **Tactical Awareness:**
    *   Bonuses for minor pieces attacking enemy heavy pieces.
    *   Bonuses for rooks attacking enemy minor pieces.
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
Simplicity of Form: Maintain the entire C++ codebase within a single, dependency-free file.
Reliability: Always return a valid move within the allocated time.

Constraints
Single-File: All C++ code is in main.cpp.
No NNUE: Does not use a Neural Network-based evaluation.
No External Dependencies: Relies only on standard C++ libraries.
g++ Compatible: Must compile cleanly with g++.

Enjoy playing with Amira!
