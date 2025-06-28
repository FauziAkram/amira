![APRECI~1](https://github.com/user-attachments/assets/1216b195-2f3a-4b05-aa7c-e27297a8d0aa)
# Amira Chess Engine

**Author:** ChessTubeTree (Fauzi)
**Version:** 0.2

## Description

Amira is a simple yet functional UCI (Universal Chess Interface) chess engine written entirely in a single C++ file (`main.cpp`). It focuses on brevity, correctness, and achieving reasonable playing strength without external dependencies or complex neural networks.

This engine was developed as an exercise in creating a compact and understandable chess program using Vibe-Coding, and Gemini 2.5 Pro model.

## YouTube
First episode where Amira is presented: https://www.youtube.com/watch?v=UiWe2E56PWI
In the ChessTubeTree series about Coding a Chess Engine using Vibe Coding.

## Key Features

*   **Board Representation:** Bitboards for pieces and colors.
*   **Move Generation:** Standard algorithms for all pieces, including pawn promotions, en passant, and castling with safety checks.
*   **Legality & Turn Management:** Strict adherence to chess rules, ensuring only legal moves are played and turns are managed correctly.
*   **Zobrist Hashing:** Used for the transposition table and repetition detection.
*   **Evaluation:**
    *   Tapered evaluation with separate middlegame (MG) and endgame (EG) piece values.
    *   Piece-Square Tables (PSTs) for both MG and EG.
    *   Game phase calculation based on remaining material.
*   **Search Algorithm:**
    *   Iterative Deepening.
    *   Principal Variation Search (PVS) with Alpha-Beta pruning.
    *   Quiescence Search for tactical stability.
    *   Transposition Table (TT) to cache search results.
    *   Move Ordering: TT move, MVV-LVA (Most Valuable Victim - Least Valuable Aggressor) for captures, Killer Moves, and History Heuristic.
    *   Pruning: Null Move Pruning (NMP), Late Move Reductions (LMR).
    *   Check Extensions.
*   **Game Rules:**
    *   Repetition detection (3-fold in game history and within the current search path).
    *   50-move rule detection.
*   **Time Management:** Basic time allocation for UCI `go` commands to ensure moves are made within the allowed time.
*   **UCI Protocol:** Implements essential UCI commands for compatibility with standard chess GUIs.

## Compilation

The engine is designed to be compiled with g++ using standard C++. No external libraries are required.

To compile:
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
Go to the engine management section (this varies by GUI, often called "Engines", "Manage Engines", "Add Engine", etc.).
Add Amira by pointing the GUI to the compiled executable file. The GUI should automatically detect it as a UCI engine.
Once added, you can select Amira to play against or to analyze positions.
The engine supports the UCI setoption name Hash value <MB> command to configure the size of its transposition table in Megabytes.

Project Goals
Functionality: Play legal chess according to all rules.
Correctness: Never generate or play illegal moves; correct turn management.
Simplicity & Brevity: Keep the codebase as short and easy to understand as possible within a single file.
Strength: Incorporate strong algorithms and evaluation to be as competitive as possible given the simplicity constraints.
Reliability: Always return a valid move within the allowed time and never lose on time.

Constraints
Single-File: All C++ code is in main.cpp.
No NNUE: Does not use Neural Network based evaluation.
No External Dependencies: Relies only on standard C++ libraries.
g++ Compatible: Must compile cleanly with g++.

Enjoy playing with Amira!
