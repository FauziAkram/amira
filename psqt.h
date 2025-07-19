#ifndef PSQT_H
#define PSQT_H

// External declarations for Piece-Square Tables.
// The actual data is defined in psqt.cpp.
// These arrays are used by the evaluation function to determine
// the value of a piece based on its position on the board.
// The arrays are indexed by Piece enum: PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING.

// Declaration for the array of pointers to middlegame PSTs
extern const int* pst_mg_all[6];

// Declaration for the array of pointers to endgame PSTs
extern const int* pst_eg_all[6];

#endif // PSQT_H
