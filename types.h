#ifndef TYPES_H
#define TYPES_H

#include <cstdint>

// --- Constants ---
enum Piece { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE };
enum Color { WHITE, BLACK, NO_COLOR };

constexpr int MAX_PLY = 128;
constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY;
constexpr int INF_SCORE = 32000;

// Castling rights masks
constexpr uint8_t WK_CASTLE_MASK = 1;
constexpr uint8_t WQ_CASTLE_MASK = 2;
constexpr uint8_t BK_CASTLE_MASK = 4;
constexpr uint8_t BQ_CASTLE_MASK = 8;

// Rook and King starting squares
constexpr int A1_SQ = 0; constexpr int E1_SQ = 4; constexpr int H1_SQ = 7;
constexpr int G1_SQ = 6; constexpr int C1_SQ = 2;
constexpr int A8_SQ = 56; constexpr int E8_SQ = 60; constexpr int H8_SQ = 63;
constexpr int G8_SQ = 62; constexpr int C8_SQ = 58;

// --- Move Structure ---
struct Move {
    int from = 0, to = 0;
    Piece promotion = NO_PIECE;
    int score = 0;

    bool operator==(const Move& other) const;
    bool is_null() const;
};

const Move NULL_MOVE = {0, 0, NO_PIECE, 0};

// --- TT Bound Enum ---
enum TTBound { TT_EXACT, TT_LOWER, TT_UPPER, TT_NONE };

#endif // TYPES_H
