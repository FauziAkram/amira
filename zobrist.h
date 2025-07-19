#ifndef ZOBRIST_H
#define ZOBRIST_H

#include "position.h"
#include <cstdint>

extern uint64_t zobrist_pieces[2][6][64];
extern uint64_t zobrist_castling[16];
extern uint64_t zobrist_ep[65];
extern uint64_t zobrist_side_to_move;

void init_zobrist();
uint64_t calculate_zobrist_hash(const Position& pos);
uint64_t calculate_pawn_zobrist_hash(const Position& pos);

#endif // ZOBRIST_H
