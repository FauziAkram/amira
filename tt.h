#ifndef TT_H
#define TT_H

#include "types.h"
#include <cstdint>

// --- Transposition Table ---
struct TTEntry {
    uint64_t hash = 0;
    Move best_move = NULL_MOVE;
    int score = 0;
    int depth = 0;
    TTBound bound = TT_NONE;
};

void init_tt(size_t mb_size);
void clear_tt();
bool probe_tt(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move_from_tt, int& score_from_tt);
void store_tt(uint64_t hash, int depth, int ply, int score, TTBound bound, const Move& best_move);

#endif // TT_H
