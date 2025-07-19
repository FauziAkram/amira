#ifndef EVALUATION_H
#define EVALUATION_H

#include "position.h"

// --- Pawn Cache ---
struct PawnCacheEntry {
    uint64_t key = 0;
    int mg_score = 0;
    int eg_score = 0;
    uint64_t white_passed_pawns = 0;
    uint64_t black_passed_pawns = 0;
};

void init_pawn_cache();
void clear_pawn_cache();
void init_eval_masks();
bool is_insufficient_material(const Position& pos);
int evaluate(Position& pos);

#endif // EVALUATION_H
