#ifndef SEARCH_H
#define SEARCH_H

#include "position.h"

void reset_killers_and_history();
void reset_search_state();
int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move);
int quiescence_search(Position& pos, int alpha, int beta, int ply);

#endif // SEARCH_H
