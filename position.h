#ifndef POSITION_H
#define POSITION_H

#include "types.h"
#include <string>

// --- Board Representation ---
struct Position {
    uint64_t piece_bb[6];
    uint64_t color_bb[2];
    int side_to_move;
    int ep_square;
    uint8_t castling_rights;
    uint64_t zobrist_hash;
    uint64_t pawn_zobrist_key;
    int halfmove_clock;
    int fullmove_number;
    int ply;

    Position();
    uint64_t get_occupied_bb() const;
    Piece piece_on_sq(int sq) const;
    Color color_on_sq(int sq) const;
};

// --- Function Declarations ---
void init_attack_tables();
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
int generate_moves(const Position& pos, Move* moves_list, bool captures_only);
Position make_move(const Position& pos, const Move& move, bool& legal);
void parse_fen(Position& pos, const std::string& fen_str);
std::string move_to_uci(const Move& move);

#endif // POSITION_H
