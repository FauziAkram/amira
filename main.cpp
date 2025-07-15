#include "position.h"
#include "engine.h"

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <chrono>
#include <algorithm>
#include <cstring>
#include <random>
#include <cctype>
#include <cmath>

#if defined(_MSC_VER)
#include <intrin.h>
#endif

// --- Constants (Implementation specific) ---
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY;
constexpr int INF_SCORE = 32000;
constexpr int TT_SIZE_MB_DEFAULT = 256;

// King and Rook squares for castling logic
constexpr int F1_SQ = 5;  constexpr int D1_SQ = 3;
constexpr int F8_SQ = 61; constexpr int D8_SQ = 59;
constexpr int G1_SQ = 6;  constexpr int C1_SQ = 2;
constexpr int G8_SQ = 62; constexpr int C8_SQ = 58;

// --- Zobrist Hashing Data (Global Definitions) ---
uint64_t zobrist_pieces[2][6][64];
uint64_t zobrist_castling[16];
uint64_t zobrist_ep[65];
uint64_t zobrist_side_to_move;
std::mt19937_64 rng_zobrist(0xCEC);

void init_zobrist() {
    for (int c = 0; c < 2; ++c)
        for (int p = 0; p < 6; ++p)
            for (int s = 0; s < 64; ++s)
                zobrist_pieces[c][p][s] = rng_zobrist();
    for (int i = 0; i < 16; ++i)
        zobrist_castling[i] = rng_zobrist();
    for (int i = 0; i < 65; ++i)
        zobrist_ep[i] = rng_zobrist();
    zobrist_side_to_move = rng_zobrist();
}

// --- Move Utilities ---
const Move NULL_MOVE = {0, 0, NO_PIECE, 0};

std::string move_to_uci(const Move& move) {
    if (move.is_null()) return "0000";
    std::string uci_move_str;
    uci_move_str += (char)('a' + (move.from % 8));
    uci_move_str += (char)('1' + (move.from / 8));
    uci_move_str += (char)('a' + (move.to % 8));
    uci_move_str += (char)('1' + (move.to / 8));
    if (move.promotion != NO_PIECE) {
        char promo_char = 'q';
        if (move.promotion == KNIGHT) promo_char = 'n';
        else if (move.promotion == BISHOP) promo_char = 'b';
        else if (move.promotion == ROOK) promo_char = 'r';
        uci_move_str += promo_char;
    }
    return uci_move_str;
}

// --- Bitboard Utilities ---
inline bool get_bit(uint64_t bb, int sq) { return (bb >> sq) & 1; }
inline int pop_count(uint64_t bb) {
#if defined(_MSC_VER)
    return static_cast<int>(__popcnt64(bb));
#elif defined(__GNUC__) || defined(__clang__)
    return __builtin_popcountll(bb);
#else
    int count = 0;
    while (bb > 0) { bb &= (bb - 1); count++; }
    return count;
#endif
}
inline int lsb_index(uint64_t bb) {
    if (bb == 0) return -1;
#if defined(_MSC_VER)
    unsigned long idx; _BitScanForward64(&idx, bb); return static_cast<int>(idx);
#elif defined(__GNUC__) || defined(__clang__)
    return __builtin_ctzll(bb);
#else
    int count = 0; while (!((bb >> count) & 1)) { count++; if (count >= 64) return -1; }
    return count;
#endif
}

uint64_t north(uint64_t b) { return b << 8; }
uint64_t south(uint64_t b) { return b >> 8; }
uint64_t east(uint64_t b) { return (b << 1) & ~0x0101010101010101ULL; }
uint64_t west(uint64_t b) { return (b >> 1) & ~0x8080808080808080ULL; }
uint64_t nw(uint64_t b) { return north(west(b)); }
uint64_t ne(uint64_t b) { return north(east(b)); }
uint64_t sw(uint64_t b) { return south(west(b)); }
uint64_t se(uint64_t b) { return south(east(b)); }

// --- Board Representation Implementation ---
Position::Position() {
    std::memset(this, 0, sizeof(Position));
    side_to_move = WHITE;
    ep_square = -1;
    fullmove_number = 1;
}

Piece Position::piece_on_sq(int sq) const {
    if (sq < 0 || sq >= 64) return NO_PIECE;
    uint64_t b = set_bit(sq);
    if (!((color_bb[WHITE] | color_bb[BLACK]) & b)) return NO_PIECE;
    for (int p = PAWN; p <= KING; ++p) {
        if (piece_bb[p] & b) return (Piece)p;
    }
    return NO_PIECE;
}

Color Position::color_on_sq(int sq) const {
    if (sq < 0 || sq >= 64) return NO_COLOR;
    uint64_t b = set_bit(sq);
    if (color_bb[WHITE] & b) return WHITE;
    if (color_bb[BLACK] & b) return BLACK;
    return NO_COLOR;
}

// --- Attack Tables and Initialization ---
uint64_t pawn_attacks_bb[2][64];
uint64_t knight_attacks_bb[64];
uint64_t king_attacks_bb[64];

void init_attack_tables() {
    for (int sq = 0; sq < 64; ++sq) {
        uint64_t b = set_bit(sq);
        pawn_attacks_bb[WHITE][sq] = nw(b) | ne(b);
        pawn_attacks_bb[BLACK][sq] = sw(b) | se(b);

        knight_attacks_bb[sq] = (
            ((b << 17) & ~0x0101010101010101ULL) | ((b << 15) & ~0x8080808080808080ULL) |
            ((b << 10) & ~0x0303030303030303ULL) | ((b << 6)  & ~0xC0C0C0C0C0C0C0C0ULL) |
            ((b >> 17) & ~0x8080808080808080ULL) | ((b >> 15) & ~0x0101010101010101ULL) |
            ((b >> 10) & ~0xC0C0C0C0C0C0C0C0ULL) | ((b >> 6)  & ~0x03030303030303ULL)
        );

        king_attacks_bb[sq] = north(b) | south(b) | east(b) | west(b) |
                              ne(b) | nw(b) | se(b) | sw(b);
    }
}

// --- Slider Attack Generation ---
uint64_t get_rook_attacks_from_sq(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {1, -1, 8, -8}; // E, W, N, S
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8;
            int r_prev = (s-d) / 8;
            if (std::abs(d) == 1 && r_curr != r_prev) break; // Horizontal wrap-around

            attacks |= set_bit(s);
            if (get_bit(occupied, s)) break;
        }
    }
    return attacks;
}

uint64_t get_bishop_attacks_from_sq(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {9, -9, 7, -7}; // NE, SW, NW, SE
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (std::abs(r_curr - r_prev) != 1 || std::abs(c_curr - c_prev) != 1) break; // Diagonal wrap-around

            attacks |= set_bit(s);
            if (get_bit(occupied, s)) break;
        }
    }
    return attacks;
}

uint64_t get_slider_attacks_for_movegen(int sq, Piece piece_type, uint64_t occupied) {
    if (piece_type == ROOK) return get_rook_attacks_from_sq(sq, occupied);
    if (piece_type == BISHOP) return get_bishop_attacks_from_sq(sq, occupied);
    if (piece_type == QUEEN) return get_rook_attacks_from_sq(sq, occupied) | get_bishop_attacks_from_sq(sq, occupied);
    return 0;
}

// --- is_square_attacked Implementation ---
bool is_square_attacked(const Position& pos, int sq_to_check, int attacker_c) {
    uint64_t attacker_pawns = pos.piece_bb[PAWN] & pos.color_bb[attacker_c];
    if (pawn_attacks_bb[1 - attacker_c][sq_to_check] & attacker_pawns) return true;

    uint64_t attacker_knights = pos.piece_bb[KNIGHT] & pos.color_bb[attacker_c];
    if (knight_attacks_bb[sq_to_check] & attacker_knights) return true;

    uint64_t attacker_king = pos.piece_bb[KING] & pos.color_bb[attacker_c];
    if (king_attacks_bb[sq_to_check] & attacker_king) return true;

    uint64_t occupied = pos.get_occupied_bb();

    uint64_t rook_queen_attackers = (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_c];
    if (get_rook_attacks_from_sq(sq_to_check, occupied) & rook_queen_attackers) return true;

    uint64_t bishop_queen_attackers = (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_c];
    if (get_bishop_attacks_from_sq(sq_to_check, occupied) & bishop_queen_attackers) return true;

    return false;
}

// --- Move Generation Implementation ---
int generate_moves(const Position& pos, Move* moves_list, bool captures_only) {
    int move_count = 0;
    int stm = pos.side_to_move;
    Color friendly_color = (Color)stm;
    Color enemy_color = (Color)(1 - stm);
    uint64_t my_pieces = pos.color_bb[friendly_color];
    uint64_t opp_pieces = pos.color_bb[enemy_color];
    uint64_t occupied = my_pieces | opp_pieces;
    uint64_t empty_squares = ~occupied;

    uint64_t pawns = pos.piece_bb[PAWN] & my_pieces;
    while (pawns) {
        int from = lsb_index(pawns);
        pawns &= pawns - 1;
        int rank = from / 8;
        int promotion_rank_idx = (stm == WHITE) ? 6 : 1; // Rank *before* promotion

        // Pawn pushes
        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >=0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (rank == promotion_rank_idx) { // Reached promotion rank by one step
                moves_list[move_count++] = {from, one_step_sq, QUEEN}; moves_list[move_count++] = {from, one_step_sq, ROOK};
                moves_list[move_count++] = {from, one_step_sq, BISHOP}; moves_list[move_count++] = {from, one_step_sq, KNIGHT};
            } else if (!captures_only) {
                moves_list[move_count++] = {from, one_step_sq};
            }
            // Double pawn push (only if single push is also possible and not captures_only)
            if (!captures_only) {
                int start_rank_idx = (stm == WHITE) ? 1 : 6;
                if (rank == start_rank_idx) {
                    int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                    if (two_steps_sq >=0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq)) {
                        moves_list[move_count++] = {from, two_steps_sq};
                    }
                }
            }
        }
        // Pawn captures (including EP)
        uint64_t pawn_cap_targets = pawn_attacks_bb[stm][from] & opp_pieces;
        if (pos.ep_square != -1) {
             if (get_bit(pawn_attacks_bb[stm][from], pos.ep_square)) { // Is the EP square a valid capture target?
                 pawn_cap_targets |= set_bit(pos.ep_square);
             }
        }
        while (pawn_cap_targets) {
            int to = lsb_index(pawn_cap_targets);
            pawn_cap_targets &= pawn_cap_targets - 1;
            if (rank == promotion_rank_idx) { // Reached promotion rank by capture
                moves_list[move_count++] = {from, to, QUEEN}; moves_list[move_count++] = {from, to, ROOK};
                moves_list[move_count++] = {from, to, BISHOP}; moves_list[move_count++] = {from, to, KNIGHT};
            } else {
                moves_list[move_count++] = {from, to};
            }
        }
    }

    Piece piece_types_non_pawn[] = {KNIGHT, BISHOP, ROOK, QUEEN, KING};
    for (Piece p_type : piece_types_non_pawn) {
        uint64_t pieces_of_type = pos.piece_bb[p_type] & my_pieces;
        while (pieces_of_type) {
            int from = lsb_index(pieces_of_type);
            pieces_of_type &= pieces_of_type - 1;

            uint64_t attacks = 0;
            if (p_type == KNIGHT) attacks = knight_attacks_bb[from];
            else if (p_type == KING) attacks = king_attacks_bb[from];
            else attacks = get_slider_attacks_for_movegen(from, p_type, occupied);

            attacks &= (captures_only ? opp_pieces : ~my_pieces); // Only captures or any non-friendly square

            while (attacks) {
                int to = lsb_index(attacks);
                attacks &= attacks - 1;
                moves_list[move_count++] = {from, to};
            }
        }
    }

    // Castling
    if (!captures_only) {
        int king_sq_idx = lsb_index(pos.piece_bb[KING] & my_pieces);
        if (king_sq_idx != -1) { // Should always be true in a valid position
            if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, F1_SQ) && !get_bit(occupied, G1_SQ) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, F1_SQ, BLACK) && !is_square_attacked(pos, G1_SQ, BLACK)) {
                    moves_list[move_count++] = {E1_SQ, G1_SQ}; // King to G1
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, D1_SQ) && !get_bit(occupied, C1_SQ) && !get_bit(occupied, B1_SQ) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, D1_SQ, BLACK) && !is_square_attacked(pos, C1_SQ, BLACK)) {
                    moves_list[move_count++] = {E1_SQ, C1_SQ}; // King to C1
                }
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, F8_SQ) && !get_bit(occupied, G8_SQ) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, F8_SQ, WHITE) && !is_square_attacked(pos, G8_SQ, WHITE)) {
                    moves_list[move_count++] = {E8_SQ, G8_SQ}; // King to G8
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, D8_SQ) && !get_bit(occupied, C8_SQ) && !get_bit(occupied, B8_SQ) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, D8_SQ, WHITE) && !is_square_attacked(pos, C8_SQ, WHITE)) {
                    moves_list[move_count++] = {E8_SQ, C8_SQ}; // King to C8
                }
            }
        }
    }
    return move_count;
}

// --- Make Move Implementation (Corrected) ---
Position make_move(const Position& pos, const Move& move, bool& legal_move_flag) {
    legal_move_flag = false;
    Position next_pos = pos;
    int stm = pos.side_to_move;
    int opp = 1 - stm;

    uint64_t from_bb = set_bit(move.from);
    uint64_t to_bb = set_bit(move.to);
    Piece piece_moved = pos.piece_on_sq(move.from);
    Piece piece_captured = pos.piece_on_sq(move.to);

    if (piece_moved == NO_PIECE || pos.color_on_sq(move.from) != stm) return pos;
    if (piece_captured != NO_PIECE && pos.color_on_sq(move.to) == stm) return pos;

    next_pos.zobrist_hash = pos.zobrist_hash;
    next_pos.halfmove_clock++;
    if (pos.ep_square != -1) next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];

    // --- Piece Movement ---
    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;
    next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.from];

    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        next_pos.halfmove_clock = 0;
    }
    
    if (piece_moved == PAWN) {
        next_pos.halfmove_clock = 0;
        if (move.to == pos.ep_square && pos.ep_square != -1) {
            int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
            next_pos.piece_bb[PAWN] &= ~set_bit(captured_pawn_sq);
            next_pos.color_bb[opp] &= ~set_bit(captured_pawn_sq);
            next_pos.zobrist_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
        }
    }

    if (move.promotion != NO_PIECE) {
        if (piece_moved != PAWN) return pos;
        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else {
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    if (piece_moved == PAWN && std::abs(move.to - move.from) == 16) {
        next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    } else {
        next_pos.ep_square = -1;
    }
    if (next_pos.ep_square != -1) next_pos.zobrist_hash ^= zobrist_ep[next_pos.ep_square];

    if (piece_moved == KING && std::abs(move.to - move.from) == 2) {
        int rook_from, rook_to;
        if (move.to == G1_SQ)      { rook_from = H1_SQ; rook_to = F1_SQ; }
        else if (move.to == C1_SQ) { rook_from = A1_SQ; rook_to = D1_SQ; }
        else if (move.to == G8_SQ) { rook_from = H8_SQ; rook_to = F8_SQ; }
        else                       { rook_from = A8_SQ; rook_to = D8_SQ; }

        next_pos.piece_bb[ROOK] &= ~set_bit(rook_from);
        next_pos.color_bb[stm] &= ~set_bit(rook_from);
        next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from];
        
        next_pos.piece_bb[ROOK] |= set_bit(rook_to);
        next_pos.color_bb[stm] |= set_bit(rook_to);
        next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to];
    }

    uint8_t old_castling_rights = pos.castling_rights;
    uint8_t new_castling_rights = old_castling_rights;
    
    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);
    }
    if (move.from == H1_SQ || move.to == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
    if (move.from == A1_SQ || move.to == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
    if (move.from == H8_SQ || move.to == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
    if (move.from == A8_SQ || move.to == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;

    next_pos.castling_rights = new_castling_rights;
    if (new_castling_rights != old_castling_rights) {
        next_pos.zobrist_hash ^= zobrist_castling[old_castling_rights];
        next_pos.zobrist_hash ^= zobrist_castling[new_castling_rights];
    }

    next_pos.side_to_move = opp;
    next_pos.zobrist_hash ^= zobrist_side_to_move;
    
    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply = pos.ply + 1;

    int king_sq = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq != -1 && is_square_attacked(next_pos, king_sq, opp)) {
        return pos;
    }

    legal_move_flag = true;
    return next_pos;
}


// --- All Evaluation and Search code follows ---

// King safety penalty tables
const int king_danger_penalty_mg[15] = {0, 0, 5, 15, 30, 50, 75, 100, 130, 160, 200, 240, 280, 320, 350};
const int king_danger_penalty_eg[15] = {0, 0, 0,  5, 10, 15,  20,  30,  40,  50,  60,  70,  80,  90, 100};

// --- Evaluation Data ---
const int piece_values_mg[6] = {100, 320, 330, 500, 900, 0};
const int piece_values_eg[6] = {120, 320, 330, 530, 950, 0};
const int pawn_pst[64] = {
     0,  0,  0,  0,  0,  0,  0,  0,   5, 10, 10,-20,-20, 10, 10,  5,
     5, -5,-10,  0,  0,-10, -5,  5,   0,  0,  0, 20, 20,  0,  0,  0,
     5,  5, 10, 25, 25, 10,  5,  5,  10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,   0,  0,  0,  0,  0,  0,  0,  0
};
const int knight_pst[64] = {
    -50,-40,-30,-30,-30,-30,-40,-50, -40,-20,  0,  5,  5,  0,-20,-40,
    -30,  5, 10, 15, 15, 10,  5,-30, -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30, -30,  0, 10, 15, 15, 10,  0,-30,
    -40,-20,  0,  0,  0,  0,-20,-40, -50,-40,-30,-30,-30,-30,-40,-50
};
const int bishop_pst[64] = {
    -20,-10,-10,-10,-10,-10,-10,-20, -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10, -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10, -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10, -20,-10,-10,-10,-10,-10,-10,-20
};
const int rook_pst[64] = {
     0,  0,  0,  5,  5,  0,  0,  0,  -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,  -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,  -5,  0,  0,  0,  0,  0,  0, -5,
     5, 10, 10, 10, 10, 10, 10,  5,   0,  0,  0,  0,  0,  0,  0,  0
};
const int queen_pst[64] = {
    -20,-10,-10, -5, -5,-10,-10,-20, -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,  -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5, -10,  0,  5,  5,  5,  5,  0,-10,
    -10,  0,  0,  0,  0,  0,  0,-10, -20,-10,-10, -5, -5,-10,-10,-20
};
const int king_pst_mg[64] = {
     20, 30, 10,  0,  0, 10, 30, 20,  20, 20,  0,  0,  0,  0, 20, 20,
    -10,-20,-20,-20,-20,-20,-20,-10, -20,-30,-30,-40,-40,-30,-30,-20,
    -30,-40,-40,-50,-50,-40,-40,-30, -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30, -30,-40,-40,-50,-50,-40,-40,-30
};
const int king_pst_eg[64] = {
   -50,-30,-30,-30,-30,-30,-30,-50, -30,-10, 20, 30, 30, 20,-10,-30,
   -30, 10, 30, 40, 40, 30, 10,-30, -30, 10, 40, 50, 50, 40, 10,-30,
   -30, 10, 40, 50, 50, 40, 10,-30, -30, 10, 30, 40, 40, 30, 10,-30,
   -30,-10, 20, 30, 30, 20,-10,-30, -50,-30,-30,-30,-30,-30,-30,-50
};

const int* pst_mg_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst_mg};
const int* pst_eg_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst_eg};
const int game_phase_inc[6] = {0, 1, 1, 2, 4, 0};

uint64_t file_bb_mask[8];
uint64_t rank_bb_mask[8];
uint64_t white_passed_pawn_block_mask[64];
uint64_t black_passed_pawn_block_mask[64];
uint64_t adjacent_files_mask[8];

const int passed_pawn_bonus_mg[8] = {0, 5, 15, 25, 40, 60, 80, 0};
const int passed_pawn_bonus_eg[8] = {0, 10, 25, 40, 60, 90, 120, 0};
const int TEMPO_BONUS = 8;
const int protected_pawn_bonus_mg = 8;
const int protected_pawn_bonus_eg = 12;
const int isolated_pawn_penalty_mg = -12;
const int isolated_pawn_penalty_eg = -20;
const int doubled_pawn_liability_mg = -10;
const int doubled_pawn_liability_eg = -15;
const int hindered_pawn_penalty_mg = -8;
const int hindered_pawn_penalty_eg = -12;
const int knight_mobility_bonus_mg = 1;
const int knight_mobility_bonus_eg = 2;
const int bishop_mobility_bonus_mg = 2;
const int bishop_mobility_bonus_eg = 3;
const int rook_mobility_bonus_mg = 2;
const int rook_mobility_bonus_eg = 4;
const int queen_mobility_bonus_mg = 1;
const int queen_mobility_bonus_eg = 2;
const int dominant_knight_bonus_mg = 25;
const int dominant_knight_bonus_eg = 15;
const int dominant_bishop_bonus_mg = 20;
const int dominant_bishop_bonus_eg = 15;
const int potential_dominance_bonus = 5;
const int minor_on_heavy_pressure_mg = 20;
const int minor_on_heavy_pressure_eg = 15;
const int rook_on_minor_pressure_mg = 15;
const int rook_on_minor_pressure_eg = 10;
const int passed_pawn_enemy_king_dist_bonus_eg = 4;

void init_eval_masks() {
    for (int f = 0; f < 8; ++f) {
        file_bb_mask[f] = 0ULL;
        for (int r = 0; r < 8; ++r) file_bb_mask[f] |= set_bit(r * 8 + f);
        adjacent_files_mask[f] = 0ULL;
        if (f > 0) adjacent_files_mask[f] |= file_bb_mask[f-1];
        if (f < 7) adjacent_files_mask[f] |= file_bb_mask[f+1];
    }
    for (int r = 0; r < 8; ++r) {
        rank_bb_mask[r] = 0ULL;
        for (int f = 0; f < 8; ++f) rank_bb_mask[r] |= set_bit(r * 8 + f);
    }
    for (int sq = 0; sq < 64; ++sq) {
        white_passed_pawn_block_mask[sq] = 0ULL;
        black_passed_pawn_block_mask[sq] = 0ULL;
        int r = sq / 8, f = sq % 8;
        for (int cur_r = r + 1; cur_r < 8; ++cur_r) {
            white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f + 1));
        }
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) {
            black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f + 1));
        }
    }
}

bool is_insufficient_material(const Position& pos) {
    if (pos.piece_bb[PAWN] != 0 || pos.piece_bb[ROOK] != 0 || pos.piece_bb[QUEEN] != 0) return false;
    int white_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[WHITE]);
    int white_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]);
    int black_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[BLACK]);
    int black_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]);
    int white_minors = white_knights + white_bishops;
    int black_minors = black_knights + black_bishops;
    if (white_minors == 0 && black_minors == 0) return true;
    if ((white_minors == 1 && black_minors == 0) || (white_minors == 0 && black_minors == 1)) return true;
    if (white_minors == 1 && black_minors == 1) return true;
    if ((white_minors == 2 && black_minors == 0 && white_knights == 2) ||
        (white_minors == 0 && black_minors == 2 && black_knights == 2)) return true;
    return false;
}

int evaluate(const Position& pos) {
    if (is_insufficient_material(pos)) return 0;
    int mg_score = 0, eg_score = 0, game_phase = 0;
    int mg_mobility_score = 0, eg_mobility_score = 0;
    uint64_t piece_attacks_bb[2][6] = {{0}};

    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        Color enemy_color = (Color)(1-c_idx);
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;
        
        uint64_t friendly_pieces = pos.color_bb[current_eval_color];
        uint64_t enemy_pieces = pos.color_bb[enemy_color];
        uint64_t attackable_squares = ~friendly_pieces;
        uint64_t occupied = pos.get_occupied_bb();
        uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & friendly_pieces;
        uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & enemy_pieces;
        uint64_t enemy_pawn_attacks = 0;
        uint64_t temp_enemy_pawns = all_enemy_pawns;
        while(temp_enemy_pawns) {
            int pawn_sq = lsb_index(temp_enemy_pawns);
            enemy_pawn_attacks |= pawn_attacks_bb[enemy_color][pawn_sq];
            temp_enemy_pawns &= temp_enemy_pawns - 1;
        }

        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);

                mg_score += side_multiplier * (piece_values_mg[p] + pst_mg_all[p][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p] + pst_eg_all[p][mirrored_sq]);

                if ((Piece)p == PAWN) {
                    int f = sq % 8;
                    if ((adjacent_files_mask[f] & all_friendly_pawns) == 0) {
                        mg_score += side_multiplier * isolated_pawn_penalty_mg;
                        eg_score += side_multiplier * isolated_pawn_penalty_eg;
                    }
                    if (pawn_attacks_bb[1 - current_eval_color][sq] & all_friendly_pawns) {
                        mg_score += side_multiplier * protected_pawn_bonus_mg;
                        eg_score += side_multiplier * protected_pawn_bonus_eg;
                    }
                    uint64_t forward_file_squares = (current_eval_color == WHITE) ? north(set_bit(sq)) : south(set_bit(sq));
                    if ((file_bb_mask[f] & forward_file_squares & all_friendly_pawns) != 0) {
                         mg_score += side_multiplier * doubled_pawn_liability_mg;
                         eg_score += side_multiplier * doubled_pawn_liability_eg;
                    }
                    uint64_t front_span = (current_eval_color == WHITE) ? white_passed_pawn_block_mask[sq] : black_passed_pawn_block_mask[sq];
                    uint64_t adjacent_pawns = adjacent_files_mask[f] & all_friendly_pawns;
                    if ((front_span & adjacent_pawns) == 0) {
                        int push_sq = (current_eval_color == WHITE) ? sq + 8 : sq - 8;
                        if (get_bit(enemy_pawn_attacks, push_sq)) {
                           mg_score += side_multiplier * hindered_pawn_penalty_mg;
                           eg_score += side_multiplier * hindered_pawn_penalty_eg;
                        }
                    }
                    bool is_passed = false;
                    if (current_eval_color == WHITE) {
                        if ((white_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0) is_passed = true;
                    } else {
                        if ((black_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0) is_passed = true;
                    }
                    if (is_passed) {
                        int rank_from_own_side = (current_eval_color == WHITE) ? (sq / 8) : (7 - (sq / 8));
                        mg_score += side_multiplier * passed_pawn_bonus_mg[rank_from_own_side];
                        eg_score += side_multiplier * passed_pawn_bonus_eg[rank_from_own_side];
                        
                        int enemy_king_sq = lsb_index(pos.piece_bb[KING] & enemy_pieces);
                        if (enemy_king_sq != -1) {
                            int pawn_rank = sq / 8; int pawn_file = sq % 8;
                            int king_rank = enemy_king_sq / 8; int king_file = enemy_king_sq % 8;
                            int dist_to_enemy_king = std::max(std::abs(pawn_rank - king_rank), std::abs(pawn_file - king_file));
                            eg_score += side_multiplier * dist_to_enemy_king * passed_pawn_enemy_king_dist_bonus_eg;
                        }
                    }
                } else if ((Piece)p == ROOK) {
                    int f = sq % 8;
                    bool friendly_pawn_on_file = (file_bb_mask[f] & all_friendly_pawns) != 0;
                    bool enemy_pawn_on_file = (file_bb_mask[f] & all_enemy_pawns) != 0;
                    if (!friendly_pawn_on_file) {
                        if (!enemy_pawn_on_file) { mg_score += side_multiplier * 20; eg_score += side_multiplier * 15; }
                        else { mg_score += side_multiplier * 10; eg_score += side_multiplier * 5;}
                    }
                    uint64_t mobility_attacks = get_rook_attacks_from_sq(sq, occupied);
                    piece_attacks_bb[c_idx][ROOK] |= mobility_attacks;
                    int mobility_count = pop_count(mobility_attacks & attackable_squares);
                    mg_mobility_score += side_multiplier * mobility_count * rook_mobility_bonus_mg;
                    eg_mobility_score += side_multiplier * mobility_count * rook_mobility_bonus_eg;
                }
                else if ((Piece)p == KNIGHT) {
                    uint64_t mobility_attacks = knight_attacks_bb[sq];
                    piece_attacks_bb[c_idx][KNIGHT] |= mobility_attacks;
                    int mobility_count = pop_count(mobility_attacks & attackable_squares);
                    mg_mobility_score += side_multiplier * mobility_count * knight_mobility_bonus_mg;
                    eg_mobility_score += side_multiplier * mobility_count * knight_mobility_bonus_eg;
                    int rank = sq / 8;
                    int relative_rank_idx = (current_eval_color == WHITE) ? rank : 7 - rank;
                    if (relative_rank_idx >= 3 && relative_rank_idx <= 5) {
                        if (get_bit(pawn_attacks_bb[enemy_color][sq], all_friendly_pawns)) {
                             if (!get_bit(enemy_pawn_attacks, sq)) {
                                mg_score += side_multiplier * dominant_knight_bonus_mg;
                                eg_score += side_multiplier * dominant_knight_bonus_eg;
                            }
                        }
                    }
                    uint64_t potential_outpost_moves = mobility_attacks & ~occupied;
                    while(potential_outpost_moves) {
                        int to_sq = lsb_index(potential_outpost_moves);
                        potential_outpost_moves &= potential_outpost_moves-1;
                        int to_rank = to_sq/8;
                        int to_relative_rank = (current_eval_color == WHITE) ? to_rank : 7-to_rank;
                        if(to_relative_rank >= 3 && to_relative_rank <=5) {
                            if(get_bit(pawn_attacks_bb[enemy_color][to_sq], all_friendly_pawns) && !get_bit(enemy_pawn_attacks, to_sq)) {
                                mg_score += side_multiplier * potential_dominance_bonus;
                                break;
                            }
                        }
                    }
                } else if ((Piece)p == BISHOP) {
                    uint64_t mobility_attacks = get_bishop_attacks_from_sq(sq, occupied);
                    piece_attacks_bb[c_idx][BISHOP] |= mobility_attacks;
                    int mobility_count = pop_count(mobility_attacks & attackable_squares);
                    mg_mobility_score += side_multiplier * mobility_count * bishop_mobility_bonus_mg;
                    eg_mobility_score += side_multiplier * mobility_count * bishop_mobility_bonus_eg;
                    int rank = sq / 8;
                    int relative_rank_idx = (current_eval_color == WHITE) ? rank : 7 - rank;
                     if (relative_rank_idx >= 3 && relative_rank_idx <= 5) {
                        if (get_bit(pawn_attacks_bb[enemy_color][sq], all_friendly_pawns)) {
                             if (!get_bit(enemy_pawn_attacks, sq)) {
                                mg_score += side_multiplier * dominant_bishop_bonus_mg;
                                eg_score += side_multiplier * dominant_bishop_bonus_eg;
                            }
                        }
                    }
                } else if ((Piece)p == QUEEN) {
                    uint64_t mobility_attacks = get_rook_attacks_from_sq(sq, occupied) | get_bishop_attacks_from_sq(sq, occupied);
                    piece_attacks_bb[c_idx][QUEEN] |= mobility_attacks;
                    int mobility_count = pop_count(mobility_attacks & attackable_squares);
                    mg_mobility_score += side_multiplier * mobility_count * queen_mobility_bonus_mg;
                    eg_mobility_score += side_multiplier * mobility_count * queen_mobility_bonus_eg;
                } else if ((Piece)p == KING) {
                    piece_attacks_bb[c_idx][KING] |= king_attacks_bb[sq];
                }
            }
        }
        if (pop_count(pos.piece_bb[BISHOP] & pos.color_bb[current_eval_color]) >= 2) {
            mg_score += side_multiplier * 30; eg_score += side_multiplier * 50;
        }

        uint64_t enemy_rooks_and_queens = (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & enemy_pieces;
        uint64_t enemy_knights_and_bishops = (pos.piece_bb[KNIGHT] | pos.piece_bb[BISHOP]) & enemy_pieces;
        uint64_t minor_attacks = piece_attacks_bb[c_idx][KNIGHT] | piece_attacks_bb[c_idx][BISHOP];
        if (pop_count(minor_attacks & enemy_rooks_and_queens) > 0) {
            mg_score += side_multiplier * minor_on_heavy_pressure_mg * pop_count(minor_attacks & enemy_rooks_and_queens);
            eg_score += side_multiplier * minor_on_heavy_pressure_eg * pop_count(minor_attacks & enemy_rooks_and_queens);
        }
        if (pop_count(piece_attacks_bb[c_idx][ROOK] & enemy_knights_and_bishops) > 0) {
             mg_score += side_multiplier * rook_on_minor_pressure_mg * pop_count(piece_attacks_bb[c_idx][ROOK] & enemy_knights_and_bishops);
             eg_score += side_multiplier * rook_on_minor_pressure_eg * pop_count(piece_attacks_bb[c_idx][ROOK] & enemy_knights_and_bishops);
        }

        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[current_eval_color]);
        if (king_sq != -1) {
            int king_rank = king_sq / 8, king_file = king_sq % 8;
            int pawn_shield_score = 0;
            const int shield_pawn_bonus = 12;
            const int missing_shield_pawn_penalty = -18;
            const int open_file_penalty_adj = -10;
            int shield_candidate_sqs[3] = {-1, -1, -1};
            bool relevant_castled_pos = false;

            if (current_eval_color == WHITE) {
                if (king_sq == G1_SQ) { shield_candidate_sqs[0] = G1_SQ + 7; shield_candidate_sqs[1] = G1_SQ + 8; shield_candidate_sqs[2] = G1_SQ + 9; relevant_castled_pos = true; }
                else if (king_sq == C1_SQ) { shield_candidate_sqs[0] = C1_SQ + 7; shield_candidate_sqs[1] = C1_SQ + 8; shield_candidate_sqs[2] = C1_SQ + 9; relevant_castled_pos = true; }
            } else {
                if (king_sq == G8_SQ) { shield_candidate_sqs[0] = G8_SQ - 9; shield_candidate_sqs[1] = G8_SQ - 8; shield_candidate_sqs[2] = G8_SQ - 7; relevant_castled_pos = true; }
                else if (king_sq == C8_SQ) { shield_candidate_sqs[0] = C8_SQ - 9; shield_candidate_sqs[1] = C8_SQ - 8; shield_candidate_sqs[2] = C8_SQ - 7; relevant_castled_pos = true; }
            }

            if (relevant_castled_pos) {
                for (int sq_idx = 0; sq_idx < 3; ++sq_idx) {
                    int shield_sq = shield_candidate_sqs[sq_idx];
                    if (shield_sq >=0 && shield_sq < 64) {
                        if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) {
                            pawn_shield_score += shield_pawn_bonus;
                        } else {
                            pawn_shield_score += missing_shield_pawn_penalty;
                            int shield_f = shield_sq % 8;
                            if ((file_bb_mask[shield_f] & all_friendly_pawns) == 0 && (file_bb_mask[shield_f] & all_enemy_pawns) == 0) {
                                pawn_shield_score += open_file_penalty_adj;
                            }
                        }
                    }
                }
            } else {
                int base_shield_rank = (current_eval_color == WHITE) ? (king_rank + 1) : (king_rank - 1);
                if (base_shield_rank >= 0 && base_shield_rank < 8) {
                    for (int df = -1; df <= 1; ++df) {
                        int current_shield_file = king_file + df;
                        if (current_shield_file >= 0 && current_shield_file < 8) {
                            int shield_sq = base_shield_rank * 8 + current_shield_file;
                            if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) pawn_shield_score += shield_pawn_bonus / 2;
                            else {
                                pawn_shield_score += missing_shield_pawn_penalty / 2;
                                int shield_f = shield_sq % 8;
                                if ((file_bb_mask[shield_f] & all_friendly_pawns) == 0 && (file_bb_mask[shield_f] & all_enemy_pawns) == 0) {
                                    pawn_shield_score += open_file_penalty_adj / 2;
                                }
                            }
                        }
                    }
                }
            }
            mg_score += side_multiplier * pawn_shield_score;

            int king_attack_score = 0;
            uint64_t king_1_ring_zone = king_attacks_bb[king_sq];
            uint64_t enemy_player_pieces = pos.color_bb[1 - current_eval_color];
            uint64_t occupied_bb = pos.get_occupied_bb();
            uint64_t enemy_knights = pos.piece_bb[KNIGHT] & enemy_player_pieces;
            while(enemy_knights) { int sq = lsb_index(enemy_knights); enemy_knights &= enemy_knights - 1; if (knight_attacks_bb[sq] & king_1_ring_zone) king_attack_score += 2; }
            uint64_t enemy_bishops = pos.piece_bb[BISHOP] & enemy_player_pieces;
            while(enemy_bishops) { int sq = lsb_index(enemy_bishops); enemy_bishops &= enemy_bishops - 1; if (get_bishop_attacks_from_sq(sq, occupied_bb) & king_1_ring_zone) king_attack_score += 2; }
            uint64_t enemy_rooks = pos.piece_bb[ROOK] & enemy_player_pieces;
            while(enemy_rooks) { int sq = lsb_index(enemy_rooks); enemy_rooks &= enemy_rooks - 1; if (get_rook_attacks_from_sq(sq, occupied_bb) & king_1_ring_zone) king_attack_score += 3; }
            uint64_t enemy_queens = pos.piece_bb[QUEEN] & enemy_player_pieces;
            while(enemy_queens) { int sq = lsb_index(enemy_queens); enemy_queens &= enemy_queens - 1; if (get_bishop_attacks_from_sq(sq, occupied_bb) & king_1_ring_zone) king_attack_score += 4; if (get_rook_attacks_from_sq(sq, occupied_bb) & king_1_ring_zone) king_attack_score += 5; }
            uint64_t temp_king_zone_for_pawns = king_1_ring_zone;
            while(temp_king_zone_for_pawns) { int sq = lsb_index(temp_king_zone_for_pawns); temp_king_zone_for_pawns &= temp_king_zone_for_pawns -1; if (pawn_attacks_bb[current_eval_color][sq] & all_enemy_pawns) king_attack_score += 1; }
            
            int danger_idx = std::min(king_attack_score, 14);
            mg_score -= side_multiplier * king_danger_penalty_mg[danger_idx];
            eg_score -= side_multiplier * king_danger_penalty_eg[danger_idx];
        }
    }

    mg_score += mg_mobility_score;
    eg_score += eg_mobility_score;

    if (pos.side_to_move == WHITE) { mg_score += TEMPO_BONUS; eg_score += TEMPO_BONUS; }
    else { mg_score -= TEMPO_BONUS; eg_score -= TEMPO_BONUS; }

    game_phase = std::min(game_phase, 24);
    game_phase = std::max(game_phase, 0);

    int final_score_from_white_pov = (mg_score * game_phase + eg_score * (24 - game_phase)) / 24;
    return (pos.side_to_move == WHITE) ? final_score_from_white_pov : -final_score_from_white_pov;
}

// --- Transposition Table Data & Implementation ---
enum TTBound { TT_EXACT, TT_LOWER, TT_UPPER, TT_NONE };
struct TTEntry {
    uint64_t hash = 0;
    Move best_move = NULL_MOVE;
    int score = 0;
    int depth = 0;
    TTBound bound = TT_NONE;
};

std::vector<TTEntry> transposition_table;
uint64_t tt_mask = 0;
bool g_tt_is_initialized = false;
int g_configured_tt_size_mb = TT_SIZE_MB_DEFAULT;

void init_tt(size_t mb_size) {
    if (mb_size == 0) { transposition_table.clear(); tt_mask = 0; return; }
    size_t num_entries = (mb_size * 1024 * 1024) / sizeof(TTEntry);
    if (num_entries == 0) { transposition_table.clear(); tt_mask = 0; return; }
    size_t power_of_2_entries = 1;
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries) {
        power_of_2_entries *= 2;
    }
     if (power_of_2_entries == 0) { transposition_table.clear(); tt_mask = 0; return; }
    try {
        transposition_table.assign(power_of_2_entries, TTEntry());
        tt_mask = power_of_2_entries - 1;
    } catch (const std::bad_alloc&) {
        transposition_table.clear(); tt_mask = 0;
    }
}

void clear_tt() {
    if (!transposition_table.empty()) {
        std::fill(transposition_table.begin(), transposition_table.end(), TTEntry());
    }
}

bool probe_tt(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move_from_tt, int& score_from_tt) {
    if (tt_mask == 0 || !g_tt_is_initialized) return false;
    TTEntry& entry = transposition_table[hash & tt_mask];

    if (entry.hash == hash && entry.bound != TT_NONE) {
        move_from_tt = entry.best_move;
        if (entry.depth >= depth) {
            int stored_score = entry.score;
            if (stored_score > MATE_THRESHOLD) stored_score -= ply;
            else if (stored_score < -MATE_THRESHOLD) stored_score += ply;

            score_from_tt = stored_score;
            if (entry.bound == TT_EXACT) return true;
            if (entry.bound == TT_LOWER && stored_score >= beta) return true;
            if (entry.bound == TT_UPPER && stored_score <= alpha) return true;
        }
    }
    return false;
}

void store_tt(uint64_t hash, int depth, int ply, int score, TTBound bound, const Move& best_move) {
    if (tt_mask == 0 || !g_tt_is_initialized) return;
    TTEntry& entry = transposition_table[hash & tt_mask];

    if (score > MATE_THRESHOLD) score += ply;
    else if (score < -MATE_THRESHOLD) score -= ply;

    bool should_replace = (entry.hash == 0) || (entry.hash != hash) ||
                          (depth > entry.depth) ||
                          (depth == entry.depth && bound == TT_EXACT && entry.bound != TT_EXACT) ||
                          (depth == entry.depth && entry.bound == TT_NONE);

    if (should_replace) {
        entry.hash = hash;
        entry.depth = depth;
        entry.score = score;
        entry.bound = bound;
        if (!best_move.is_null() || entry.hash != hash || bound == TT_EXACT || bound == TT_LOWER) {
             entry.best_move = best_move;
        }
    }
}

// --- Search Data & Implementation ---
std::chrono::steady_clock::time_point search_start_timepoint;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;
std::chrono::steady_clock::time_point soft_limit_timepoint;
std::chrono::steady_clock::time_point hard_limit_timepoint;
bool use_time_limits = false;
Move killer_moves[MAX_PLY][2];
int move_history_score[2][64][64];
constexpr int MAX_HISTORY_SCORE = 24000;
uint64_t game_history_hashes[256];
int game_history_length = 0;
uint64_t search_path_hashes[MAX_PLY];

void reset_search_state() {
    nodes_searched = 0;
    stop_search_flag = false;
    use_time_limits = false;
}

void reset_killers_and_history() {
    for(int i=0; i<MAX_PLY; ++i) {
        killer_moves[i][0] = NULL_MOVE;
        killer_moves[i][1] = NULL_MOVE;
    }
    std::memset(move_history_score, 0, sizeof(move_history_score));
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) {
        if (use_time_limits) {
            if (std::chrono::steady_clock::now() > hard_limit_timepoint) {
                stop_search_flag = true;
                return true;
            }
        }
    }
    return false;
}

const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0};

void score_moves(const Position& pos, Move* moves, int num_moves, const Move& tt_move, int ply) {
    for (int i = 0; i < num_moves; ++i) {
        Move& m = moves[i];
        if (!tt_move.is_null() && m == tt_move) {
            m.score = 2000000;
        } else {
            Piece moved_piece = pos.piece_on_sq(m.from);
            Piece captured_piece = pos.piece_on_sq(m.to);

            if (captured_piece != NO_PIECE) {
                m.score = 1000000 + (mvv_lva_piece_values[captured_piece] * 100) - mvv_lva_piece_values[moved_piece];
            } else if (m.promotion != NO_PIECE) {
                 m.score = 900000 + mvv_lva_piece_values[m.promotion];
            } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
                m.score = 800000;
            } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
                m.score = 700000;
            } else {
                m.score = move_history_score[pos.side_to_move][m.from][m.to];
            }
        }
    }
    std::sort(moves, moves + num_moves, [](const Move& a, const Move& b){ return a.score > b.score; });
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return evaluate(pos);

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    int stand_pat_score;

    if (in_check) {
        stand_pat_score = -INF_SCORE + ply;
    } else {
        stand_pat_score = evaluate(pos);
        if (stand_pat_score >= beta) return beta;
        if (alpha < stand_pat_score) alpha = stand_pat_score;
    }

    Move q_moves[256];
    int num_q_moves = generate_moves(pos, q_moves, !in_check);

    Move dummy_tt_move = NULL_MOVE;
    score_moves(pos, q_moves, num_q_moves, dummy_tt_move, ply);

    int legal_moves_in_qsearch = 0;
    for (int i = 0; i < num_q_moves; ++i) {
        const Move& cap_move = q_moves[i];
        bool legal;
        Position next_pos = make_move(pos, cap_move, legal);
        if (!legal) continue;
        legal_moves_in_qsearch++;

        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);

        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }

    if (in_check && legal_moves_in_qsearch == 0) {
        return -MATE_SCORE + ply;
    }

    return alpha;
}

int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move) {
    search_path_hashes[ply] = pos.zobrist_hash;

    nodes_searched++;
    if (check_time()) return 0;
    if (ply >= MAX_PLY -1) return evaluate(pos);
    
    if (pos.halfmove_clock >= 100 && ply > 0) return 0;
    if (ply > 0 && is_insufficient_material(pos)) return 0;

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth++;

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score;
    }

    if (!is_pv_node && !in_check) {
        int static_eval = evaluate(pos);
        if (depth < 8) {
            int futility_margin = 90 + 60 * depth;
            if (static_eval - futility_margin >= beta) {
                return beta;
            }
        }
        if (depth < 4) {
            int razoring_margin = 250 + 80 * (depth-1);
            if (static_eval + razoring_margin < alpha) {
                int q_score = quiescence_search(pos, alpha, beta, ply);
                if (q_score < alpha) {
                    return alpha;
                }
            }
        }
    }

    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 &&
        evaluate(pos) >= beta) {
            Position null_next_pos = pos;
            null_next_pos.side_to_move = 1 - pos.side_to_move;
            null_next_pos.zobrist_hash = pos.zobrist_hash;
            if (pos.ep_square != -1) null_next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];
            null_next_pos.ep_square = -1;
            if (null_next_pos.ep_square != -1) null_next_pos.zobrist_hash ^= zobrist_ep[null_next_pos.ep_square];
            null_next_pos.zobrist_hash ^= zobrist_side_to_move;
            null_next_pos.ply = pos.ply + 1;

            int R_nmp = (depth > 6) ? 3 : 2;
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false);
            
            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 if (null_score >= MATE_THRESHOLD) null_score = beta;
                 store_tt(pos.zobrist_hash, depth, ply, null_score, TT_LOWER, NULL_MOVE);
                 return beta;
            }
    }

    Move moves[256];
    int num_moves = generate_moves(pos, moves, false);
    score_moves(pos, moves, num_moves, tt_move, ply);

    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;

    for (int i = 0; i < num_moves; ++i) {
        const Move& current_move = moves[i];
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        bool is_repetition = false;
        for (int k = ply - 1; k >= 0; k -= 2) {
            if (search_path_hashes[k] == next_pos.zobrist_hash) {
                is_repetition = true;
                break;
            }
        }
        if (!is_repetition) {
            for (int k = 0; k < game_history_length; ++k) {
                if (game_history_hashes[k] == next_pos.zobrist_hash) {
                    is_repetition = true;
                    break;
                }
            }
        }

        if (is_repetition) {
            score = 0;
        } else {
            if (legal_moves_played == 1) {
                score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true);
            } else {
                int R_lmr = 0;
                if (depth >= 3 && i >= 2 && !in_check && current_move.promotion == NO_PIECE &&
                    pos.piece_on_sq(current_move.to) == NO_PIECE) {
                    R_lmr = 1;
                    if (depth >= 5) R_lmr++;
                    if (i >= 6) R_lmr++;
                    R_lmr = std::min(R_lmr, depth - 2);
                    R_lmr = std::max(R_lmr, 0);
                }

                score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true);
                if (score > alpha && R_lmr > 0) {
                     score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true);
                }
                if (score > alpha && score < beta) {
                     score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true);
                }
            }
        }

        if (stop_search_flag) return 0;

        if (score > best_score) {
            best_score = score;
            best_move_found = current_move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) {
                    if (ply < MAX_PLY && pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) {
                        if (!(current_move == killer_moves[ply][0])) {
                            killer_moves[ply][1] = killer_moves[ply][0];
                            killer_moves[ply][0] = current_move;
                        }
                        int bonus = depth * depth;
                        int& good_hist = move_history_score[pos.side_to_move][current_move.from][current_move.to];
                        good_hist += bonus - (good_hist * std::abs(bonus) / MAX_HISTORY_SCORE);

                        for (int j = 0; j < i; ++j) {
                            const Move& bad_move = moves[j];
                            if (pos.piece_on_sq(bad_move.to) == NO_PIECE && bad_move.promotion == NO_PIECE) {
                                int& bad_hist = move_history_score[pos.side_to_move][bad_move.from][bad_move.to];
                                bad_hist -= bonus - (bad_hist * std::abs(bonus) / MAX_HISTORY_SCORE);
                            }
                        }
                    }
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found);
                    return beta;
                }
            }
        }
    }

    if (legal_moves_played == 0) {
        return in_check ? (-MATE_SCORE + ply) : 0;
    }

    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, ply, best_score, final_bound_type, best_move_found);
    return best_score;
}

// --- UCI Data & Implementation ---
Position uci_root_pos;
Move uci_best_move_overall;

void parse_fen(Position& pos, const std::string& fen_str) {
    pos = Position();
    std::stringstream ss(fen_str);
    std::string part;

    ss >> part;
    int rank = 7, file = 0;
    for (char c : part) {
        if (std::isdigit(c)) {
            file += (c - '0');
        } else if (c == '/') {
            rank--; file = 0;
        } else {
            Piece p_type = NO_PIECE; Color p_color = NO_COLOR;
            if (std::islower(c)) p_color = BLACK; else p_color = WHITE;
            char lower_c = std::tolower(c);
            if (lower_c == 'p') p_type = PAWN; else if (lower_c == 'n') p_type = KNIGHT;
            else if (lower_c == 'b') p_type = BISHOP; else if (lower_c == 'r') p_type = ROOK;
            else if (lower_c == 'q') p_type = QUEEN; else if (lower_c == 'k') p_type = KING;

            if (p_type != NO_PIECE && rank >=0 && rank < 8 && file >=0 && file < 8) {
                int sq = rank * 8 + file;
                pos.piece_bb[p_type] |= set_bit(sq);
                pos.color_bb[p_color] |= set_bit(sq);
            }
            file++;
        }
    }

    ss >> part; pos.side_to_move = (part == "w") ? WHITE : BLACK;
    ss >> part; pos.castling_rights = 0;
    for (char c : part) {
        if (c == 'K') pos.castling_rights |= WK_CASTLE_MASK;
        else if (c == 'Q') pos.castling_rights |= WQ_CASTLE_MASK;
        else if (c == 'k') pos.castling_rights |= BK_CASTLE_MASK;
        else if (c == 'q') pos.castling_rights |= BQ_CASTLE_MASK;
    }

    ss >> part;
    if (part != "-") {
        if (part.length() == 2 && part[0] >= 'a' && part[0] <= 'h' && part[1] >= '1' && part[1] <= '8') {
            int ep_file = part[0] - 'a';
            int ep_rank = part[1] - '1';
            pos.ep_square = ep_rank * 8 + ep_file;
        } else pos.ep_square = -1;
    } else pos.ep_square = -1;

    if (ss >> part) { try {pos.halfmove_clock = std::stoi(part);} catch(...) {pos.halfmove_clock = 0;} }
    else pos.halfmove_clock = 0;
    if (ss >> part) { try {pos.fullmove_number = std::stoi(part);} catch(...){pos.fullmove_number = 1;} }
    else pos.fullmove_number = 1;
    pos.ply = 0;
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_length = 0;
}

Move parse_uci_move_from_string(const Position& current_pos, const std::string& uci_move_str) {
    Move m = NULL_MOVE;
    if (uci_move_str.length() < 4 || uci_move_str.length() > 5 || uci_move_str == "0000") return m;
    m.from = (uci_move_str[0] - 'a') + (uci_move_str[1] - '1') * 8;
    m.to = (uci_move_str[2] - 'a') + (uci_move_str[3] - '1') * 8;
    if (m.from < 0 || m.from > 63 || m.to < 0 || m.to > 63) return NULL_MOVE;
    if (uci_move_str.length() == 5) {
        char promo_char = uci_move_str[4];
        if (promo_char == 'q') m.promotion = QUEEN;
        else if (promo_char == 'n') m.promotion = KNIGHT;
        else if (promo_char == 'b') m.promotion = BISHOP;
        else if (promo_char == 'r') m.promotion = ROOK;
        else return NULL_MOVE;
    }
    return m;
}

uint64_t calculate_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c) {
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[c];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                h ^= zobrist_pieces[c][p][sq];
            }
        }
    }
    h ^= zobrist_castling[pos.castling_rights];
    h ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    if (pos.side_to_move == BLACK) h ^= zobrist_side_to_move;
    return h;
}

void uci_loop() {
    std::string line, token;
    parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    while (std::getline(std::cin, line)) {
        std::istringstream ss(line);
        ss >> token;

        if (token == "uci") {
            std::cout << "id name Amira 1.2\n";
            std::cout << "id author ChessTubeTree\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 1024\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str_val;
            ss >> name_token; // "name"
            if (ss >> name_str && name_str == "Hash" && ss >> value_token /* "value" */ && ss >> value_str_val) {
                try { g_configured_tt_size_mb = std::max(0, std::min(std::stoi(value_str_val), 1024)); }
                catch (...) { }
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }
            else { clear_tt(); }
            reset_killers_and_history();
            game_history_length = 0;
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                ss >> token;
            } else if (token == "fen") {
                while (ss >> token && token != "moves") fen_str_collector += token + " ";
                if (!fen_str_collector.empty()) fen_str_collector.pop_back();
                parse_fen(uci_root_pos, fen_str_collector);
            }

            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null()) break;
                    if (game_history_length < 256) game_history_hashes[game_history_length++] = uci_root_pos.zobrist_hash;
                    if (uci_root_pos.piece_on_sq(m.from) == PAWN || uci_root_pos.piece_on_sq(m.to) != NO_PIECE) game_history_length = 0;
                    bool legal; uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }

            long long wtime = -1, btime = -1;
            int max_depth_to_search = MAX_PLY;
            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime;
                else if (go_param == "btime") ss >> btime;
                else if (go_param == "depth") ss >> max_depth_to_search;
            }

            reset_search_state();
            search_start_timepoint = std::chrono::steady_clock::now();
            long long time_alotment_ms = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;

            if (time_alotment_ms != -1) {
                use_time_limits = true;
                double soft_limit_s = time_alotment_ms * 0.000052;
                double hard_limit_s = time_alotment_ms * 0.00041;
                soft_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(soft_limit_s * 1000000.0));
                hard_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(hard_limit_s * 1000000.0));
            } else use_time_limits = false;

            uci_best_move_overall = NULL_MOVE;
            int best_score_overall = 0;
            int aspiration_alpha = -INF_SCORE, aspiration_beta = INF_SCORE;
            int aspiration_window_delta = 25;

            for (int depth = 1; depth <= max_depth_to_search; ++depth) {
                int current_score;
                if (depth <= 1) {
                     current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true);
                } else {
                    current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true);
                    if (!stop_search_flag && (current_score <= aspiration_alpha || current_score >= aspiration_beta)) {
                        aspiration_alpha = -INF_SCORE; aspiration_beta = INF_SCORE;
                        current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true);
                    }
                }

                if (stop_search_flag && depth > 1) break;

                if (std::abs(current_score) < MATE_THRESHOLD) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 3 + 5;
                    if (aspiration_window_delta > 300) aspiration_window_delta = 300;
                } else {
                    aspiration_alpha = -INF_SCORE; aspiration_beta = INF_SCORE;
                }

                Move tt_root_move = NULL_MOVE; int tt_root_score; int dummy_alpha = -INF_SCORE, dummy_beta = INF_SCORE;
                if (probe_tt(uci_root_pos.zobrist_hash, depth, 0, dummy_alpha, dummy_beta, tt_root_move, tt_root_score)) {
                     if (!tt_root_move.is_null()) uci_best_move_overall = tt_root_move;
                } else {
                     TTEntry& root_entry_check = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                     if (root_entry_check.hash == uci_root_pos.zobrist_hash && !root_entry_check.best_move.is_null()) uci_best_move_overall = root_entry_check.best_move;
                }
                best_score_overall = current_score;

                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - search_start_timepoint).count();
                std::cout << "info depth " << depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) std::cout << " mate " << (MATE_SCORE - best_score_overall + 1)/2 ;
                else if (best_score_overall < -MATE_THRESHOLD) std::cout << " mate " << -(MATE_SCORE + best_score_overall +1)/2;
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);

                if (!uci_best_move_overall.is_null()) {
                    std::cout << " pv";
                    Position temp_pos = uci_root_pos;
                    for (int pv_idx = 0; pv_idx < depth; ++pv_idx) {
                        Move pv_m = NULL_MOVE; int pv_s; int pv_a = -INF_SCORE, pv_b = INF_SCORE;
                        if (probe_tt(temp_pos.zobrist_hash, 1, 0, pv_a, pv_b, pv_m, pv_s) && !pv_m.is_null()) {
                            bool legal_pv;
                            Position next_temp_pos = make_move(temp_pos, pv_m, legal_pv);
                            if (legal_pv) { std::cout << " " << move_to_uci(pv_m); temp_pos = next_temp_pos; }
                            else { break; }
                        } else { break; }
                    }
                }
                std::cout << std::endl;

                if (use_time_limits && std::chrono::steady_clock::now() > soft_limit_timepoint) break;
                if (std::abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break;
            }

            if (!uci_best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            } else {
                 Move moves[256]; int n = generate_moves(uci_root_pos, moves, false);
                 for (int i=0; i<n; ++i) { bool l; make_move(uci_root_pos, moves[i], l); if(l){std::cout << "bestmove " << move_to_uci(moves[i]) << std::endl; goto end_go; }}
                 std::cout << "bestmove 0000" << std::endl;
            }
            end_go:;

        } else if (token == "quit" || token == "stop") {
            stop_search_flag = true;
            if (token == "quit") break;
        }
    }
}

// --- Main Program Entry Point (for UCI mode) ---
#ifndef PERFT_BUILD

int main(int argc, char* argv[]) {
    // Correct square constants for piece moves
    constexpr int B1_SQ = 1;
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    init_zobrist();
    init_attack_tables();
    init_eval_masks();
    reset_killers_and_history();

    uci_loop();
    return 0;
}

#endif // PERFT_BUILD
