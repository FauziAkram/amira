#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <chrono>
#include <algorithm>
#include <cstring> // For std::memset
#include <cstdint>
#include <random>   // For std::mt19937_64
#include <cctype>   // For std::isdigit, std::islower, std::tolower

// Bit manipulation builtins (MSVC/GCC specific)
#if defined(_MSC_VER)
#include <intrin.h>
#endif

// --- Constants ---
enum Piece { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE };
enum Color { WHITE, BLACK, NO_COLOR };

constexpr int MAX_PLY = 64;
constexpr int TT_SIZE_MB_DEFAULT = 8;

constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY;
constexpr int INF_SCORE = 32000;

// Castling rights masks
constexpr uint8_t WK_CASTLE_MASK = 1;
constexpr uint8_t WQ_CASTLE_MASK = 2;
constexpr uint8_t BK_CASTLE_MASK = 4;
constexpr uint8_t BQ_CASTLE_MASK = 8;

// Rook and King starting squares (standard chess)
constexpr int A1_SQ = 0; constexpr int E1_SQ = 4; constexpr int H1_SQ = 7;
constexpr int G1_SQ = 6; constexpr int C1_SQ = 2; // White castled king squares
constexpr int A8_SQ = 56; constexpr int E8_SQ = 60; constexpr int H8_SQ = 63;
constexpr int G8_SQ = 62; constexpr int C8_SQ = 58; // Black castled king squares

// King safety penalty tables (index is king_attack_score, capped)
const int king_danger_penalty_mg[15] = {0, 0, 5, 15, 30, 50, 75, 100, 130, 160, 200, 240, 280, 320, 350};
const int king_danger_penalty_eg[15] = {0, 0, 0,  5, 10, 15,  20,  30,  40,  50,  60,  70,  80,  90, 100};

// Mobility bonuses per piece type [P,N,B,R,Q,K (unused)]
const int mobility_bonus_mg[6] = {0, 2, 2, 1, 1, 0}; // Smaller bonuses, PSTs do heavy lifting
const int mobility_bonus_eg[6] = {0, 3, 3, 2, 2, 0};

// Search Parameter Constants
constexpr int IID_MIN_DEPTH = 4;
constexpr int IID_REDUCTION = 2; // Search depth D-R_iid

constexpr int FUTILITY_PRUNING_MAX_DEPTH = 2; // Apply FP at depth 1 and 2
const int futility_margins[FUTILITY_PRUNING_MAX_DEPTH + 1] = {0, 200, 450}; // Margins for depth 1, 2

constexpr int DELTA_MARGIN = 50; // For Delta Pruning in Q-search

// Forward Declarations
struct Move;
struct Position;
int evaluate(const Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only = false);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);
int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, std::vector<uint64_t>& current_search_path_hashes, const Move& prev_opponent_move);


// --- Zobrist Hashing ---
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

// --- Move Structure ---
struct Move {
    int from = 0, to = 0;
    Piece promotion = NO_PIECE;
    int score = 0;

    bool operator==(const Move& other) const {
        return from == other.from && to == other.to && promotion == other.promotion;
    }
    bool is_null() const { return from == 0 && to == 0 && promotion == NO_PIECE; }
};

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
inline uint64_t set_bit(int sq) { return 1ULL << sq; }
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


// --- Board Representation ---
struct Position {
    uint64_t piece_bb[6];
    uint64_t color_bb[2];
    int side_to_move;
    int ep_square;
    uint8_t castling_rights;
    uint64_t zobrist_hash;
    int halfmove_clock;
    int fullmove_number;
    int ply;

    Position() {
        std::memset(this, 0, sizeof(Position));
        side_to_move = WHITE;
        ep_square = -1;
        fullmove_number = 1;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }
    uint64_t get_empty_bb() const { return ~get_occupied_bb(); }
    uint64_t get_friendly_bb(Color c) const { return color_bb[c]; }
    uint64_t get_enemy_bb(Color c) const { return color_bb[1-c]; }


    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE;
        uint64_t b = set_bit(sq);
        if (!( (color_bb[WHITE] | color_bb[BLACK]) & b)) return NO_PIECE;
        for (int p = PAWN; p <= KING; ++p) {
            if (piece_bb[p] & b) return (Piece)p;
        }
        return NO_PIECE;
    }
    Color color_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_COLOR;
        uint64_t b = set_bit(sq);
        if (color_bb[WHITE] & b) return WHITE;
        if (color_bb[BLACK] & b) return BLACK;
        return NO_COLOR;
    }
};

// --- Attack Tables Init ---
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
            ((b >> 10) & ~0xC0C0C0C0C0C0C0C0ULL) | ((b >> 6)  & ~0x0303030303030303ULL)
        );

        king_attacks_bb[sq] = north(b) | south(b) | east(b) | west(b) |
                              ne(b) | nw(b) | se(b) | sw(b);
    }
}

// --- Slider Attack Generation ---
uint64_t get_rook_attacks_from_sq(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {1, -1, 8, -8};
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (abs(d) == 1 && r_curr != r_prev) break;
            if (abs(d) == 8 && c_curr != c_prev) break;

            attacks |= set_bit(s);
            if (get_bit(occupied, s)) break;
        }
    }
    return attacks;
}

uint64_t get_bishop_attacks_from_sq(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {9, -9, 7, -7};
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (abs(r_curr - r_prev) != 1 || abs(c_curr - c_prev) != 1) break;

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


// --- is_square_attacked ---
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

// --- Move Generation ---
void add_move_to_list(std::vector<Move>& moves_list, int from, int to, Piece promotion = NO_PIECE, int score = 0) {
    moves_list.push_back({from, to, promotion, score});
}

void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only) {
    moves_list.clear();
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
        int promotion_rank_idx = (stm == WHITE) ? 6 : 1;

        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >=0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (rank == promotion_rank_idx) {
                add_move_to_list(moves_list, from, one_step_sq, QUEEN); add_move_to_list(moves_list, from, one_step_sq, ROOK);
                add_move_to_list(moves_list, from, one_step_sq, BISHOP); add_move_to_list(moves_list, from, one_step_sq, KNIGHT);
            } else if (!captures_only) {
                add_move_to_list(moves_list, from, one_step_sq);
            }
            if (!captures_only) {
                int start_rank_idx = (stm == WHITE) ? 1 : 6;
                if (rank == start_rank_idx) {
                    int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                    if (two_steps_sq >=0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq)) {
                        add_move_to_list(moves_list, from, two_steps_sq);
                    }
                }
            }
        }
        uint64_t pawn_cap_targets = pawn_attacks_bb[stm][from] & opp_pieces;
        if (pos.ep_square != -1) {
             if (get_bit(pawn_attacks_bb[stm][from], pos.ep_square)) {
                 pawn_cap_targets |= set_bit(pos.ep_square);
             }
        }
        while (pawn_cap_targets) {
            int to = lsb_index(pawn_cap_targets);
            pawn_cap_targets &= pawn_cap_targets - 1;
            if (rank == promotion_rank_idx) {
                add_move_to_list(moves_list, from, to, QUEEN); add_move_to_list(moves_list, from, to, ROOK);
                add_move_to_list(moves_list, from, to, BISHOP); add_move_to_list(moves_list, from, to, KNIGHT);
            } else {
                add_move_to_list(moves_list, from, to);
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

            attacks &= (captures_only ? opp_pieces : ~my_pieces);

            while (attacks) {
                int to = lsb_index(attacks);
                attacks &= attacks - 1;
                add_move_to_list(moves_list, from, to);
            }
        }
    }

    if (!captures_only) {
        int king_sq_idx = lsb_index(pos.piece_bb[KING] & my_pieces);
        if (king_sq_idx != -1) {
            if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ + 1) && !get_bit(occupied, E1_SQ + 2) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ + 1, BLACK) && !is_square_attacked(pos, E1_SQ + 2, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, E1_SQ + 2);
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ - 1) && !get_bit(occupied, E1_SQ - 2) && !get_bit(occupied, E1_SQ - 3) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ - 1, BLACK) && !is_square_attacked(pos, E1_SQ - 2, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, E1_SQ - 2);
                }
            } else {
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ + 1) && !get_bit(occupied, E8_SQ + 2) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ + 1, WHITE) && !is_square_attacked(pos, E8_SQ + 2, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, E8_SQ + 2);
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ - 1) && !get_bit(occupied, E8_SQ - 2) && !get_bit(occupied, E8_SQ - 3) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ - 1, WHITE) && !is_square_attacked(pos, E8_SQ - 2, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, E8_SQ - 2);
                }
            }
        }
    }
}

// --- Make Move ---
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
    next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.from];

    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;

    next_pos.halfmove_clock++;

    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        next_pos.halfmove_clock = 0;
    }
    if (piece_moved == PAWN) next_pos.halfmove_clock = 0;

    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) {
        int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
        next_pos.piece_bb[PAWN] &= ~set_bit(captured_pawn_sq);
        next_pos.color_bb[opp] &= ~set_bit(captured_pawn_sq);
        next_pos.zobrist_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
    }

    next_pos.zobrist_hash ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    next_pos.ep_square = -1;
    if (piece_moved == PAWN && abs(move.to - move.from) == 16) {
        next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    }
    next_pos.zobrist_hash ^= zobrist_ep[(next_pos.ep_square == -1) ? 64 : next_pos.ep_square];

    if (move.promotion != NO_PIECE) {
        if (piece_moved != PAWN) return pos;
        int promotion_rank_actual = (stm == WHITE) ? 7 : 0;
        if (move.to / 8 != promotion_rank_actual) return pos;

        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else {
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    uint8_t old_castling_rights = pos.castling_rights;
    uint8_t new_castling_rights = pos.castling_rights;

    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);

        if (abs(move.to - move.from) == 2) {
            int rook_from_sq, rook_to_sq;
            if (move.to == E1_SQ + 2) { rook_from_sq = H1_SQ; rook_to_sq = E1_SQ + 1; }
            else if (move.to == E1_SQ - 2) { rook_from_sq = A1_SQ; rook_to_sq = E1_SQ - 1; }
            else if (move.to == E8_SQ + 2) { rook_from_sq = H8_SQ; rook_to_sq = E8_SQ + 1; }
            else { rook_from_sq = A8_SQ; rook_to_sq = E8_SQ - 1; }

            next_pos.piece_bb[ROOK] &= ~set_bit(rook_from_sq);
            next_pos.piece_bb[ROOK] |= set_bit(rook_to_sq);
            next_pos.color_bb[stm] &= ~set_bit(rook_from_sq);
            next_pos.color_bb[stm] |= set_bit(rook_to_sq);
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
        }
    }

    if (piece_moved == ROOK) {
        if (stm == WHITE) {
            if (move.from == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
            else if (move.from == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
        } else {
            if (move.from == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
            else if (move.from == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
        }
    }

    if (piece_captured == ROOK) {
        if (opp == WHITE) {
             if (move.to == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
             else if (move.to == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
        } else {
             if (move.to == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
             else if (move.to == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
        }
    }

    next_pos.castling_rights = new_castling_rights;
    if (old_castling_rights != next_pos.castling_rights) {
        next_pos.zobrist_hash ^= zobrist_castling[old_castling_rights];
        next_pos.zobrist_hash ^= zobrist_castling[next_pos.castling_rights];
    }

    next_pos.side_to_move = opp;
    next_pos.zobrist_hash ^= zobrist_side_to_move;

    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply = pos.ply + 1;

    int king_sq_after_move = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq_after_move == -1) return pos;
    if (is_square_attacked(next_pos, king_sq_after_move, opp)) {
        return pos;
    }

    legal_move_flag = true;
    return next_pos;
}

// --- Evaluation ---
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
const int passed_pawn_bonus_mg[8] = {0, 5, 15, 25, 40, 60, 80, 0};
const int passed_pawn_bonus_eg[8] = {0, 10, 25, 40, 60, 90, 120, 0};

void init_eval_masks() {
    for (int f = 0; f < 8; ++f) {
        file_bb_mask[f] = 0ULL;
        for (int r = 0; r < 8; ++r) file_bb_mask[f] |= set_bit(r * 8 + f);
    }
    for (int r = 0; r < 8; ++r) {
        rank_bb_mask[r] = 0ULL;
        for (int f = 0; f < 8; ++f) rank_bb_mask[r] |= set_bit(r * 8 + f);
    }
    for (int sq = 0; sq < 64; ++sq) {
        white_passed_pawn_block_mask[sq] = 0ULL;
        black_passed_pawn_block_mask[sq] = 0ULL;
        int r = sq / 8; int f = sq % 8;
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

int evaluate(const Position& pos) {
    int mg_score = 0;
    int eg_score = 0;
    int game_phase = 0;

    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;

        uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[current_eval_color];
        uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1 - current_eval_color];
        uint64_t friendly_pieces_bb = pos.color_bb[current_eval_color];
        uint64_t occupied_bb = pos.get_occupied_bb();


        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            int piece_mobility_mg = 0;
            int piece_mobility_eg = 0;

            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);

                mg_score += side_multiplier * (piece_values_mg[p] + pst_mg_all[p][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p] + pst_eg_all[p][mirrored_sq]);

                // Mobility
                uint64_t attacks = 0;
                if ((Piece)p == KNIGHT) attacks = knight_attacks_bb[sq];
                else if ((Piece)p == BISHOP) attacks = get_bishop_attacks_from_sq(sq, occupied_bb);
                else if ((Piece)p == ROOK) attacks = get_rook_attacks_from_sq(sq, occupied_bb);
                else if ((Piece)p == QUEEN) attacks = get_rook_attacks_from_sq(sq, occupied_bb) | get_bishop_attacks_from_sq(sq, occupied_bb);
                
                if ((Piece)p != PAWN && (Piece)p != KING) { // Pawns/King mobility handled by PSTs or specific terms
                    attacks &= ~friendly_pieces_bb; // Can't move to own piece
                    int mobility_count = pop_count(attacks);
                    piece_mobility_mg += mobility_bonus_mg[p] * mobility_count;
                    piece_mobility_eg += mobility_bonus_eg[p] * mobility_count;
                }


                if ((Piece)p == PAWN) {
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
                    }
                } else if ((Piece)p == ROOK) {
                    int f = sq % 8;
                    bool friendly_pawn_on_file = (file_bb_mask[f] & all_friendly_pawns) != 0;
                    bool enemy_pawn_on_file = (file_bb_mask[f] & all_enemy_pawns) != 0;
                    if (!friendly_pawn_on_file) {
                        if (!enemy_pawn_on_file) { mg_score += side_multiplier * 20; eg_score += side_multiplier * 15; }
                        else { mg_score += side_multiplier * 10; eg_score += side_multiplier * 5;}
                    }
                }
            }
            mg_score += side_multiplier * piece_mobility_mg;
            eg_score += side_multiplier * piece_mobility_eg;
        }
        if (pop_count(pos.piece_bb[BISHOP] & pos.color_bb[current_eval_color]) >= 2) {
            mg_score += side_multiplier * 30; eg_score += side_multiplier * 50;
        }

        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[current_eval_color]);
        if (king_sq != -1) {
            int king_rank = king_sq / 8;
            int king_file = king_sq % 8;
            int pawn_shield_score = 0;
            const int shield_pawn_bonus = 12;
            const int missing_shield_pawn_penalty = -18;
            const int open_file_penalty_adj = -10;
            int shield_candidate_sqs[3] = {-1, -1, -1};
            bool relevant_castled_pos = false;

            if (current_eval_color == WHITE) {
                if (king_sq == G1_SQ) { shield_candidate_sqs[0] = G1_SQ + 7; shield_candidate_sqs[1] = G1_SQ + 8; shield_candidate_sqs[2] = G1_SQ + 9; relevant_castled_pos = true;}
                else if (king_sq == C1_SQ) { shield_candidate_sqs[0] = C1_SQ + 7; shield_candidate_sqs[1] = C1_SQ + 8; shield_candidate_sqs[2] = C1_SQ + 9; relevant_castled_pos = true;}
            } else {
                if (king_sq == G8_SQ) { shield_candidate_sqs[0] = G8_SQ - 9; shield_candidate_sqs[1] = G8_SQ - 8; shield_candidate_sqs[2] = G8_SQ - 7; relevant_castled_pos = true;}
                else if (king_sq == C8_SQ) { shield_candidate_sqs[0] = C8_SQ - 9; shield_candidate_sqs[1] = C8_SQ - 8; shield_candidate_sqs[2] = C8_SQ - 7; relevant_castled_pos = true;}
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
                            if (!((file_bb_mask[shield_f] & all_friendly_pawns) || (file_bb_mask[shield_f] & all_enemy_pawns))) {
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
                            if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) {
                                pawn_shield_score += shield_pawn_bonus / 2;
                            } else {
                                pawn_shield_score += missing_shield_pawn_penalty / 2;
                                 if (!((file_bb_mask[shield_sq % 8] & all_friendly_pawns) || (file_bb_mask[shield_sq % 8] & all_enemy_pawns))) {
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

            uint64_t enemy_knights = pos.piece_bb[KNIGHT] & enemy_player_pieces;
            while(enemy_knights) { int s = lsb_index(enemy_knights); enemy_knights &= enemy_knights-1; if (knight_attacks_bb[s] & king_1_ring_zone) king_attack_score += 2;}
            uint64_t enemy_bishops = pos.piece_bb[BISHOP] & enemy_player_pieces;
            while(enemy_bishops) { int s = lsb_index(enemy_bishops); enemy_bishops &= enemy_bishops-1; if (get_bishop_attacks_from_sq(s, occupied_bb) & king_1_ring_zone) king_attack_score += 2;}
            uint64_t enemy_rooks = pos.piece_bb[ROOK] & enemy_player_pieces;
            while(enemy_rooks) { int s = lsb_index(enemy_rooks); enemy_rooks &= enemy_rooks-1; if (get_rook_attacks_from_sq(s, occupied_bb) & king_1_ring_zone) king_attack_score += 3;}
            uint64_t enemy_queens = pos.piece_bb[QUEEN] & enemy_player_pieces;
            while(enemy_queens) { int s = lsb_index(enemy_queens); enemy_queens &= enemy_queens-1; if (get_bishop_attacks_from_sq(s, occupied_bb) & king_1_ring_zone) king_attack_score += 4; if (get_rook_attacks_from_sq(s, occupied_bb) & king_1_ring_zone) king_attack_score += 5;}
            uint64_t temp_king_zone_for_pawns = king_1_ring_zone;
            while(temp_king_zone_for_pawns) { int s = lsb_index(temp_king_zone_for_pawns); temp_king_zone_for_pawns &= temp_king_zone_for_pawns-1; if (pawn_attacks_bb[current_eval_color][s] & all_enemy_pawns) king_attack_score += 1;}
            
            int danger_idx = std::min(king_attack_score, 14);
            mg_score -= side_multiplier * king_danger_penalty_mg[danger_idx];
            eg_score -= side_multiplier * king_danger_penalty_eg[danger_idx];
        }
    }

    game_phase = std::min(game_phase, 24); game_phase = std::max(game_phase, 0);
    int final_score_from_white_pov = (mg_score * game_phase + eg_score * (24 - game_phase)) / 24;
    return (pos.side_to_move == WHITE) ? final_score_from_white_pov : -final_score_from_white_pov;
}


// --- Transposition Table ---
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
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries) power_of_2_entries *= 2;
    if (power_of_2_entries == 0) { transposition_table.clear(); tt_mask = 0; return; }
    try {
        transposition_table.assign(power_of_2_entries, TTEntry());
        tt_mask = power_of_2_entries - 1;
    } catch (const std::bad_alloc&) { transposition_table.clear(); tt_mask = 0; }
}

void clear_tt() {
    if (!transposition_table.empty()) std::memset(transposition_table.data(), 0, transposition_table.size() * sizeof(TTEntry));
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
    bool should_replace = (entry.hash == 0) || (entry.hash != hash) || (depth > entry.depth) ||
                          (depth == entry.depth && bound == TT_EXACT && entry.bound != TT_EXACT) ||
                          (depth == entry.depth && entry.bound == TT_NONE);
    if (should_replace) {
        entry.hash = hash; entry.depth = depth; entry.score = score; entry.bound = bound;
        if (!best_move.is_null() || entry.hash != hash || bound == TT_EXACT || bound == TT_LOWER) {
             entry.best_move = best_move;
        }
    }
}

// --- Search Heuristics & State ---
std::chrono::steady_clock::time_point search_start_timepoint;
long long search_budget_ms = 0;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;

Move killer_moves[MAX_PLY][2];
int history_heuristic[2][64][64];
std::vector<uint64_t> game_history_hashes;
Move countermoves[Piece::NO_PIECE][64]; // [opponent_last_moved_piece_type][opponent_last_move_to_sq]

void reset_search_state() {
    nodes_searched = 0;
    stop_search_flag = false;
}

void reset_killers_and_history() {
    for(int i=0; i<MAX_PLY; ++i) {
        killer_moves[i][0] = NULL_MOVE;
        killer_moves[i][1] = NULL_MOVE;
    }
    std::memset(history_heuristic, 0, sizeof(history_heuristic));
    std::memset(countermoves, 0, sizeof(countermoves)); // Reset countermoves to NULL_MOVE equivalent
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) {
        if (search_budget_ms > 0) {
            auto now = std::chrono::steady_clock::now();
            auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now - search_start_timepoint).count();
            if (elapsed >= search_budget_ms) {
                stop_search_flag = true;
                return true;
            }
        }
    }
    return false;
}

const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0}; // P,N,B,R,Q,K,NO_PIECE

void score_moves(const Position& pos, std::vector<Move>& moves, const Move& tt_move, int ply, const Move& prev_opponent_move) {
    for (Move& m : moves) {
        if (!tt_move.is_null() && m == tt_move) {
            m.score = 2000000; // TT move
        } else {
            Piece moved_piece = pos.piece_on_sq(m.from);
            Piece captured_piece = pos.piece_on_sq(m.to);

            if (captured_piece != NO_PIECE) { // MVV-LVA
                m.score = 1000000 + (mvv_lva_piece_values[captured_piece] * 100) - mvv_lva_piece_values[moved_piece];
            } else if (m.promotion != NO_PIECE) {
                 m.score = 900000 + mvv_lva_piece_values[m.promotion]; // Promotions
            } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
                m.score = 800000; // Killer 1
            } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
                m.score = 700000; // Killer 2
            } else if (!prev_opponent_move.is_null()) { // Countermove Heuristic
                Piece opp_moved_piece_type = pos.piece_on_sq(prev_opponent_move.from); // Note: this is piece on *current* board,
                                                                                        // which might be wrong if prev_opp_move was a capture.
                                                                                        // This needs care. For now, assume prev_opp_move has correct type info
                                                                                        // or we infer it correctly.
                                                                                        // A safer way is to pass Piece type directly if possible.
                                                                                        // For now, this is a simplification. A more robust solution would
                                                                                        // ensure the Piece type is from the state *before* opponent's move.
                                                                                        // However, the killer/history will likely dominate if this is off.
                                                                                        // Let's assume prev_opponent_move 'from' and 'to' are correct.
                                                                                        // The piece type on 'from' of prev_opponent_move is what we need.
                                                                                        // This is complex to get from `pos` *after* opponent moved.
                                                                                        // The simplest is to use `countermoves[NO_PIECE][prev_opponent_move.to]`
                                                                                        // or pass the piece type of opponent's move.
                                                                                        // For this iteration, I'll use a simplified key for countermoves.
                if (m == countermoves[NO_PIECE][prev_opponent_move.to]) { // Simplified key: just opponent's to-square
                    m.score = 650000; // Countermove score
                } else {
                    m.score = history_heuristic[pos.side_to_move][m.from][m.to]; // History
                }
            }
             else {
                m.score = history_heuristic[pos.side_to_move][m.from][m.to]; // History
            }
        }
    }
    std::sort(moves.begin(), moves.end(), [](const Move& a, const Move& b){ return a.score > b.score; });
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

    std::vector<Move> q_moves;
    generate_moves(pos, q_moves, !in_check);

    Move dummy_tt_move = NULL_MOVE; Move dummy_prev_opp_move = NULL_MOVE;
    score_moves(pos, q_moves, dummy_tt_move, ply, dummy_prev_opp_move); // Use simplified score_moves for qsearch

    int legal_moves_in_qsearch = 0;
    for (const Move& cap_move : q_moves) {
        bool legal;
        Position next_pos = make_move(pos, cap_move, legal);
        if (!legal) continue;
        legal_moves_in_qsearch++;

        // Delta Pruning for captures
        if (!in_check && pos.piece_on_sq(cap_move.to) != NO_PIECE) { // Is a capture
            Piece captured_val_piece = pos.piece_on_sq(cap_move.to); // Piece type on target square
            if (stand_pat_score + piece_values_mg[captured_val_piece] + DELTA_MARGIN < alpha) {
                continue; // Prune this capture
            }
        }


        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);

        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }

    if (in_check && legal_moves_in_qsearch == 0) {
        return -MATE_SCORE + ply;
    }
    return alpha;
}


int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, std::vector<uint64_t>& current_search_path_hashes, const Move& prev_opponent_move) {
    nodes_searched++;
    if (check_time()) return 0;
    if (ply >= MAX_PLY -1) return evaluate(pos);

    for (uint64_t path_hash : current_search_path_hashes) if (path_hash == pos.zobrist_hash) return 0;
    int game_reps = 0;
    for(uint64_t hist_hash : game_history_hashes) if(hist_hash == pos.zobrist_hash) game_reps++;
    if(game_reps >= 2 && ply > 0) return 0;
    if (pos.halfmove_clock >= 100 && ply > 0) return 0;

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth = std::max(depth + 1, depth);

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    // Futility Pruning
    if (!is_pv_node && !in_check && depth <= FUTILITY_PRUNING_MAX_DEPTH) {
        int static_eval = evaluate(pos);
        if (static_eval + futility_margins[depth] <= alpha) {
            // If we are this bad off, just do qsearch to verify no tactical surprise.
            // A stronger version might return alpha or static_eval directly for non-captures.
            // For safety, we fall through to qsearch if all moves are quiet and pruned.
            // The actual pruning of quiet moves happens in the loop below.
        }
    }


    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score;
    }

    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 &&
        evaluate(pos) >= beta) { // NMP
            Position null_next_pos = pos;
            null_next_pos.side_to_move = 1 - pos.side_to_move;
            null_next_pos.zobrist_hash = pos.zobrist_hash;
            if (pos.ep_square != -1) null_next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];
            null_next_pos.ep_square = -1;
            null_next_pos.zobrist_hash ^= zobrist_ep[64];
            null_next_pos.zobrist_hash ^= zobrist_side_to_move;
            null_next_pos.ply = pos.ply + 1;
            int R_nmp = (depth > 6) ? 3 : 2;
            current_search_path_hashes.push_back(pos.zobrist_hash);
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false, current_search_path_hashes, NULL_MOVE /*no prev_opp_move for null*/);
            current_search_path_hashes.pop_back();
            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 if (null_score >= MATE_THRESHOLD) null_score = beta;
                 store_tt(pos.zobrist_hash, depth, ply, null_score, TT_LOWER, NULL_MOVE);
                 return beta;
            }
    }

    std::vector<Move> moves;
    generate_moves(pos, moves);
    score_moves(pos, moves, tt_move, ply, prev_opponent_move);

    // Internal Iterative Deepening (Simplified: only for TT move or first move)
    if (depth >= IID_MIN_DEPTH && !is_pv_node && !tt_move.is_null() && moves.size() > 0 ) {
        // Check if tt_move is actually in the legal moves list (it should be if TT is consistent)
        bool tt_move_is_legal_and_first = false;
        if (moves[0] == tt_move) tt_move_is_legal_and_first = true;
        // A more robust IID would search its found move even if not TT.
        // For this version, we'll only IID the TT move if it's the best according to scoring.
        
        if (tt_move_is_legal_and_first) {
            bool legal_iid;
            Position iid_next_pos = make_move(pos, moves[0], legal_iid);
            if (legal_iid) {
                current_search_path_hashes.push_back(pos.zobrist_hash);
                /* int iid_score = */ search(iid_next_pos, depth - IID_REDUCTION, alpha, beta, ply + 1, false, true, current_search_path_hashes, moves[0]);
                current_search_path_hashes.pop_back();
                // Result of IID search could refine tt_move or its score, or reorder moves.
                // For simplicity, we just do the search; its TT entry might help the main search of this move.
                // Re-score moves after IID potentially updates TT:
                if (!stop_search_flag) { // Don't re-score if search stopped
                     probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score); // Re-probe for updated TT move
                     score_moves(pos, moves, tt_move, ply, prev_opponent_move); // Re-sort
                }
            }
        }
    }


    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;

    current_search_path_hashes.push_back(pos.zobrist_hash);

    for (int i = 0; i < (int)moves.size(); ++i) {
        const Move& current_move = moves[i];

        // Futility Pruning application (actual pruning of quiet moves)
        if (!is_pv_node && !in_check && depth <= FUTILITY_PRUNING_MAX_DEPTH) {
            if (pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) { // Is a quiet move
                int static_eval = evaluate(pos); // Re-evaluate or use a stored one if available
                if (static_eval + futility_margins[depth] <= alpha) {
                    continue; // Prune this quiet move
                }
            }
        }

        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        if (legal_moves_played == 1) {
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_search_path_hashes, current_move);
        } else {
            int R_lmr = 0;
            if (depth >= 3 && i >= (is_pv_node ? 3 : 2) &&
                !in_check && current_move.promotion == NO_PIECE &&
                pos.piece_on_sq(current_move.to) == NO_PIECE &&
                current_move.score < 700000) { // LMR conditions
                R_lmr = 1;
                if (depth >= 5 && i >= (is_pv_node ? 5 : 4)) R_lmr = (depth > 7 ? 2 : 1);
                R_lmr = std::min(R_lmr, depth - 2); if (R_lmr < 0) R_lmr = 0;
            }
            score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes, current_move);
            if (R_lmr > 0 && score > alpha) {
                 score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes, current_move);
            }
            if (score > alpha && score < beta) {
                 score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, current_search_path_hashes, current_move);
            }
        }

        if (stop_search_flag) { current_search_path_hashes.pop_back(); return 0; }

        if (score > best_score) {
            best_score = score;
            best_move_found = current_move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) { // Beta cutoff
                    if (ply < MAX_PLY && pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) {
                        if (!(current_move == killer_moves[ply][0])) {
                            killer_moves[ply][1] = killer_moves[ply][0];
                            killer_moves[ply][0] = current_move;
                        }
                        if(history_heuristic[pos.side_to_move][current_move.from][current_move.to] < (30000 - depth*depth) )
                            history_heuristic[pos.side_to_move][current_move.from][current_move.to] += depth * depth;
                    }
                    // Update Countermove Heuristic
                    if (!prev_opponent_move.is_null()) {
                         // Using simplified key: just opponent's to-square.
                         // A more robust key would involve opponent's piece type.
                         countermoves[NO_PIECE][prev_opponent_move.to] = best_move_found;
                    }

                    current_search_path_hashes.pop_back();
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found);
                    return beta;
                }
            }
        }
    }
    current_search_path_hashes.pop_back();

    if (legal_moves_played == 0) return in_check ? (-MATE_SCORE + ply) : 0;

    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, ply, best_score, final_bound_type, best_move_found);
    return best_score;
}


// --- UCI ---
Position uci_root_pos;
Move uci_best_move_overall;

void parse_fen(Position& pos, const std::string& fen_str) {
    pos = Position(); std::stringstream ss(fen_str); std::string part;
    ss >> part; int rank = 7, file = 0;
    for (char c : part) {
        if (std::isdigit(c)) file += (c - '0');
        else if (c == '/') { rank--; file = 0; }
        else {
            Piece p_type = NO_PIECE; Color p_color = NO_COLOR;
            if (std::islower(c)) p_color = BLACK; else p_color = WHITE;
            char lc = std::tolower(c);
            if (lc=='p') p_type=PAWN; else if (lc=='n') p_type=KNIGHT; else if (lc=='b') p_type=BISHOP;
            else if (lc=='r') p_type=ROOK; else if (lc=='q') p_type=QUEEN; else if (lc=='k') p_type=KING;
            if (p_type != NO_PIECE && rank>=0 && rank<8 && file>=0 && file<8) {
                int sq = rank*8+file; pos.piece_bb[p_type] |= set_bit(sq); pos.color_bb[p_color] |= set_bit(sq);
            } file++;
        }
    }
    ss >> part; pos.side_to_move = (part == "w") ? WHITE : BLACK;
    ss >> part; pos.castling_rights = 0;
    for (char c:part) {
        if (c=='K') pos.castling_rights|=WK_CASTLE_MASK; else if (c=='Q') pos.castling_rights|=WQ_CASTLE_MASK;
        else if (c=='k') pos.castling_rights|=BK_CASTLE_MASK; else if (c=='q') pos.castling_rights|=BQ_CASTLE_MASK;
    }
    ss >> part;
    if (part != "-") {
        if (part.length()==2 && part[0]>='a' && part[0]<='h' && part[1]>='1' && part[1]<='8')
            pos.ep_square = (part[0]-'a') + (part[1]-'1')*8;
        else pos.ep_square = -1;
    } else pos.ep_square = -1;
    if(ss>>part) {try{pos.halfmove_clock=std::stoi(part);}catch(...){pos.halfmove_clock=0;}} else pos.halfmove_clock=0;
    if(ss>>part) {try{pos.fullmove_number=std::stoi(part);}catch(...){pos.fullmove_number=1;}} else pos.fullmove_number=1;
    pos.ply = 0; pos.zobrist_hash = calculate_zobrist_hash(pos); game_history_hashes.clear();
}

Move parse_uci_move_from_string(const Position& current_pos, const std::string& uci_move_str) {
    Move m = NULL_MOVE;
    if (uci_move_str.length()<4 || uci_move_str.length()>5 || uci_move_str=="0000") return m;
    m.from = (uci_move_str[0]-'a') + (uci_move_str[1]-'1')*8;
    m.to = (uci_move_str[2]-'a') + (uci_move_str[3]-'1')*8;
    if (m.from<0 || m.from>63 || m.to<0 || m.to>63) return NULL_MOVE;
    if (uci_move_str.length()==5) {
        char pc = uci_move_str[4];
        if(pc=='q')m.promotion=QUEEN; else if(pc=='n')m.promotion=KNIGHT;
        else if(pc=='b')m.promotion=BISHOP; else if(pc=='r')m.promotion=ROOK; else return NULL_MOVE;
    } return m;
}

uint64_t calculate_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c)
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[c];
            while(b) { int sq=lsb_index(b); b&=b-1; h^=zobrist_pieces[c][p][sq]; }
        }
    h ^= zobrist_castling[pos.castling_rights];
    h ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    if (pos.side_to_move == BLACK) h ^= zobrist_side_to_move;
    return h;
}

void uci_loop() {
    std::string line, token;
    parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    Move last_actual_move_made_by_opponent = NULL_MOVE; // Track for countermoves at root

    while (std::getline(std::cin, line)) {
        std::istringstream ss(line);
        ss >> token;

        if (token == "uci") {
            std::cout << "id name Amira_v4_AdvancedSearch\n";
            std::cout << "id author ChessTubeTree & Gemini\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 1024\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            std::string nt, vt, ns, vsv; ss>>nt;
            if(nt=="name"){ ss>>ns>>vt>>vsv; if(ns=="Hash"){try{g_configured_tt_size_mb=std::max(0,std::min(std::stoi(vsv),1024));}catch(...){} init_tt(g_configured_tt_size_mb); g_tt_is_initialized=true;}}
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; } else { clear_tt(); }
            reset_killers_and_history(); game_history_hashes.clear();
            last_actual_move_made_by_opponent = NULL_MOVE;
        } else if (token == "position") {
            std::string fen_str_collector; ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                std::string ntc; if (ss>>ntc) { if(ntc=="moves")token="moves"; else {ss.clear();ss.seekg(-(std::streamoff)ntc.length(),std::ios_base::cur);token="";}} else token="";
            } else if (token == "fen") {
                std::string tfp;
                while(ss>>tfp){if(tfp=="moves"){token="moves";break;} fen_str_collector+=tfp+" ";}
                if(!fen_str_collector.empty())fen_str_collector.pop_back(); parse_fen(uci_root_pos,fen_str_collector);
            }
            game_history_hashes.push_back(uci_root_pos.zobrist_hash);
            if (token == "moves") {
                std::string move_str_uci;
                Move current_parsed_move = NULL_MOVE;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && move_str_uci!="0000") break; if (m.is_null() && move_str_uci=="0000") break;
                    bool legal; uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                    game_history_hashes.push_back(uci_root_pos.zobrist_hash);
                    current_parsed_move = m; // Track the last move made in the sequence
                }
                last_actual_move_made_by_opponent = current_parsed_move; // This was the opponent's last move before "go"
            } else {
                 last_actual_move_made_by_opponent = NULL_MOVE; // Reset if position set by FEN without moves
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }
            int wtime=-1,btime=-1,winc=0,binc=0,movestogo=0; long long ftmpm=-1; int max_depth=MAX_PLY;
            std::string gp; while(ss>>gp){
                if(gp=="wtime")ss>>wtime; else if(gp=="btime")ss>>btime; else if(gp=="winc")ss>>winc;
                else if(gp=="binc")ss>>binc; else if(gp=="movestogo")ss>>movestogo;
                else if(gp=="movetime")ss>>ftmpm; else if(gp=="depth")ss>>max_depth;
            }
            if(ftmpm!=-1) search_budget_ms=std::max(10LL,ftmpm-50);
            else{
                int mt=(uci_root_pos.side_to_move==WHITE)?wtime:btime; int mi=(uci_root_pos.side_to_move==WHITE)?winc:binc;
                if(mt!=-1){ long long bts; if(movestogo>0&&movestogo<40) bts=mt/std::max(1,movestogo); else bts=mt/25;
                search_budget_ms=bts+mi-50; if(mt>100&&search_budget_ms>mt*0.8)search_budget_ms=(long long)(mt*0.8);}
                else search_budget_ms=2000;
            } if(search_budget_ms<=0)search_budget_ms=50;
            search_start_timepoint=std::chrono::steady_clock::now(); reset_search_state();
            uci_best_move_overall=NULL_MOVE; int best_score_overall=0; std::vector<uint64_t> rph;
            int asp_alpha=-INF_SCORE, asp_beta=INF_SCORE, asp_delta=25;

            for (int depth = 1; depth <= max_depth; ++depth) {
                int cur_score;
                if(depth<=1) cur_score=search(uci_root_pos,depth,-INF_SCORE,INF_SCORE,0,true,true,rph,last_actual_move_made_by_opponent);
                else{
                    cur_score=search(uci_root_pos,depth,asp_alpha,asp_beta,0,true,true,rph,last_actual_move_made_by_opponent);
                    if(!stop_search_flag&&(cur_score<=asp_alpha||cur_score>=asp_beta)){
                        asp_alpha=-INF_SCORE; asp_beta=INF_SCORE;
                        cur_score=search(uci_root_pos,depth,asp_alpha,asp_beta,0,true,true,rph,last_actual_move_made_by_opponent);
                    }
                }
                if(stop_search_flag && depth > 1) break;
                if(abs(cur_score)<MATE_THRESHOLD&&cur_score>-INF_SCORE&&cur_score<INF_SCORE){
                    asp_alpha=cur_score-asp_delta; asp_beta=cur_score+asp_delta;
                    asp_delta+=asp_delta/3+5; if(asp_delta>300)asp_delta=300;
                } else { asp_alpha=-INF_SCORE; asp_beta=INF_SCORE; asp_delta=50; }
                Move ttrm=NULL_MOVE; int ttrs; int da=-INF_SCORE,db=INF_SCORE;
                if(probe_tt(uci_root_pos.zobrist_hash,depth,0,da,db,ttrm,ttrs)){if(!ttrm.is_null())uci_best_move_overall=ttrm; best_score_overall=cur_score;}
                else{best_score_overall=cur_score; TTEntry rtec=transposition_table[uci_root_pos.zobrist_hash&tt_mask]; if(rtec.hash==uci_root_pos.zobrist_hash&&!rtec.best_move.is_null())uci_best_move_overall=rtec.best_move;}
                auto ntp=std::chrono::steady_clock::now(); auto ems=std::chrono::duration_cast<std::chrono::milliseconds>(ntp-search_start_timepoint).count(); if(ems<0)ems=0;
                std::cout<<"info depth "<<depth<<" score cp "<<best_score_overall;
                if(best_score_overall>MATE_THRESHOLD)std::cout<<" mate "<<(MATE_SCORE-best_score_overall+1)/2;
                else if(best_score_overall<-MATE_THRESHOLD)std::cout<<" mate "<<-(MATE_SCORE+best_score_overall+1)/2;
                std::cout<<" nodes "<<nodes_searched<<" time "<<ems; if(ems>0&&nodes_searched>0)std::cout<<" nps "<<(nodes_searched*1000/ems);
                if(!uci_best_move_overall.is_null()){ std::cout<<" pv"; Position tempp=uci_root_pos;
                    for(int pvi=0;pvi<depth;++pvi){ Move pvm; int pvs; int pva=-INF_SCORE,pvb=INF_SCORE;
                        if(probe_tt(tempp.zobrist_hash,1,0,pva,pvb,pvm,pvs)&&!pvm.is_null()){ bool lpv; Position ntpos=make_move(tempp,pvm,lpv);
                            if(lpv){std::cout<<" "<<move_to_uci(pvm);tempp=ntpos;} else {if(pvi==0)std::cout<<" "<<move_to_uci(uci_best_move_overall);break;}
                        } else {if(pvi==0)std::cout<<" "<<move_to_uci(uci_best_move_overall);break;} if(stop_search_flag)break;
                    }
                } std::cout<<std::endl;
                if(abs(best_score_overall)>MATE_THRESHOLD&&depth>1)break;
                if(search_budget_ms>0&&ems>0&&depth>1){if(ems*2.5>search_budget_ms&&depth>3)break; else if(ems*1.8>search_budget_ms)break;}
                if(depth>=max_depth)break;
            }
            if(!uci_best_move_overall.is_null()) std::cout<<"bestmove "<<move_to_uci(uci_best_move_overall)<<std::endl;
            else{ std::vector<Move> lmf; generate_moves(uci_root_pos,lmf); bool folf=false;
                for(const auto& mf:lmf){bool ilf; Position tp=make_move(uci_root_pos,mf,ilf); if(ilf){std::cout<<"bestmove "<<move_to_uci(mf)<<std::endl;folf=true;break;}}
                if(!folf) std::cout<<"bestmove 0000\n"<<std::flush;
            }
        } else if (token == "quit" || token == "stop") {
            stop_search_flag = true; if (token == "quit") break;
        }
    }
}

int main(int argc, char* argv[]) {
    std::ios_base::sync_with_stdio(false); std::cin.tie(NULL);
    init_zobrist(); init_attack_tables(); init_eval_masks();
    reset_killers_and_history();
    uci_loop();
    return 0;
}
