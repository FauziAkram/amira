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

constexpr int MAX_PLY = 128;
constexpr int TT_SIZE_MB_DEFAULT = 256;

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


// Forward Declarations
struct Move;
struct Position;
int evaluate(const Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only = false);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);
int see(const Position& pos, const Move& move);


// --- Zobrist Hashing ---
uint64_t zobrist_pieces[2][6][64];
uint64_t zobrist_castling[16];
uint64_t zobrist_ep[65]; // 64 squares + 1 for no EP square (index 64)
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
        std::memset(this, 0, sizeof(Position)); // Efficiently zero out
        side_to_move = WHITE;
        ep_square = -1;
        fullmove_number = 1;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }

    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE;
        uint64_t b = set_bit(sq);
        if (!( (color_bb[WHITE] | color_bb[BLACK]) & b)) return NO_PIECE; // Optimization
        for (int p = PAWN; p <= KING; ++p) {
            if (piece_bb[p] & b) return (Piece)p;
        }
        return NO_PIECE; // Should not be reached if occupied bit was set
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
    const int deltas[] = {1, -1, 8, -8}; // E, W, N, S
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8; // (s-d) is original square
            if (abs(d) == 1 && r_curr != r_prev) break; // Horizontal wrap-around
            if (abs(d) == 8 && c_curr != c_prev && abs(d) != 8) break; // Vertical wrap-around (bugfix)

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
            if (abs(r_curr - r_prev) != 1 || abs(c_curr - c_prev) != 1) break; // Diagonal wrap-around

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
    return 0; // Should not happen
}


// --- is_square_attacked and SEE support ---
uint64_t get_attackers_to(const Position& pos, int sq, uint64_t occupied) {
    uint64_t white_pieces = pos.color_bb[WHITE];
    uint64_t black_pieces = pos.color_bb[BLACK];

    uint64_t attackers = (pawn_attacks_bb[WHITE][sq] & pos.piece_bb[PAWN] & black_pieces) |
                         (pawn_attacks_bb[BLACK][sq] & pos.piece_bb[PAWN] & white_pieces);

    attackers |= knight_attacks_bb[sq] & pos.piece_bb[KNIGHT] & (white_pieces | black_pieces);
    attackers |= king_attacks_bb[sq] & pos.piece_bb[KING] & (white_pieces | black_pieces);

    uint64_t bishops_queens = pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN];
    attackers |= get_bishop_attacks_from_sq(sq, occupied) & bishops_queens & (white_pieces | black_pieces);

    uint64_t rooks_queens = pos.piece_bb[ROOK] | pos.piece_bb[QUEEN];
    attackers |= get_rook_attacks_from_sq(sq, occupied) & rooks_queens & (white_pieces | black_pieces);

    return attackers;
}

bool is_square_attacked(const Position& pos, int sq_to_check, int attacker_c) {
    return get_attackers_to(pos, sq_to_check, pos.get_occupied_bb()) & pos.color_bb[attacker_c];
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
        int promotion_rank_idx = (stm == WHITE) ? 6 : 1; // Rank *before* promotion

        // Pawn pushes
        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >=0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (rank == promotion_rank_idx) { // Reached promotion rank by one step
                add_move_to_list(moves_list, from, one_step_sq, QUEEN); add_move_to_list(moves_list, from, one_step_sq, ROOK);
                add_move_to_list(moves_list, from, one_step_sq, BISHOP); add_move_to_list(moves_list, from, one_step_sq, KNIGHT);
            } else if (!captures_only) {
                add_move_to_list(moves_list, from, one_step_sq);
            }
            // Double pawn push (only if single push is also possible and not captures_only)
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

            attacks &= (captures_only ? opp_pieces : ~my_pieces); // Only captures or any non-friendly square

            while (attacks) {
                int to = lsb_index(attacks);
                attacks &= attacks - 1;
                add_move_to_list(moves_list, from, to);
            }
        }
    }

    // Castling
    if (!captures_only) {
        int king_sq_idx = lsb_index(pos.piece_bb[KING] & my_pieces);
        if (king_sq_idx != -1) { // Should always be true in a valid position
            if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ + 1) && !get_bit(occupied, E1_SQ + 2) && // F1, G1 empty
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ + 1, BLACK) && !is_square_attacked(pos, E1_SQ + 2, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, E1_SQ + 2); // King to G1
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ - 1) && !get_bit(occupied, E1_SQ - 2) && !get_bit(occupied, E1_SQ - 3) && // D1, C1, B1 empty
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ - 1, BLACK) && !is_square_attacked(pos, E1_SQ - 2, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, E1_SQ - 2); // King to C1
                }
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ + 1) && !get_bit(occupied, E8_SQ + 2) && // F8, G8 empty
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ + 1, WHITE) && !is_square_attacked(pos, E8_SQ + 2, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, E8_SQ + 2); // King to G8
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ - 1) && !get_bit(occupied, E8_SQ - 2) && !get_bit(occupied, E8_SQ - 3) && // D8, C8, B8 empty
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ - 1, WHITE) && !is_square_attacked(pos, E8_SQ - 2, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, E8_SQ - 2); // King to C8
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
    Piece piece_captured = pos.piece_on_sq(move.to); // Piece on 'to' square before move

    if (piece_moved == NO_PIECE || pos.color_on_sq(move.from) != stm) {
        return pos; // Moving no piece or opponent's piece
    }
    if (piece_captured != NO_PIECE && pos.color_on_sq(move.to) == stm) {
        return pos; // Tried to capture own piece
    }

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
        if (piece_moved != PAWN) return pos; // Invalid promotion
        int promotion_rank_actual = (stm == WHITE) ? 7 : 0; // Actual rank where pawn lands for promotion
        if (move.to / 8 != promotion_rank_actual) return pos; // Invalid promotion square

        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else {
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    uint8_t old_castling_rights = pos.castling_rights; // Use pos's castling rights for comparison later
    uint8_t new_castling_rights = pos.castling_rights;

    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);

        // Handle castling move itself (rook movement)
        if (abs(move.to - move.from) == 2) { // King moved two squares
            int rook_from_sq, rook_to_sq;
            if (move.to == E1_SQ + 2) { rook_from_sq = H1_SQ; rook_to_sq = E1_SQ + 1; } // White Kingside
            else if (move.to == E1_SQ - 2) { rook_from_sq = A1_SQ; rook_to_sq = E1_SQ - 1; } // White Queenside
            else if (move.to == E8_SQ + 2) { rook_from_sq = H8_SQ; rook_to_sq = E8_SQ + 1; } // Black Kingside
            else { rook_from_sq = A8_SQ; rook_to_sq = E8_SQ - 1; } // Black Queenside (move.to == E8_SQ - 2)

            next_pos.piece_bb[ROOK] &= ~set_bit(rook_from_sq);
            next_pos.piece_bb[ROOK] |= set_bit(rook_to_sq);
            next_pos.color_bb[stm] &= ~set_bit(rook_from_sq);
            next_pos.color_bb[stm] |= set_bit(rook_to_sq);
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
        }
    }

    // Rook moves from its starting square
    if (piece_moved == ROOK) {
        if (stm == WHITE) {
            if (move.from == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
            else if (move.from == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
        } else { // BLACK
            if (move.from == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
            else if (move.from == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
        }
    }

    // Rook captured on its starting square
    if (piece_captured == ROOK) {
        if (opp == WHITE) { // Captured rook was white (stm is black, opp is white)
             if (move.to == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
             else if (move.to == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
        } else { // Captured rook was black (stm is white, opp is black)
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

    int king_sq_after_move = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]); // King of the side that just MOVED
    if (king_sq_after_move == -1) { /* Should not happen */ return pos; }
    if (is_square_attacked(next_pos, king_sq_after_move, opp)) { // Check if own king is attacked by opponent
        return pos; // Illegal move, king left in check
    }

    legal_move_flag = true;
    return next_pos;
}

// --- Evaluation ---
const int piece_values[7] = {100, 320, 330, 500, 900, 10000, 0}; // P,N,B,R,Q,K,NO_PIECE
const int piece_values_mg[6] = {100, 320, 330, 500, 900, 0};
const int piece_values_eg[6] = {120, 320, 330, 530, 950, 0};

// --- NEW/UPDATED Evaluation Constants ---
const int ISOLATED_PAWN_PENALTY_MG = -10;
const int ISOLATED_PAWN_PENALTY_EG = -15;
const int DOUBLED_PAWN_PENALTY_MG = -12;
const int DOUBLED_PAWN_PENALTY_EG = -20;
const int ROOK_ON_SEVENTH_BONUS_MG = 35;
const int ROOK_ON_SEVENTH_BONUS_EG = 50;
const int KNIGHT_OUTPOST_BONUS_MG = 25;
const int KNIGHT_OUTPOST_BONUS_EG = 40;
const int PAWN_SHIELD_MISSING_PENALTY = -18;
const int PAWN_SHIELD_ADVANCED_PENALTY_PER_RANK = -12;
const int ROOK_MOBILITY_BONUS[15] = { -15, -10, -5, 0, 3, 5, 7, 9, 10, 11, 12, 12, 13, 13, 13 };
const int BISHOP_MOBILITY_BONUS[14] = { -15, -10, -5, 0, 3, 5, 7, 8, 9, 10, 10, 11, 11, 11 };

// Piece Square Tables (values are for white, mirrored for black)
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
     20, 30, 10,  0,  0, 10, 30, 20,
     20, 20,  0,  0,  0,  0, 20, 20,
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
const int game_phase_inc[6] = {0, 1, 1, 2, 4, 0}; // P,N,B,R,Q,K

// Evaluation helper masks
uint64_t file_bb_mask[8];
uint64_t rank_bb_mask[8];
uint64_t adjacent_files_mask[8];
uint64_t white_passed_pawn_block_mask[64];
uint64_t black_passed_pawn_block_mask[64];
uint64_t pawn_attack_span[2][64];
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

    for (int f = 0; f < 8; ++f) {
        adjacent_files_mask[f] = 0ULL;
        if (f > 0) adjacent_files_mask[f] |= file_bb_mask[f - 1];
        if (f < 7) adjacent_files_mask[f] |= file_bb_mask[f + 1];
    }

    for (int sq = 0; sq < 64; ++sq) {
        white_passed_pawn_block_mask[sq] = 0ULL;
        black_passed_pawn_block_mask[sq] = 0ULL;
        pawn_attack_span[WHITE][sq] = 0ULL;
        pawn_attack_span[BLACK][sq] = 0ULL;
        int r = sq / 8;
        int f = sq % 8;

        uint64_t forward_files = file_bb_mask[f];
        if (f > 0) forward_files |= file_bb_mask[f-1];
        if (f < 7) forward_files |= file_bb_mask[f+1];

        for (int cur_r = r + 1; cur_r < 8; ++cur_r) {
            white_passed_pawn_block_mask[sq] |= forward_files & rank_bb_mask[cur_r];
        }
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) {
            black_passed_pawn_block_mask[sq] |= forward_files & rank_bb_mask[cur_r];
        }
        
        // For knight outposts, check if enemy pawns can attack
        uint64_t attack_files = 0;
        if (f > 0) attack_files |= file_bb_mask[f-1];
        if (f < 7) attack_files |= file_bb_mask[f+1];
        
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) pawn_attack_span[WHITE][sq] |= attack_files & rank_bb_mask[cur_r]; // Enemy pawns for white are on lower ranks
        for (int cur_r = r + 1; cur_r < 8; ++cur_r) pawn_attack_span[BLACK][sq] |= attack_files & rank_bb_mask[cur_r]; // Enemy pawns for black are on higher ranks
    }
}


int evaluate(const Position& pos) {
    int mg_score = 0;
    int eg_score = 0;
    int game_phase = 0;
    uint64_t occupied = pos.get_occupied_bb();

    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;

        uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[current_eval_color];
        uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1 - current_eval_color];
        uint64_t friendly_pieces = pos.color_bb[current_eval_color];

        // Material and PST
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);

                mg_score += side_multiplier * (piece_values_mg[p] + pst_mg_all[p][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p] + pst_eg_all[p][mirrored_sq]);

                // --- POSITIONAL EVALUATION FEATURES ---
                if ((Piece)p == PAWN) {
                    int f = sq % 8;
                    // ISOLATED PAWN
                    if ((adjacent_files_mask[f] & all_friendly_pawns) == 0) {
                        mg_score += side_multiplier * ISOLATED_PAWN_PENALTY_MG;
                        eg_score += side_multiplier * ISOLATED_PAWN_PENALTY_EG;
                    }
                    // DOUBLED PAWN
                    if (pop_count(file_bb_mask[f] & all_friendly_pawns) > 1) {
                         mg_score += side_multiplier * DOUBLED_PAWN_PENALTY_MG;
                         eg_score += side_multiplier * DOUBLED_PAWN_PENALTY_EG;
                    }
                    // PASSED PAWN
                    bool is_passed = (current_eval_color == WHITE)
                        ? ((white_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0)
                        : ((black_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0);
                    if (is_passed) {
                        int rank_from_own_side = (current_eval_color == WHITE) ? (sq / 8) : (7 - (sq / 8));
                        mg_score += side_multiplier * passed_pawn_bonus_mg[rank_from_own_side];
                        eg_score += side_multiplier * passed_pawn_bonus_eg[rank_from_own_side];
                    }
                } else if ((Piece)p == KNIGHT) {
                    int r = sq / 8;
                    // KNIGHT OUTPOST
                    int own_side_rank = (current_eval_color == WHITE) ? r : (7 - r);
                    if (own_side_rank >= 3) { // Ranks 4-7 for white, 5-2 for black
                        if ((pawn_attacks_bb[1-current_eval_color][sq] & all_friendly_pawns) != 0) { // Supported by friendly pawn
                            if ((pawn_attack_span[current_eval_color][sq] & all_enemy_pawns) == 0) { // Not attackable by enemy pawns
                                mg_score += side_multiplier * KNIGHT_OUTPOST_BONUS_MG;
                                eg_score += side_multiplier * KNIGHT_OUTPOST_BONUS_EG;
                            }
                        }
                    }
                } else if ((Piece)p == ROOK) {
                    int r = sq / 8;
                    int f = sq % 8;
                    // ROOK ON OPEN/SEMI-OPEN FILE
                    if ((file_bb_mask[f] & all_friendly_pawns) == 0) {
                        if ((file_bb_mask[f] & all_enemy_pawns) == 0) { mg_score += side_multiplier * 20; eg_score += side_multiplier * 15; }
                        else { mg_score += side_multiplier * 10; eg_score += side_multiplier * 5;}
                    }
                    // ROOK ON 7th RANK
                    int seventh_rank = (current_eval_color == WHITE) ? 6 : 1;
                    if (r == seventh_rank) {
                        mg_score += side_multiplier * ROOK_ON_SEVENTH_BONUS_MG;
                        eg_score += side_multiplier * ROOK_ON_SEVENTH_BONUS_EG;
                    }
                    // ROOK MOBILITY
                    uint64_t attacks = get_rook_attacks_from_sq(sq, occupied) & ~friendly_pieces;
                    int mobility = pop_count(attacks);
                    mg_score += side_multiplier * ROOK_MOBILITY_BONUS[mobility];
                    eg_score += side_multiplier * ROOK_MOBILITY_BONUS[mobility];

                } else if ((Piece)p == BISHOP) {
                    // BISHOP MOBILITY
                    uint64_t attacks = get_bishop_attacks_from_sq(sq, occupied) & ~friendly_pieces;
                    int mobility = pop_count(attacks);
                    mg_score += side_multiplier * BISHOP_MOBILITY_BONUS[mobility];
                    eg_score += side_multiplier * BISHOP_MOBILITY_BONUS[mobility];
                }
            }
        }
        if (pop_count(pos.piece_bb[BISHOP] & pos.color_bb[current_eval_color]) >= 2) {
            mg_score += side_multiplier * 30; eg_score += side_multiplier * 50;
        }

        // --- King Safety, Pawn Shield, Pawn Storms ---
        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[current_eval_color]);
        if (king_sq != -1) {
            int king_file = king_sq % 8;
            int pawn_shield_score = 0;

            // Check if king is on wings, indicating likely castled state
            if (king_file >= 5 || king_file <= 2) {
                int ideal_shield_rank = (current_eval_color == WHITE) ? 1 : 6;
                for (int df = -1; df <= 1; ++df) {
                    int shield_file = king_file + df;
                    if (shield_file >= 0 && shield_file < 8) {
                        uint64_t shield_pawns = file_bb_mask[shield_file] & all_friendly_pawns;
                        if (shield_pawns == 0) {
                            pawn_shield_score += PAWN_SHIELD_MISSING_PENALTY;
                        } else {
                            int pawn_sq = (current_eval_color == WHITE) ? lsb_index(shield_pawns) : (63 - lsb_index(_byteswap_uint64(shield_pawns))); // Foremost pawn
                            int pawn_rank = pawn_sq / 8;
                            int rank_diff = abs(pawn_rank - ideal_shield_rank);
                            if (rank_diff > 1) { // Pawn has advanced beyond initial 2 squares
                                pawn_shield_score += (rank_diff - 1) * PAWN_SHIELD_ADVANCED_PENALTY_PER_RANK;
                            }
                        }
                    }
                }
            }
            mg_score += side_multiplier * pawn_shield_score;

            // KING ATTACKS (Original Logic)
            int king_attack_score = 0;
            uint64_t king_1_ring_zone = king_attacks_bb[king_sq];
            uint64_t enemy_player_pieces = pos.color_bb[1 - current_eval_color];
            
            uint64_t attackers = get_attackers_to(pos, king_sq, occupied) & enemy_player_pieces;
            while(attackers) {
                int attacker_sq = lsb_index(attackers);
                attackers &= attackers - 1;
                Piece p = pos.piece_on_sq(attacker_sq);
                if (p == PAWN) king_attack_score +=1;
                else if (p == KNIGHT) king_attack_score +=2;
                else if (p == BISHOP) king_attack_score +=2;
                else if (p == ROOK) king_attack_score +=3;
                else if (p == QUEEN) king_attack_score +=5;
            }
            int danger_idx = std::min(king_attack_score, 14);
            mg_score -= side_multiplier * king_danger_penalty_mg[danger_idx];
            eg_score -= side_multiplier * king_danger_penalty_eg[danger_idx];
        }
    }

    game_phase = std::min(game_phase, 24);
    game_phase = std::max(game_phase, 0);

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

// --- Configurable Time Management Options ---
int g_time_usage_divisor = 25;
int g_increment_usage_percent = 80;
int g_max_time_usage_percent = 80;

void init_tt(size_t mb_size) {
    if (mb_size == 0) {
        transposition_table.clear(); tt_mask = 0; return;
    }
    size_t num_entries = (mb_size * 1024 * 1024) / sizeof(TTEntry);
    if (num_entries == 0) {
        transposition_table.clear(); tt_mask = 0; return;
    }
    size_t power_of_2_entries = 1;
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries) {
        power_of_2_entries *= 2;
    }
     if (power_of_2_entries == 0) {
         transposition_table.clear(); tt_mask = 0; return;
    }
    try {
        transposition_table.assign(power_of_2_entries, TTEntry());
        tt_mask = power_of_2_entries - 1;
    } catch (const std::bad_alloc&) {
        transposition_table.clear(); tt_mask = 0;
    }
}

void clear_tt() {
    if (!transposition_table.empty()) {
        std::memset(transposition_table.data(), 0, transposition_table.size() * sizeof(TTEntry));
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

// --- Search ---
std::chrono::steady_clock::time_point search_start_timepoint;
long long search_budget_ms = 0;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;

Move killer_moves[MAX_PLY][2];
int history_heuristic[2][64][64];
std::vector<uint64_t> game_history_hashes; // Stores hashes of positions played in the game for 3-fold repetition

// --- NEW: Search Constants ---
const int FUTILITY_MAX_DEPTH = 3;
const int futility_margins[FUTILITY_MAX_DEPTH + 1] = { 0, 150, 300, 500 }; // Margins for depths 0, 1, 2, 3
const int SEE_GOOD_CAPTURE_BONUS = 1000000;
const int SEE_BAD_CAPTURE_SCORE = -500000;

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
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) { // Check time every 2048 nodes
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

// --- NEW: Static Exchange Evaluation (SEE) ---
int see_recursive(Position& pos, int to_sq, int side, uint64_t occupied) {
    int score = 0;
    uint64_t attackers = get_attackers_to(pos, to_sq, occupied) & pos.color_bb[side];
    if (attackers == 0) return 0;
    
    int from_sq = -1;
    Piece p_type = NO_PIECE;

    // Find least valuable attacker
    for (int p = PAWN; p <= KING; ++p) {
        uint64_t subset = attackers & pos.piece_bb[p];
        if (subset) {
            p_type = (Piece)p;
            from_sq = lsb_index(subset);
            break;
        }
    }

    if (from_sq != -1) {
        uint64_t from_bb = set_bit(from_sq);
        occupied &= ~from_bb; // Virtually remove attacker

        // To get the value of the victim, we need to know what piece was on the square before this move
        // This requires passing the victim piece value down. Let's simplify and just use a see(pos, move) entry point.
        // For now, let's restructure `see` to be simpler.
    }
    
    return score;
}

// Simplified SEE for now. A full SEE needs to track the victim's value.
// Let's create the entry point `see(pos, move)` which is cleaner.
int see(const Position& pos, const Move& move) {
    int gain[32];
    int d = 0;
    
    uint64_t from_bb = set_bit(move.from);
    uint64_t to_bb = set_bit(move.to);

    uint64_t occupied = pos.get_occupied_bb();
    uint64_t attackers = get_attackers_to(pos, move.to, occupied);
    
    int side = pos.side_to_move;
    
    Piece victim = pos.piece_on_sq(move.to);
    if(move.promotion != NO_PIECE) victim = move.promotion;
    else if(move.to == pos.ep_square && pos.piece_on_sq(move.from) == PAWN) victim = PAWN;
    
    gain[d] = piece_values[victim];
    
    Piece attacker_piece = pos.piece_on_sq(move.from);
    
    occupied ^= from_bb; // Attacker moves, so its square is now empty
    
    side = 1-side; // switch side

    while(true) {
        d++;
        gain[d] = piece_values[attacker_piece] - gain[d-1];
        if (std::max(-gain[d-1], gain[d]) < 0) break; // if previous trade was bad, and current is worse, stop.
        
        attackers &= occupied; // Update attackers that are still on the board

        uint64_t our_attackers = attackers & pos.color_bb[side];
        if(!our_attackers) break;
        
        // find our least valuable attacker
        attacker_piece = NO_PIECE;
        for (int p = PAWN; p <= KING; p++) {
            uint64_t piece_subset = our_attackers & pos.piece_bb[p];
            if (piece_subset) {
                from_bb = set_bit(lsb_index(piece_subset));
                attacker_piece = (Piece)p;
                break;
            }
        }
        if (attacker_piece == NO_PIECE) break;

        occupied ^= from_bb;
        side = 1-side;
    }

    while(--d) {
        gain[d-1] = -std::max(-gain[d-1], gain[d]);
    }
    
    return gain[0];
}


void score_moves(const Position& pos, std::vector<Move>& moves, const Move& tt_move, int ply) {
    for (Move& m : moves) {
        if (!tt_move.is_null() && m == tt_move) {
            m.score = 2000000; // TT move gets highest priority
            continue;
        }
        
        Piece captured_piece = pos.piece_on_sq(m.to);
        if (captured_piece != NO_PIECE || (pos.piece_on_sq(m.from) == PAWN && m.to == pos.ep_square)) {
            int see_score = see(pos, m);
            if (see_score >= 0) {
                m.score = SEE_GOOD_CAPTURE_BONUS + see_score;
            } else {
                m.score = SEE_BAD_CAPTURE_SCORE + see_score;
            }
        } else if (m.promotion != NO_PIECE) {
             m.score = 900000 + piece_values[m.promotion];
        } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
            m.score = 800000; // First killer move
        } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
            m.score = 700000; // Second killer move
        } else {
            // History heuristic for quiet moves
            m.score = history_heuristic[pos.side_to_move][m.from][m.to];
        }
    }
    // Sort moves by score in descending order
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

    Move dummy_tt_move = NULL_MOVE;
    score_moves(pos, q_moves, dummy_tt_move, ply);

    int legal_moves_in_qsearch = 0;
    for (const Move& cap_move : q_moves) {
        if (pos.piece_on_sq(cap_move.to) == NO_PIECE && !in_check) continue; // In Q-search, only search captures unless in check
        if (cap_move.score < SEE_GOOD_CAPTURE_BONUS && !in_check) continue; // SEE Pruning: Don't search bad captures in Q-search

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


int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, std::vector<uint64_t>& current_search_path_hashes) {
    nodes_searched++;
    if (check_time()) return 0;
    if (ply >= MAX_PLY -1) return evaluate(pos);

    for (uint64_t path_hash : current_search_path_hashes) {
        if (path_hash == pos.zobrist_hash) return 0;
    }
    int game_reps = 0;
    for(uint64_t hist_hash : game_history_hashes) if(hist_hash == pos.zobrist_hash) game_reps++;
    if(game_reps >= 2 && ply > 0) return 0;
    if (pos.halfmove_clock >= 100 && ply > 0) return 0;

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth = std::max(depth + 1, depth);

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score;
    }
    
    // --- NEW: Futility Pruning ---
    if (!is_pv_node && !in_check && depth <= FUTILITY_MAX_DEPTH) {
        int static_eval = evaluate(pos);
        if (static_eval - futility_margins[depth] >= beta) {
            return beta; // Static eval is high enough to cause a cutoff
        }
    }


    // Null Move Pruning (NMP)
    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 &&
        evaluate(pos) >= beta) {
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
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false, current_search_path_hashes);
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
    score_moves(pos, moves, tt_move, ply);

    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;

    current_search_path_hashes.push_back(pos.zobrist_hash);

    for (int i = 0; i < (int)moves.size(); ++i) {
        const Move& current_move = moves[i];
        
        // --- Futility Pruning (move loop part) ---
        if (!is_pv_node && !in_check && depth <= FUTILITY_MAX_DEPTH && alpha > -MATE_THRESHOLD) {
             int static_eval = evaluate(pos);
             if (static_eval + futility_margins[depth] <= alpha) {
                // Prune if move is quiet (not capture, not promo)
                if (pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) {
                    continue;
                }
             }
        }
        
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        // PVS
        if (legal_moves_played == 1) {
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_search_path_hashes);
        } else {
            // LMR
            int R_lmr = 0;
            if (depth >= 3 && i >= (is_pv_node ? 3 : 2) && !in_check &&
                pos.piece_on_sq(current_move.to) == NO_PIECE &&
                current_move.promotion == NO_PIECE) {
                R_lmr = 1;
                if (depth >= 5 && i >= (is_pv_node ? 5 : 4)) R_lmr = (depth > 7 ? 2 : 1);
                R_lmr = std::min(R_lmr, depth - 2);
                if (R_lmr < 0) R_lmr = 0;
            }

            score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);

            if (R_lmr > 0 && score > alpha) {
                 score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);
            }
            if (score > alpha && score < beta) {
                 score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, current_search_path_hashes);
            }
        }

        if (stop_search_flag) { current_search_path_hashes.pop_back(); return 0; }

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
                        if(history_heuristic[pos.side_to_move][current_move.from][current_move.to] < (30000 - depth*depth) )
                            history_heuristic[pos.side_to_move][current_move.from][current_move.to] += depth * depth;
                    }
                    current_search_path_hashes.pop_back();
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found);
                    return beta;
                }
            }
        }
    }
    current_search_path_hashes.pop_back();

    if (legal_moves_played == 0) {
        return in_check ? (-MATE_SCORE + ply) : 0;
    }

    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, ply, best_score, final_bound_type, best_move_found);
    return best_score;
}


// --- UCI ---
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

    ss >> part;
    pos.castling_rights = 0;
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
        } else {
            pos.ep_square = -1;
        }
    } else {
        pos.ep_square = -1;
    }

    if (ss >> part) { try {pos.halfmove_clock = std::stoi(part);} catch(...) {pos.halfmove_clock = 0;} }
    else pos.halfmove_clock = 0;

    if (ss >> part) { try {pos.fullmove_number = std::stoi(part);} catch(...){pos.fullmove_number = 1;} }
    else pos.fullmove_number = 1;

    pos.ply = 0;
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_hashes.clear();
}

Move parse_uci_move_from_string(const Position& current_pos, const std::string& uci_move_str) {
    Move m = NULL_MOVE;
    if (uci_move_str.length() < 4 || uci_move_str.length() > 5) return m;
    if (uci_move_str == "0000") return NULL_MOVE;


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
            std::cout << "id name Amira 0.4\n";
            std::cout << "id author ChessTubeTree\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 1024\n";
            std::cout << "option name TimeUsageDivisor type spin default 25 min 10 max 50\n";
            std::cout << "option name IncrementUsagePercent type spin default 80 min 0 max 100\n";
            std::cout << "option name MaxTimeUsagePercent type spin default 80 min 40 max 100\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str_val;
            ss >> name_token;
            if (name_token == "name") {
                ss >> name_str;
                ss >> value_token;
                ss >> value_str_val;

                if (name_str == "Hash") {
                    try {
                        int parsed_size = std::stoi(value_str_val);
                        g_configured_tt_size_mb = std::max(0, std::min(parsed_size, 1024));
                    } catch (...) { }
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true;
                } else if (name_str == "TimeUsageDivisor") {
                    try { g_time_usage_divisor = std::stoi(value_str_val); } catch(...) {}
                } else if (name_str == "IncrementUsagePercent") {
                    try { g_increment_usage_percent = std::stoi(value_str_val); } catch(...) {}
                } else if (name_str == "MaxTimeUsagePercent") {
                    try { g_max_time_usage_percent = std::stoi(value_str_val); } catch(...) {}
                }
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) {
                 init_tt(g_configured_tt_size_mb);
                 g_tt_is_initialized = true;
            } else {
                 clear_tt();
            }
            reset_killers_and_history();
            game_history_hashes.clear();
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                std::string next_token_check;
                if (ss >> next_token_check) {
                    if (next_token_check == "moves") token = "moves";
                    else {
                        ss.clear();
                        ss.seekg(-(std::streamoff)next_token_check.length(), std::ios_base::cur);
                        token = "";
                    }
                } else token = "";

            } else if (token == "fen") {
                std::string temp_fen_part;
                while (ss >> temp_fen_part) {
                    if (temp_fen_part == "moves") {
                        token = "moves";
                        break;
                    }
                    fen_str_collector += temp_fen_part + " ";
                }
                if (!fen_str_collector.empty()) fen_str_collector.pop_back();
                parse_fen(uci_root_pos, fen_str_collector);
            }

            if(game_history_hashes.empty() || game_history_hashes.back() != uci_root_pos.zobrist_hash) {
                 game_history_hashes.push_back(uci_root_pos.zobrist_hash);
            }


            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && move_str_uci != "0000") break;
                    if (m.is_null() && move_str_uci == "0000") break;

                    bool legal;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                    game_history_hashes.push_back(uci_root_pos.zobrist_hash);
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }

            std::vector<Move> root_pseudo_moves;
            generate_moves(uci_root_pos, root_pseudo_moves);
            std::vector<Move> root_legal_moves;
            for (const Move& m : root_pseudo_moves) {
                bool is_legal_flag;
                make_move(uci_root_pos, m, is_legal_flag);
                if (is_legal_flag) {
                    root_legal_moves.push_back(m);
                }
            }

            if (root_legal_moves.size() == 1) {
                std::cout << "bestmove " << move_to_uci(root_legal_moves[0]) << std::endl;
                continue;
            }
            if (root_legal_moves.empty()) {
                std::cout << "bestmove 0000" << std::endl;
                continue;
            }

            int wtime = -1, btime = -1, winc = 0, binc = 0, movestogo = 0;
            long long fixed_time_per_move = -1;
            int max_depth_to_search = MAX_PLY;

            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime;
                else if (go_param == "btime") ss >> btime;
                else if (go_param == "winc") ss >> winc;
                else if (go_param == "binc") ss >> binc;
                else if (go_param == "movestogo") ss >> movestogo;
                else if (go_param == "movetime") ss >> fixed_time_per_move;
                else if (go_param == "depth") ss >> max_depth_to_search;
            }

            if (fixed_time_per_move != -1) {
                search_budget_ms = std::max(10LL, fixed_time_per_move - 50);
            } else {
                int my_time = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;
                int my_inc = (uci_root_pos.side_to_move == WHITE) ? winc : binc;

                if (my_time != -1) {
                    long long base_time_slice;
                    if (movestogo > 0 && movestogo < 40) {
                        base_time_slice = my_time / std::max(1, movestogo);
                    } else {
                        base_time_slice = my_time / std::max(1, g_time_usage_divisor);
                    }
                    search_budget_ms = base_time_slice + (long long)(my_inc * (g_increment_usage_percent / 100.0)) - 50;
                    
                    if (my_time > 100 && search_budget_ms > my_time * (g_max_time_usage_percent / 100.0)) {
                        search_budget_ms = (long long)(my_time * (g_max_time_usage_percent / 100.0));
                    }
                } else {
                    search_budget_ms = 2000;
                }
            }
            if (search_budget_ms <= 0) search_budget_ms = 50;

            search_start_timepoint = std::chrono::steady_clock::now();
            reset_search_state();

            uci_best_move_overall = NULL_MOVE;
            int best_score_overall = 0;
            std::vector<uint64_t> root_path_hashes;

            int aspiration_alpha = -INF_SCORE;
            int aspiration_beta = INF_SCORE;
            int aspiration_window_delta = 25;

            for (int depth = 1; depth <= max_depth_to_search; ++depth) {
                int current_score;
                if (depth <= 1) {
                     current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true, root_path_hashes);
                } else {
                    current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, root_path_hashes);
                    if (!stop_search_flag && (current_score <= aspiration_alpha || current_score >= aspiration_beta)) {
                        aspiration_alpha = -INF_SCORE;
                        aspiration_beta = INF_SCORE;
                        current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, root_path_hashes);
                    }
                }

                if (stop_search_flag && depth > 1) break;

                if (abs(current_score) < MATE_THRESHOLD && current_score > -INF_SCORE && current_score < INF_SCORE) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 3 + 5;
                    if (aspiration_window_delta > 300) aspiration_window_delta = 300;
                } else {
                    aspiration_alpha = -INF_SCORE;
                    aspiration_beta = INF_SCORE;
                    aspiration_window_delta = 50;
                }

                Move tt_root_move = NULL_MOVE; int tt_root_score;
                int dummy_alpha = -INF_SCORE, dummy_beta = INF_SCORE;
                if (probe_tt(uci_root_pos.zobrist_hash, depth, 0, dummy_alpha, dummy_beta, tt_root_move, tt_root_score)) {
                     if (!tt_root_move.is_null()) uci_best_move_overall = tt_root_move;
                     best_score_overall = current_score;
                } else {
                     best_score_overall = current_score;
                     TTEntry root_entry_check = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                     if (root_entry_check.hash == uci_root_pos.zobrist_hash && !root_entry_check.best_move.is_null()) {
                         uci_best_move_overall = root_entry_check.best_move;
                     }
                }

                auto now_tp = std::chrono::steady_clock::now();
                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_tp - search_start_timepoint).count();
                if (elapsed_ms < 0) elapsed_ms = 0;

                std::cout << "info depth " << depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) std::cout << " mate " << (MATE_SCORE - best_score_overall + 1)/2 ;
                else if (best_score_overall < -MATE_THRESHOLD) std::cout << " mate " << -(MATE_SCORE + best_score_overall +1)/2;
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0 && nodes_searched > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);

                if (!uci_best_move_overall.is_null()) {
                    std::cout << " pv";
                    Position temp_pos = uci_root_pos;
                    for (int pv_idx = 0; pv_idx < depth; ++pv_idx) {
                        Move pv_m; int pv_s; int pv_a = -INF_SCORE, pv_b = INF_SCORE;
                        if (probe_tt(temp_pos.zobrist_hash, 1, 0, pv_a, pv_b, pv_m, pv_s) && !pv_m.is_null()) {
                            bool legal_pv;
                            Position next_temp_pos = make_move(temp_pos, pv_m, legal_pv);
                            if (legal_pv) {
                                std::cout << " " << move_to_uci(pv_m);
                                temp_pos = next_temp_pos;
                            } else {
                                if (pv_idx == 0) std::cout << " " << move_to_uci(uci_best_move_overall);
                                break;
                            }
                        } else {
                            if (pv_idx == 0) std::cout << " " << move_to_uci(uci_best_move_overall);
                            break;
                        }
                        if (stop_search_flag) break;
                    }
                }
                std::cout << std::endl;

                if (abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break;
                if (search_budget_ms > 0 && elapsed_ms > 0 && depth > 1) {
                    if (elapsed_ms * 2.5 > search_budget_ms && depth > 3) break;
                    else if (elapsed_ms * 1.8 > search_budget_ms ) break;
                }
                 if (depth >= max_depth_to_search) break;
            }

            if (!uci_best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            } else {
                std::vector<Move> legal_moves_fallback;
                generate_moves(uci_root_pos, legal_moves_fallback);
                bool found_one_legal_fallback = false;
                for(const auto& m_fall : legal_moves_fallback) {
                    bool is_leg_fall;
                    make_move(uci_root_pos, m_fall, is_leg_fall);
                    if(is_leg_fall) {
                        std::cout << "bestmove " << move_to_uci(m_fall) << std::endl;
                        found_one_legal_fallback = true;
                        break;
                    }
                }
                if (!found_one_legal_fallback) {
                     std::cout << "bestmove 0000\n" << std::flush;
                }
            }

        } else if (token == "quit" || token == "stop") {
            stop_search_flag = true;
            if (token == "quit") break;
        }
    }
}

int main(int argc, char* argv[]) {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    init_zobrist();
    init_attack_tables();
    init_eval_masks();
    reset_killers_and_history();

    uci_loop();
    return 0;
}
