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
int generate_moves(const Position& pos, Move* moves_list, bool captures_only);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);


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
            if (abs(d) == 8 && c_curr != c_prev) break; // Vertical wrap-around

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

// --- Move Generation --- //
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
                    !get_bit(occupied, E1_SQ + 1) && !get_bit(occupied, E1_SQ + 2) && // F1, G1 empty
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ + 1, BLACK) && !is_square_attacked(pos, E1_SQ + 2, BLACK)) {
                    moves_list[move_count++] = {king_sq_idx, E1_SQ + 2}; // King to G1
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ - 1) && !get_bit(occupied, E1_SQ - 2) && !get_bit(occupied, E1_SQ - 3) && // D1, C1, B1 empty
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ - 1, BLACK) && !is_square_attacked(pos, E1_SQ - 2, BLACK)) {
                    moves_list[move_count++] = {king_sq_idx, E1_SQ - 2}; // King to C1
                }
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ + 1) && !get_bit(occupied, E8_SQ + 2) && // F8, G8 empty
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ + 1, WHITE) && !is_square_attacked(pos, E8_SQ + 2, WHITE)) {
                    moves_list[move_count++] = {king_sq_idx, E8_SQ + 2}; // King to G8
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ - 1) && !get_bit(occupied, E8_SQ - 2) && !get_bit(occupied, E8_SQ - 3) && // D8, C8, B8 empty
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ - 1, WHITE) && !is_square_attacked(pos, E8_SQ - 2, WHITE)) {
                    moves_list[move_count++] = {king_sq_idx, E8_SQ - 2}; // King to C8
                }
            }
        }
    }
    return move_count;
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
const int piece_values_mg[6] = {100, 320, 330, 500, 900, 0}; // P,N,B,R,Q,K
const int piece_values_eg[6] = {120, 320, 330, 530, 950, 0};

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
const int king_pst_mg[64] = { // King safety oriented for midgame
     20, 30, 10,  0,  0, 10, 30, 20,  // Castled king on G1/C1 gets +30 (mirrored for G8/C8)
     20, 20,  0,  0,  0,  0, 20, 20,
    -10,-20,-20,-20,-20,-20,-20,-10, -20,-30,-30,-40,-40,-30,-30,-20,
    -30,-40,-40,-50,-50,-40,-40,-30, -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30, -30,-40,-40,-50,-50,-40,-40,-30
};
const int king_pst_eg[64] = { // King activity for endgame
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
uint64_t white_passed_pawn_block_mask[64];
uint64_t black_passed_pawn_block_mask[64];
const int passed_pawn_bonus_mg[8] = {0, 5, 15, 25, 40, 60, 80, 0}; // Bonus by rank (0-indexed from own side)
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
        int r = sq / 8;
        int f = sq % 8;

        for (int cur_r = r + 1; cur_r < 8; ++cur_r) { // Squares in front for White
            white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) white_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f + 1));
        }
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) { // Squares in front for Black
            black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) black_passed_pawn_block_mask[sq] |= set_bit(cur_r * 8 + (f + 1));
        }
    }
}

// Checks for draw by insufficient material.
bool is_insufficient_material(const Position& pos) {
    // If there are any pawns, rooks, or queens, it's not a draw by insufficient material.
    if (pos.piece_bb[PAWN] != 0 || pos.piece_bb[ROOK] != 0 || pos.piece_bb[QUEEN] != 0) {
        return false;
    }

    // Count minor pieces for both sides
    int white_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[WHITE]);
    int white_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]);
    int black_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[BLACK]);
    int black_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]);

    int white_minors = white_knights + white_bishops;
    int black_minors = black_knights + black_bishops;

    // Case: K vs K
    if (white_minors == 0 && black_minors == 0) return true;

    // Case: K + minor vs K
    if ((white_minors == 1 && black_minors == 0) || (white_minors == 0 && black_minors == 1)) {
        return true;
    }

    // Case: K + minor vs K + minor (covers K+N vs K+N, K+N vs K+B, K+B vs K+B)
    if (white_minors == 1 && black_minors == 1) {
        return true;
    }

    // Case: K+N+N vs K (generally treated as a draw)
    if ((white_minors == 2 && black_minors == 0 && white_knights == 2) ||
        (white_minors == 0 && black_minors == 2 && black_knights == 2)) {
        return true;
    }

    // All other cases (like K+B+N vs K, K+B+B vs K) are not considered drawn by default.
    return false;
}


int evaluate(const Position& pos) {
    // Check for insufficient material draw at the beginning of evaluation.
    if (is_insufficient_material(pos)) {
        return 0; // Draw score
    }

    int mg_score = 0;
    int eg_score = 0;
    int game_phase = 0;

    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;

        uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[current_eval_color];
        uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1 - current_eval_color];

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
        }
        if (pop_count(pos.piece_bb[BISHOP] & pos.color_bb[current_eval_color]) >= 2) {
            mg_score += side_multiplier * 30; eg_score += side_multiplier * 50;
        }

        // --- King Safety and Pawn Shield ---
        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[current_eval_color]);
        if (king_sq != -1) {
            int king_rank = king_sq / 8;
            int king_file = king_sq % 8;

            // PAWN SHIELD
            int pawn_shield_score = 0;
            const int shield_pawn_bonus = 12;
            const int missing_shield_pawn_penalty = -18;
            const int open_file_penalty_adj = -10; // Additional for open file on missing shield

            int shield_candidate_sqs[3] = {-1, -1, -1};
            bool relevant_castled_pos = false;

            if (current_eval_color == WHITE) {
                if (king_sq == G1_SQ) { // White Kingside Castle
                    shield_candidate_sqs[0] = G1_SQ + 7; shield_candidate_sqs[1] = G1_SQ + 8; shield_candidate_sqs[2] = G1_SQ + 9; // F2,G2,H2
                    relevant_castled_pos = true;
                } else if (king_sq == C1_SQ) { // White Queenside Castle
                    shield_candidate_sqs[0] = C1_SQ + 7; shield_candidate_sqs[1] = C1_SQ + 8; shield_candidate_sqs[2] = C1_SQ + 9; // B2,C2,D2
                    relevant_castled_pos = true;
                }
            } else { // BLACK
                if (king_sq == G8_SQ) { // Black Kingside Castle
                    shield_candidate_sqs[0] = G8_SQ - 9; shield_candidate_sqs[1] = G8_SQ - 8; shield_candidate_sqs[2] = G8_SQ - 7; // F7,G7,H7
                    relevant_castled_pos = true;
                } else if (king_sq == C8_SQ) { // Black Queenside Castle
                    shield_candidate_sqs[0] = C8_SQ - 9; shield_candidate_sqs[1] = C8_SQ - 8; shield_candidate_sqs[2] = C8_SQ - 7; // B7,C7,D7
                    relevant_castled_pos = true;
                }
            }

            if (relevant_castled_pos) {
                for (int sq_idx = 0; sq_idx < 3; ++sq_idx) {
                    int shield_sq = shield_candidate_sqs[sq_idx];
                    // Ensure shield_sq is on board (it should be by construction from G1/C1 etc.)
                    if (shield_sq >=0 && shield_sq < 64) {
                        if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) {
                            pawn_shield_score += shield_pawn_bonus;
                        } else {
                            pawn_shield_score += missing_shield_pawn_penalty;
                            int shield_f = shield_sq % 8;
                            bool friendly_pawn_on_shield_file = (file_bb_mask[shield_f] & all_friendly_pawns) != 0;
                            bool enemy_pawn_on_shield_file = (file_bb_mask[shield_f] & all_enemy_pawns) != 0;
                            if (!friendly_pawn_on_shield_file && !enemy_pawn_on_shield_file) {
                                pawn_shield_score += open_file_penalty_adj;
                            }
                        }
                    }
                }
            } else { // General pawn shield for non-standard king positions
                int base_shield_rank = (current_eval_color == WHITE) ? (king_rank + 1) : (king_rank - 1);
                if (base_shield_rank >= 0 && base_shield_rank < 8) {
                    for (int df = -1; df <= 1; ++df) {
                        int current_shield_file = king_file + df;
                        if (current_shield_file >= 0 && current_shield_file < 8) {
                            int shield_sq = base_shield_rank * 8 + current_shield_file;
                            if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) {
                                pawn_shield_score += shield_pawn_bonus / 2; // Smaller bonus
                            } else {
                                pawn_shield_score += missing_shield_pawn_penalty / 2;
                                int shield_f = shield_sq % 8;
                                bool friendly_pawn_on_gen_shield_file = (file_bb_mask[shield_f] & all_friendly_pawns) != 0;
                                bool enemy_pawn_on_gen_shield_file = (file_bb_mask[shield_f] & all_enemy_pawns) != 0;
                                if (!friendly_pawn_on_gen_shield_file && !enemy_pawn_on_gen_shield_file) {
                                    pawn_shield_score += open_file_penalty_adj / 2;
                                }
                            }
                        }
                    }
                }
            }
            mg_score += side_multiplier * pawn_shield_score;
            // eg_score += side_multiplier * (pawn_shield_score / 3); // Less emphasis in EG for shield

            // KING SAFETY (ATTACKS ON KING'S IMMEDIATE VICINITY)
            int king_attack_score = 0; // Aggregated score of attacks
            uint64_t king_1_ring_zone = king_attacks_bb[king_sq]; // Squares king can move to
            uint64_t enemy_player_pieces = pos.color_bb[1 - current_eval_color];
            uint64_t occupied_bb = pos.get_occupied_bb();

            uint64_t enemy_knights = pos.piece_bb[KNIGHT] & enemy_player_pieces;
            while(enemy_knights) {
                int attacker_sq = lsb_index(enemy_knights); enemy_knights &= enemy_knights - 1;
                if (knight_attacks_bb[attacker_sq] & king_1_ring_zone) king_attack_score += 2;
            }
            uint64_t enemy_bishops = pos.piece_bb[BISHOP] & enemy_player_pieces;
            while(enemy_bishops) {
                int attacker_sq = lsb_index(enemy_bishops); enemy_bishops &= enemy_bishops - 1;
                if (get_bishop_attacks_from_sq(attacker_sq, occupied_bb) & king_1_ring_zone) king_attack_score += 2;
            }
            uint64_t enemy_rooks = pos.piece_bb[ROOK] & enemy_player_pieces;
            while(enemy_rooks) {
                int attacker_sq = lsb_index(enemy_rooks); enemy_rooks &= enemy_rooks - 1;
                if (get_rook_attacks_from_sq(attacker_sq, occupied_bb) & king_1_ring_zone) king_attack_score += 3;
            }
            uint64_t enemy_queens = pos.piece_bb[QUEEN] & enemy_player_pieces;
            while(enemy_queens) {
                int attacker_sq = lsb_index(enemy_queens); enemy_queens &= enemy_queens - 1;
                if (get_bishop_attacks_from_sq(attacker_sq, occupied_bb) & king_1_ring_zone) king_attack_score += 4; // Queen as bishop
                if (get_rook_attacks_from_sq(attacker_sq, occupied_bb) & king_1_ring_zone) king_attack_score += 5;   // Queen as rook
            }
            uint64_t temp_king_zone_for_pawns = king_1_ring_zone;
            while(temp_king_zone_for_pawns) {
                int sq_in_kz = lsb_index(temp_king_zone_for_pawns); temp_king_zone_for_pawns &= temp_king_zone_for_pawns -1;
                if (pawn_attacks_bb[current_eval_color][sq_in_kz] & all_enemy_pawns) { // Enemy pawns that attack this king zone square
                    king_attack_score += 1;
                }
            }
            int danger_idx = std::min(king_attack_score, 14); // Cap index for penalty table
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
     if (power_of_2_entries == 0) { // Should not happen if num_entries > 0
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
bool stop_search_flag = false;
uint64_t nodes_searched = 0;

// Time management globals
std::chrono::steady_clock::time_point soft_limit_timepoint;
std::chrono::steady_clock::time_point hard_limit_timepoint;
bool use_time_limits = false;

Move killer_moves[MAX_PLY][2];
int history_heuristic[2][64][64];
std::vector<uint64_t> game_history_hashes; // Stores hashes of positions played in the game for 3-fold repetition

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
    std::memset(history_heuristic, 0, sizeof(history_heuristic));
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) { // Check time every 2048 nodes
        if (use_time_limits) {
            if (std::chrono::steady_clock::now() > hard_limit_timepoint) {
                stop_search_flag = true;
                return true;
            }
        }
    }
    return false;
}

const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0}; // P,N,B,R,Q,K,NO_PIECE

void score_moves(const Position& pos, Move* moves, int num_moves, const Move& tt_move, int ply) {
    for (int i = 0; i < num_moves; ++i) {
        Move& m = moves[i];
        if (!tt_move.is_null() && m == tt_move) {
            m.score = 2000000; // TT move gets highest priority
        } else {
            Piece moved_piece = pos.piece_on_sq(m.from);
            Piece captured_piece = pos.piece_on_sq(m.to);

            if (captured_piece != NO_PIECE) {
                // MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
                m.score = 1000000 + (mvv_lva_piece_values[captured_piece] * 100) - mvv_lva_piece_values[moved_piece];
            } else if (m.promotion != NO_PIECE) {
                 m.score = 900000 + mvv_lva_piece_values[m.promotion];
            } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
                m.score = 800000; // First killer move
            } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
                m.score = 700000; // Second killer move
            } else {
                // History heuristic for quiet moves
                m.score = history_heuristic[pos.side_to_move][m.from][m.to];
            }
        }
    }
    // Sort moves by score in descending order
    std::sort(moves, moves + num_moves, [](const Move& a, const Move& b){ return a.score > b.score; });
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return evaluate(pos);

    // Check for repetitions in qsearch too, although less critical here than in main search
    // For simplicity and focus on qsearch's primary role (captures), this is omitted here.

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    int stand_pat_score;

    if (in_check) {
        stand_pat_score = -INF_SCORE + ply; // If in check, we must make a move
    } else {
        stand_pat_score = evaluate(pos);
        if (stand_pat_score >= beta) return beta; // Fail-high
        if (alpha < stand_pat_score) alpha = stand_pat_score;
    }

    Move q_moves[256];
    int num_q_moves = generate_moves(pos, q_moves, !in_check);

    Move dummy_tt_move = NULL_MOVE; // No TT move in qsearch for ordering in this simplified version
    score_moves(pos, q_moves, num_q_moves, dummy_tt_move, ply);

    int legal_moves_in_qsearch = 0;
    for (int i = 0; i < num_q_moves; ++i) {
        const Move& cap_move = q_moves[i];
        bool legal;
        Position next_pos = make_move(pos, cap_move, legal);
        if (!legal) continue;
        legal_moves_in_qsearch++;

        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);

        if (score >= beta) return beta; // Fail-high
        if (score > alpha) alpha = score;
    }

    // If in check and no legal moves found, it's mate
    if (in_check && legal_moves_in_qsearch == 0) {
        return -MATE_SCORE + ply; // Mate score (deeper mates are worse)
    }

    return alpha;
}


int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, std::vector<uint64_t>& current_search_path_hashes) {
    nodes_searched++;
    if (check_time()) return 0; // Search budget exceeded
    if (ply >= MAX_PLY -1) return evaluate(pos); // Max ply reached

    // Repetition detection within the current search path
    for (uint64_t path_hash : current_search_path_hashes) {
        if (path_hash == pos.zobrist_hash) return 0; // Draw by repetition in current path
    }
    // Check against game history for 3-fold repetition
    int game_reps = 0;
    for(uint64_t hist_hash : game_history_hashes) if(hist_hash == pos.zobrist_hash) game_reps++;
    if(game_reps >= 2 && ply > 0) return 0; // Draw by 3-fold repetition in game

    // 50-move rule
    if (pos.halfmove_clock >= 100 && ply > 0) return 0; // Draw

    // Check for insufficient material draw
    if (ply > 0 && is_insufficient_material(pos)) return 0;


    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    // Check extension
    if (in_check) depth = std::max(depth + 1, depth); // Extend search if in check

    // Leaf node or quiescence search
    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score; // TT Hit
    }

    // Null Move Pruning (NMP)
    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 && // Have non-pawn/king pieces
        evaluate(pos) >= beta) { // Static eval indicates a likely cutoff
            Position null_next_pos = pos;
            null_next_pos.side_to_move = 1 - pos.side_to_move; // Switch side

            // Update Zobrist for null move
            null_next_pos.zobrist_hash = pos.zobrist_hash;
            if (pos.ep_square != -1) null_next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];
            null_next_pos.ep_square = -1; // EP square is lost after null move
            null_next_pos.zobrist_hash ^= zobrist_ep[64]; // XOR out old EP, XOR in no EP
            null_next_pos.zobrist_hash ^= zobrist_side_to_move; // Flip side to move
            null_next_pos.ply = pos.ply + 1; // Increment ply

            int R_nmp = (depth > 6) ? 3 : 2; // Adaptive reduction for NMP
            current_search_path_hashes.push_back(pos.zobrist_hash); // Add current pos for child's rep check
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false, current_search_path_hashes);
            current_search_path_hashes.pop_back(); // Backtrack

            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 if (null_score >= MATE_THRESHOLD) null_score = beta; // Avoid propagating unverified mate scores
                 store_tt(pos.zobrist_hash, depth, ply, null_score, TT_LOWER, NULL_MOVE); // Store with lower bound
                 return beta; // Null move cutoff
            }
    }

    Move moves[256];
    int num_moves = generate_moves(pos, moves, false);
    score_moves(pos, moves, num_moves, tt_move, ply);

    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;

    current_search_path_hashes.push_back(pos.zobrist_hash); // Add current position to path for children

    for (int i = 0; i < num_moves; ++i) {
        const Move& current_move = moves[i];
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        // Principal Variation Search (PVS)
        if (legal_moves_played == 1) { // First move is searched with full window
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_search_path_hashes);
        } else { // Subsequent moves: LMR then PVS re-search if necessary
            // Late Move Reduction (LMR)
            int R_lmr = 0;
            if (depth >= 3 && i >= (is_pv_node ? 3 : 2) && // Conditions for LMR
                !in_check && current_move.promotion == NO_PIECE &&
                pos.piece_on_sq(current_move.to) == NO_PIECE && // Not a capture
                current_move.score < 700000) { // Not a killer or TT move or high history score

                R_lmr = 1; // Base reduction
                if (depth >= 5 && i >= (is_pv_node ? 5 : 4)) R_lmr = (depth > 7 ? 2 : 1); // Deeper reduction for later moves/deeper searches
                R_lmr = std::min(R_lmr, depth - 2); // Don't reduce too much
                if (R_lmr < 0) R_lmr = 0;
            }

            // --- LMR/PVS FIX STARTS HERE ---
            // Search with reduced depth and null window
            score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);

            // If LMR failed high (score > alpha) and a reduction was applied, re-search with full depth
            // THIS BLOCK IS REMOVED AS IT'S A REDUNDANT RE-SEARCH WITH A NULL WINDOW
            // if (R_lmr > 0 && score > alpha) {
            //      score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);
            // }
            
            // If the PVS null window search (or the LMR search) failed high, a re-search with the full window is required.
            if (score > alpha && score < beta) { 
                 score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, current_search_path_hashes); // Re-search with full window
            }
            // --- LMR/PVS FIX ENDS HERE ---
        }

        if (stop_search_flag) { current_search_path_hashes.pop_back(); return 0; }

        if (score > best_score) {
            best_score = score;
            best_move_found = current_move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) { // Beta cutoff
                    // Store killer move and update history heuristic for non-captures causing cutoff
                    if (ply < MAX_PLY && pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) {
                        if (!(current_move == killer_moves[ply][0])) {
                            killer_moves[ply][1] = killer_moves[ply][0];
                            killer_moves[ply][0] = current_move;
                        }
                        // Increase history score for this move
                        if(history_heuristic[pos.side_to_move][current_move.from][current_move.to] < (30000 - depth*depth) ) // Cap history
                            history_heuristic[pos.side_to_move][current_move.from][current_move.to] += depth * depth;
                    }
                    current_search_path_hashes.pop_back(); // Backtrack
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found);
                    return beta; // Fail-high
                }
            }
        }
    }
    current_search_path_hashes.pop_back(); // Backtrack from current position

    // Handle stalemate or checkmate
    if (legal_moves_played == 0) {
        return in_check ? (-MATE_SCORE + ply) : 0; // Checkmate or stalemate
    }

    // Store result in TT
    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, ply, best_score, final_bound_type, best_move_found);
    return best_score;
}


// --- UCI ---
Position uci_root_pos;
Move uci_best_move_overall;

void parse_fen(Position& pos, const std::string& fen_str) {
    pos = Position(); // Reset position object
    std::stringstream ss(fen_str);
    std::string part;

    // 1. Piece placement
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

    // 2. Side to move
    ss >> part; pos.side_to_move = (part == "w") ? WHITE : BLACK;

    // 3. Castling rights
    ss >> part;
    pos.castling_rights = 0;
    for (char c : part) {
        if (c == 'K') pos.castling_rights |= WK_CASTLE_MASK;
        else if (c == 'Q') pos.castling_rights |= WQ_CASTLE_MASK;
        else if (c == 'k') pos.castling_rights |= BK_CASTLE_MASK;
        else if (c == 'q') pos.castling_rights |= BQ_CASTLE_MASK;
    }

    // 4. En passant square
    ss >> part;
    if (part != "-") {
        if (part.length() == 2 && part[0] >= 'a' && part[0] <= 'h' && part[1] >= '1' && part[1] <= '8') {
            int ep_file = part[0] - 'a';
            int ep_rank = part[1] - '1';
            pos.ep_square = ep_rank * 8 + ep_file;
        } else {
            pos.ep_square = -1; // Invalid EP string
        }
    } else {
        pos.ep_square = -1;
    }

    // 5. Halfmove clock
    if (ss >> part) { try {pos.halfmove_clock = std::stoi(part);} catch(...) {pos.halfmove_clock = 0;} }
    else pos.halfmove_clock = 0;

    // 6. Fullmove number
    if (ss >> part) { try {pos.fullmove_number = std::stoi(part);} catch(...){pos.fullmove_number = 1;} }
    else pos.fullmove_number = 1;

    pos.ply = 0; // Ply at root is 0
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_hashes.clear(); // Clear game history for a new FEN
}

Move parse_uci_move_from_string(const Position& current_pos, const std::string& uci_move_str) {
    Move m = NULL_MOVE;
    if (uci_move_str.length() < 4 || uci_move_str.length() > 5) return m; // Invalid length
    if (uci_move_str == "0000") return NULL_MOVE; // Null move notation


    m.from = (uci_move_str[0] - 'a') + (uci_move_str[1] - '1') * 8;
    m.to = (uci_move_str[2] - 'a') + (uci_move_str[3] - '1') * 8;

    if (m.from < 0 || m.from > 63 || m.to < 0 || m.to > 63) return NULL_MOVE; // Invalid square

    if (uci_move_str.length() == 5) {
        char promo_char = uci_move_str[4];
        if (promo_char == 'q') m.promotion = QUEEN;
        else if (promo_char == 'n') m.promotion = KNIGHT;
        else if (promo_char == 'b') m.promotion = BISHOP;
        else if (promo_char == 'r') m.promotion = ROOK;
        else return NULL_MOVE; // Invalid promotion character
    }
    // For castling moves, the 'from' and 'to' are king's squares.
    // make_move will handle rook movement internally.
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
    h ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square]; // Use index 64 for no EP
    if (pos.side_to_move == BLACK) h ^= zobrist_side_to_move;
    return h;
}

void uci_loop() {
    std::string line, token;
    // Initialize with start position
    parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    while (std::getline(std::cin, line)) {
        std::istringstream ss(line);
        ss >> token;

        if (token == "uci") {
            std::cout << "id name Amira 0.23\n";
            std::cout << "id author ChessTubeTree\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 1024\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            if (!g_tt_is_initialized) { // Initialize TT if not done yet (e.g., if setoption wasn't called)
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str_val;
            ss >> name_token; // Should be "name"
            if (name_token == "name") {
                ss >> name_str; // e.g., "Hash"
                ss >> value_token; // Should be "value"
                ss >> value_str_val; // e.g., "64"

                if (name_str == "Hash") {
                    try {
                        int parsed_size = std::stoi(value_str_val);
                        g_configured_tt_size_mb = std::max(0, std::min(parsed_size, 1024));
                    } catch (...) { /* ignore parse error, keep default */ }
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true;
                }
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) { // Ensure TT is initialized
                 init_tt(g_configured_tt_size_mb);
                 g_tt_is_initialized = true;
            } else {
                 clear_tt(); // Clear TT for a new game
            }
            reset_killers_and_history();
            game_history_hashes.clear();
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                std::string next_token_check;
                if (ss >> next_token_check) { // Check if "moves" follows startpos
                    if (next_token_check == "moves") token = "moves";
                    else { // Put it back if it's not "moves"
                        ss.clear(); // Clear EOF flags
                        ss.seekg(-(std::streamoff)next_token_check.length(), std::ios_base::cur);
                        token = ""; // No moves to parse for "startpos"
                    }
                } else token = ""; // No "moves" token found

            } else if (token == "fen") {
                std::string temp_fen_part;
                while (ss >> temp_fen_part) {
                    if (temp_fen_part == "moves") {
                        token = "moves"; // Found "moves" token, stop collecting FEN
                        break;
                    }
                    fen_str_collector += temp_fen_part + " ";
                }
                if (!fen_str_collector.empty()) fen_str_collector.pop_back(); // Remove trailing space
                parse_fen(uci_root_pos, fen_str_collector);
            }

            // Add initial position to game history (after fen/startpos, before moves)
            // Important for 3-fold repetition detection from the very first move
            if(game_history_hashes.empty() || game_history_hashes.back() != uci_root_pos.zobrist_hash) {
                 game_history_hashes.push_back(uci_root_pos.zobrist_hash);
            }


            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && move_str_uci != "0000") { /* Invalid move string */ break; }
                    if (m.is_null() && move_str_uci == "0000") { /* UCI nullmove, usually indicates error or end */ break; }

                    bool legal;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) { /* Illegal move received from GUI */ break; }
                    // Add hash of position *after* move to game history
                    game_history_hashes.push_back(uci_root_pos.zobrist_hash);
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) { // Failsafe
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }

            // Check for single legal move case
            Move root_pseudo_moves[256];
            int num_pseudo_moves = generate_moves(uci_root_pos, root_pseudo_moves, false);
            std::vector<Move> root_legal_moves;
            for (int i = 0; i < num_pseudo_moves; ++i) {
                const Move& m = root_pseudo_moves[i];
                bool is_legal_flag;
                make_move(uci_root_pos, m, is_legal_flag); // This call checks legality
                if (is_legal_flag) {
                    root_legal_moves.push_back(m);
                }
            }

            // If there's only one legal move, play it immediately.
            if (root_legal_moves.size() == 1) {
                std::cout << "bestmove " << move_to_uci(root_legal_moves[0]) << std::endl;
                continue; // Skip the rest of the "go" command logic
            }

            // If there are no legal moves (checkmate/stalemate), output null move.
            if (root_legal_moves.empty()) {
                std::cout << "bestmove 0000" << std::endl;
                continue; // Skip the rest of the "go" command logic
            }

            long long wtime = -1, btime = -1;
            int max_depth_to_search = MAX_PLY; // Default to effectively infinite depth

            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime;
                else if (go_param == "btime") ss >> btime;
                else if (go_param == "depth") ss >> max_depth_to_search;
                // Ignore other time params like winc, binc, movestogo, movetime
            }

            reset_search_state(); // Reset nodes, stop_flag, use_time_limits
            search_start_timepoint = std::chrono::steady_clock::now();

            // Time management
            long long time_alotment_ms = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;

            if (time_alotment_ms != -1) {
                use_time_limits = true;
                // Calculate durations in seconds based on total time, then convert to chrono duration
                double soft_limit_s = time_alotment_ms * 0.000054;
                double hard_limit_s = time_alotment_ms * 0.0004;

                soft_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(soft_limit_s * 1000000.0));
                hard_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(hard_limit_s * 1000000.0));
            } else {
                use_time_limits = false; // Infinite search
            }

            uci_best_move_overall = NULL_MOVE;
            int best_score_overall = 0;
            std::vector<uint64_t> root_path_hashes; // Empty for root call

            // Aspiration Windows
            int aspiration_alpha = -INF_SCORE;
            int aspiration_beta = INF_SCORE;
            int aspiration_window_delta = 25; // Initial aspiration window size

            for (int depth = 1; depth <= max_depth_to_search; ++depth) {
                int current_score;
                if (depth <= 1) { // No aspiration for very shallow depths
                     current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true, root_path_hashes);
                } else {
                    // Search with aspiration window
                    current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, root_path_hashes);
                    // If search failed high or low, re-search with wider/full window
                    if (!stop_search_flag && (current_score <= aspiration_alpha || current_score >= aspiration_beta)) {
                        aspiration_alpha = -INF_SCORE; // Reset for full search
                        aspiration_beta = INF_SCORE;
                        current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, root_path_hashes);
                    }
                }

                if (stop_search_flag && depth > 1) break; // Time's up, use results from previous iteration

                // Update aspiration window for next iteration if search was successful
                if (abs(current_score) < MATE_THRESHOLD && current_score > -INF_SCORE && current_score < INF_SCORE) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 3 + 5; // Increase window slightly
                    if (aspiration_window_delta > 300) aspiration_window_delta = 300; // Cap window size
                } else { // Mate score or other boundary, reset to full window
                    aspiration_alpha = -INF_SCORE;
                    aspiration_beta = INF_SCORE;
                    aspiration_window_delta = 50; // Reset delta for next non-mate score
                }

                Move tt_root_move = NULL_MOVE; int tt_root_score;
                int dummy_alpha = -INF_SCORE, dummy_beta = INF_SCORE;
                // Retrieve best move from TT for this depth. Search populates it.
                if (probe_tt(uci_root_pos.zobrist_hash, depth, 0, dummy_alpha, dummy_beta, tt_root_move, tt_root_score)) {
                     if (!tt_root_move.is_null()) uci_best_move_overall = tt_root_move;
                     best_score_overall = current_score; // The score from search is more reliable than TT's raw score here
                } else {
                     // Fallback if probe_tt doesn't give a move (e.g. depth mismatch but entry exists)
                     best_score_overall = current_score;
                     // Try to get best move directly if hash matches, even if depth criteria not met by probe_tt
                     TTEntry root_entry_check = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                     if (root_entry_check.hash == uci_root_pos.zobrist_hash && !root_entry_check.best_move.is_null()) {
                         uci_best_move_overall = root_entry_check.best_move;
                     }
                }

                auto now_tp = std::chrono::steady_clock::now();
                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_tp - search_start_timepoint).count();
                if (elapsed_ms < 0) elapsed_ms = 0; // Should not happen

                std::cout << "info depth " << depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) std::cout << " mate " << (MATE_SCORE - best_score_overall + 1)/2 ; // White mates
                else if (best_score_overall < -MATE_THRESHOLD) std::cout << " mate " << -(MATE_SCORE + best_score_overall +1)/2; // Black mates
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0 && nodes_searched > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);

                // Print PV
                if (!uci_best_move_overall.is_null()) {
                    std::cout << " pv";
                    Position temp_pos = uci_root_pos;
                    for (int pv_idx = 0; pv_idx < depth; ++pv_idx) {
                        Move pv_m; int pv_s; int pv_a = -INF_SCORE, pv_b = INF_SCORE;
                        // Probe TT for the best move from the current temp_pos
                        // We only need *a* move, so depth 1 probe is fine if deeper not available
                        if (probe_tt(temp_pos.zobrist_hash, 1, 0, pv_a, pv_b, pv_m, pv_s) && !pv_m.is_null()) {
                            bool legal_pv;
                            Position next_temp_pos = make_move(temp_pos, pv_m, legal_pv);
                            if (legal_pv) {
                                std::cout << " " << move_to_uci(pv_m);
                                temp_pos = next_temp_pos;
                            } else { // Should not happen if TT stores legal moves
                                if (pv_idx == 0) std::cout << " " << move_to_uci(uci_best_move_overall); // At least print first move
                                break;
                            }
                        } else { // No move from TT for this PV node
                            if (pv_idx == 0) std::cout << " " << move_to_uci(uci_best_move_overall); // Print first move if others not found
                            break;
                        }
                        if (stop_search_flag) break; // Stop printing PV if search is stopping
                    }
                }
                std::cout << std::endl; // End of info line

                // Soft time limit check
                if (use_time_limits && std::chrono::steady_clock::now() > soft_limit_timepoint) {
                    break;
                }

                // Early exit conditions for iterative deepening
                if (abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break; // Found a mate
                if (depth >= max_depth_to_search) break; // Reached UCI depth limit
            }

            // Output bestmove
            if (!uci_best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            } else {
                // Fallback: if no move found (e.g. instant timeout or bug), pick first legal move
                Move legal_moves_fallback[256];
                int num_fallback_moves = generate_moves(uci_root_pos, legal_moves_fallback, false);
                bool found_one_legal_fallback = false;
                for(int i = 0; i < num_fallback_moves; ++i) {
                    const auto& m_fall = legal_moves_fallback[i];
                    bool is_leg_fall;
                    Position temp_p = make_move(uci_root_pos, m_fall, is_leg_fall); // Check legality
                    if(is_leg_fall) {
                        std::cout << "bestmove " << move_to_uci(m_fall) << std::endl;
                        found_one_legal_fallback = true;
                        break;
                    }
                }
                if (!found_one_legal_fallback) { // No legal moves at all (checkmate/stalemate)
                     std::cout << "bestmove 0000\n" << std::flush; // UCI null move
                }
            }

        } else if (token == "quit" || token == "stop") {
            stop_search_flag = true; // Signal search to stop
            if (token == "quit") break; // Exit UCI loop
        }
        // Unknown command, ignore
    }
}

int main(int argc, char* argv[]) {
    std::ios_base::sync_with_stdio(false); // Faster I/O
    std::cin.tie(NULL);

    init_zobrist();
    init_attack_tables();
    init_eval_masks();
    reset_killers_and_history(); // Initialize killers and history table

    // TT will be initialized on first "isready" or "setoption" or "go"

    uci_loop();
    return 0;
}
