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
constexpr int A1_SQ = 0; constexpr int B1_SQ = 1; constexpr int C1_SQ = 2; constexpr int D1_SQ = 3; constexpr int E1_SQ = 4; constexpr int F1_SQ = 5; constexpr int G1_SQ = 6; constexpr int H1_SQ = 7;
constexpr int A2_SQ = 8; constexpr int B2_SQ = 9; constexpr int C2_SQ = 10; constexpr int D2_SQ = 11; constexpr int E2_SQ = 12; constexpr int F2_SQ = 13; constexpr int G2_SQ = 14; constexpr int H2_SQ = 15;
constexpr int A7_SQ = 48; constexpr int B7_SQ = 49; constexpr int C7_SQ = 50; constexpr int D7_SQ = 51; constexpr int E7_SQ = 52; constexpr int F7_SQ = 53; constexpr int G7_SQ = 54; constexpr int H7_SQ = 55;
constexpr int A8_SQ = 56; constexpr int B8_SQ = 57; constexpr int C8_SQ = 58; constexpr int D8_SQ = 59; constexpr int E8_SQ = 60; constexpr int F8_SQ = 61; constexpr int G8_SQ = 62; constexpr int H8_SQ = 63;


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
    // Fallback implementation, significantly slower.
    int count = 0;
    while (!((bb >> count) & 1)) {
        count++;
        if (count >= 64) return -1;
    }
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
    Piece board[64];
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
        for (int i = 0; i < 64; ++i) board[i] = NO_PIECE;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }

    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE;
        return board[sq];
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
        int r_start = sq / 8;
        for (int s = sq + d; s >= 0 && s < 64; s += d) {
            int r_curr = s / 8;
            if (abs(d) == 1 && r_curr != r_start) break;

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
        for (int s = sq + d; s >= 0 && s < 64; s += d) {
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

        // Pawn pushes
        if (!captures_only) {
            int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
            if (get_bit(empty_squares, one_step_sq)) {
                if (rank == promotion_rank_idx) {
                    add_move_to_list(moves_list, from, one_step_sq, QUEEN); add_move_to_list(moves_list, from, one_step_sq, ROOK);
                    add_move_to_list(moves_list, from, one_step_sq, BISHOP); add_move_to_list(moves_list, from, one_step_sq, KNIGHT);
                } else {
                    add_move_to_list(moves_list, from, one_step_sq);
                }
                
                int start_rank_idx = (stm == WHITE) ? 1 : 6;
                if (rank == start_rank_idx) {
                    int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                    if (get_bit(empty_squares, two_steps_sq)) {
                        add_move_to_list(moves_list, from, two_steps_sq);
                    }
                }
            }
        }
        // Pawn captures (including EP)
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

    // Castling
    if (!captures_only) {
        int king_sq_idx = lsb_index(pos.piece_bb[KING] & my_pieces);
        if (king_sq_idx != -1) {
             if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, F1_SQ) && !get_bit(occupied, G1_SQ) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, F1_SQ, BLACK) && !is_square_attacked(pos, G1_SQ, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, G1_SQ);
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, D1_SQ) && !get_bit(occupied, C1_SQ) && !get_bit(occupied, B1_SQ) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, D1_SQ, BLACK) && !is_square_attacked(pos, C1_SQ, BLACK)) {
                    add_move_to_list(moves_list, king_sq_idx, C1_SQ);
                }
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, F8_SQ) && !get_bit(occupied, G8_SQ) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, F8_SQ, WHITE) && !is_square_attacked(pos, G8_SQ, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, G8_SQ);
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, D8_SQ) && !get_bit(occupied, C8_SQ) && !get_bit(occupied, B8_SQ) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, D8_SQ, WHITE) && !is_square_attacked(pos, C8_SQ, WHITE)) {
                    add_move_to_list(moves_list, king_sq_idx, C8_SQ);
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

    // --- Move piece ---
    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;
    next_pos.board[move.from] = NO_PIECE;
    next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.from];

    next_pos.halfmove_clock++;

    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        next_pos.halfmove_clock = 0;
    }
    if (piece_moved == PAWN) next_pos.halfmove_clock = 0;

    // --- Handle special moves ---
    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) { // En Passant
        int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
        next_pos.piece_bb[PAWN] &= ~set_bit(captured_pawn_sq);
        next_pos.color_bb[opp] &= ~set_bit(captured_pawn_sq);
        next_pos.board[captured_pawn_sq] = NO_PIECE;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
    }
    
    if (move.promotion != NO_PIECE) { // Promotion
        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.board[move.to] = move.promotion;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else { // Normal move placement
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.board[move.to] = piece_moved;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }
    
    // --- Update state variables (EP, Castling) ---
    next_pos.zobrist_hash ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    next_pos.ep_square = -1;
    if (piece_moved == PAWN && abs(move.to - move.from) == 16) {
        next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    }
    next_pos.zobrist_hash ^= zobrist_ep[(next_pos.ep_square == -1) ? 64 : next_pos.ep_square];
    
    uint8_t old_castling_rights = pos.castling_rights;
    uint8_t new_castling_rights = pos.castling_rights;

    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);

        if (abs(move.to - move.from) == 2) { // Castle move
            int rook_from_sq, rook_to_sq;
            if (move.to == G1_SQ)      { rook_from_sq = H1_SQ; rook_to_sq = F1_SQ; }
            else if (move.to == C1_SQ) { rook_from_sq = A1_SQ; rook_to_sq = D1_SQ; }
            else if (move.to == G8_SQ) { rook_from_sq = H8_SQ; rook_to_sq = F8_SQ; }
            else /*(move.to == C8_SQ)*/ { rook_from_sq = A8_SQ; rook_to_sq = D8_SQ; }

            next_pos.piece_bb[ROOK] &= ~set_bit(rook_from_sq);
            next_pos.piece_bb[ROOK] |= set_bit(rook_to_sq);
            next_pos.color_bb[stm] &= ~set_bit(rook_from_sq);
            next_pos.color_bb[stm] |= set_bit(rook_to_sq);
            next_pos.board[rook_from_sq] = NO_PIECE;
            next_pos.board[rook_to_sq] = ROOK;
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
        }
    }
    if (move.from == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
    if (move.from == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
    if (move.from == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
    if (move.from == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
    if (move.to == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
    if (move.to == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
    if (move.to == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
    if (move.to == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;

    next_pos.castling_rights = new_castling_rights;
    if (old_castling_rights != next_pos.castling_rights) {
        next_pos.zobrist_hash ^= zobrist_castling[old_castling_rights];
        next_pos.zobrist_hash ^= zobrist_castling[next_pos.castling_rights];
    }
    
    // --- Finalize and check legality ---
    next_pos.side_to_move = opp;
    next_pos.zobrist_hash ^= zobrist_side_to_move;
    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply = pos.ply + 1;

    int king_sq_after_move = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq_after_move == -1) return pos; // Should not happen
    if (is_square_attacked(next_pos, king_sq_after_move, opp)) {
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
const int game_phase_inc[6] = {0, 1, 1, 2, 4, 0}; // P,N,B,R,Q,K

// Evaluation helper masks
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
                    bool is_passed = (current_eval_color == WHITE) ?
                        ((white_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0) :
                        ((black_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0);
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

        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[current_eval_color]);
        if (king_sq != -1) {
            int pawn_shield_score = 0;
            const int shield_pawn_bonus = 12;
            const int missing_shield_pawn_penalty = -18;

            int shield_candidate_sqs[3] = {-1, -1, -1};
            bool relevant_castled_pos = false;
            
            if (current_eval_color == WHITE) {
                if (king_sq == G1_SQ) { shield_candidate_sqs[0] = F2_SQ; shield_candidate_sqs[1] = G2_SQ; shield_candidate_sqs[2] = H2_SQ; relevant_castled_pos = true; }
                else if (king_sq == C1_SQ) { shield_candidate_sqs[0] = B2_SQ; shield_candidate_sqs[1] = C2_SQ; shield_candidate_sqs[2] = D2_SQ; relevant_castled_pos = true; }
            } else {
                if (king_sq == G8_SQ) { shield_candidate_sqs[0] = F7_SQ; shield_candidate_sqs[1] = G7_SQ; shield_candidate_sqs[2] = H7_SQ; relevant_castled_pos = true; }
                else if (king_sq == C8_SQ) { shield_candidate_sqs[0] = B7_SQ; shield_candidate_sqs[1] = C7_SQ; shield_candidate_sqs[2] = D7_SQ; relevant_castled_pos = true; }
            }

            if (relevant_castled_pos) {
                for (int shield_sq : shield_candidate_sqs) {
                    if (pos.piece_on_sq(shield_sq) == PAWN && pos.color_on_sq(shield_sq) == current_eval_color) {
                        pawn_shield_score += shield_pawn_bonus;
                    } else {
                        pawn_shield_score += missing_shield_pawn_penalty;
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
            while(temp_king_zone_for_pawns) { int sq = lsb_index(temp_king_zone_for_pawns); temp_king_zone_for_pawns &= temp_king_zone_for_pawns-1; if (pawn_attacks_bb[current_eval_color][sq] & all_enemy_pawns) king_attack_score += 1; }

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

void init_tt(size_t mb_size) {
    if (mb_size == 0) { transposition_table.clear(); tt_mask = 0; return; }
    size_t num_entries = (mb_size * 1024 * 1024) / sizeof(TTEntry);
    if (num_entries == 0) { transposition_table.clear(); tt_mask = 0; return; }
    size_t power_of_2_entries = 1;
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries) { power_of_2_entries *= 2; }
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

    bool should_replace = (entry.hash != hash) || (depth >= entry.depth);

    if (should_replace) {
        entry.hash = hash;
        entry.depth = depth;
        entry.score = score;
        entry.bound = bound;
        if (!best_move.is_null() || entry.hash != hash) {
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
std::vector<uint64_t> game_history_hashes;

void reset_search_state() {
    nodes_searched = 0;
    stop_search_flag = false;
}

void reset_killers_and_history() {
    std::memset(killer_moves, 0, sizeof(killer_moves));
    std::memset(history_heuristic, 0, sizeof(history_heuristic));
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

constexpr int SCORE_TT_MOVE = 2000000;
constexpr int SCORE_PROMOTION_BASE = 1100000;
constexpr int SCORE_CAPTURE_BASE = 1000000;
constexpr int SCORE_KILLER_1 = 900000;
constexpr int SCORE_KILLER_2 = 800000;
constexpr int HISTORY_MAX = 30000;
const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0}; // P,N,B,R,Q,K,NO_PIECE

void score_moves(const Position& pos, std::vector<Move>& moves, const Move& tt_move, int ply) {
    for (Move& m : moves) {
        if (!tt_move.is_null() && m == tt_move) {
            m.score = SCORE_TT_MOVE;
        } else if (m.promotion != NO_PIECE) {
             m.score = SCORE_PROMOTION_BASE + mvv_lva_piece_values[m.promotion];
        } else {
            Piece captured_piece = pos.piece_on_sq(m.to);
            if (captured_piece != NO_PIECE) {
                Piece moved_piece = pos.piece_on_sq(m.from);
                m.score = SCORE_CAPTURE_BASE + (mvv_lva_piece_values[captured_piece] * 10) - mvv_lva_piece_values[moved_piece];
            } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
                m.score = SCORE_KILLER_1;
            } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
                m.score = SCORE_KILLER_2;
            } else {
                m.score = history_heuristic[pos.side_to_move][m.from][m.to];
            }
        }
    }
    std::sort(moves.begin(), moves.end(), [](const Move& a, const Move& b){ return a.score > b.score; });
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return evaluate(pos);

    int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]);
    bool in_check = (king_sq != -1) && is_square_attacked(pos, king_sq, 1 - pos.side_to_move);
    
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

    score_moves(pos, q_moves, NULL_MOVE, ply);

    int legal_moves_in_qsearch = 0;
    for (const Move& cap_move : q_moves) {
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

    int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]);
    bool in_check = (king_sq != -1) && is_square_attacked(pos, king_sq, 1 - pos.side_to_move);
    
    if (in_check) depth++;

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score;
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
                 store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, NULL_MOVE);
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
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        if (legal_moves_played == 1) {
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_search_path_hashes);
        } else {
            int R_lmr = 0;
            if (depth >= 3 && i >= 3 && !in_check && current_move.promotion == NO_PIECE &&
                pos.piece_on_sq(current_move.to) == NO_PIECE) {
                R_lmr = 1;
                if(depth >= 5 && i >= 5) R_lmr = (depth > 7) ? 2 : 1;
                R_lmr = std::min(R_lmr, depth - 2);
            }

            score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);
            
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
                    if (pos.piece_on_sq(current_move.to) == NO_PIECE) {
                        if (!(current_move == killer_moves[ply][0])) {
                            killer_moves[ply][1] = killer_moves[ply][0];
                            killer_moves[ply][0] = current_move;
                        }
                        int& hist_score = history_heuristic[pos.side_to_move][current_move.from][current_move.to];
                        hist_score += depth * depth;
                        if (hist_score > HISTORY_MAX) hist_score = HISTORY_MAX;
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
                pos.board[sq] = p_type;
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
        } else { pos.ep_square = -1; }
    } else { pos.ep_square = -1; }

    if (ss >> part) { try {pos.halfmove_clock = std::stoi(part);} catch(...) {pos.halfmove_clock = 0;} }
    else pos.halfmove_clock = 0;
    if (ss >> part) { try {pos.fullmove_number = std::stoi(part);} catch(...){pos.fullmove_number = 1;} }
    else pos.fullmove_number = 1;

    pos.ply = 0;
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_hashes.clear();
}

Move parse_uci_move_from_string(const Position& current_pos, const std::string& uci_move_str) {
    if (uci_move_str.length() < 4 || uci_move_str.length() > 5) return NULL_MOVE;
    if (uci_move_str == "0000") return NULL_MOVE;

    Move m;
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
            std::cout << "id name Amira 0.3 (Revised)\n";
            std::cout << "id author ChessTubeTree\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 2048\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str_val;
            ss >> name_token >> name_str >> value_token >> value_str_val;
            if (name_token == "name" && name_str == "Hash" && value_token == "value") {
                 try {
                    g_configured_tt_size_mb = std::max(0, std::min(std::stoi(value_str_val), 2048));
                } catch (...) { /* ignore parse error */ }
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) {
                 init_tt(g_configured_tt_size_mb);
                 g_tt_is_initialized = true;
            }
            clear_tt();
            reset_killers_and_history();
            game_history_hashes.clear();
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                ss >> token;
            } else if (token == "fen") {
                while (ss >> token && token != "moves") {
                    fen_str_collector += token + " ";
                }
                parse_fen(uci_root_pos, fen_str_collector);
            }

            if(game_history_hashes.empty() || game_history_hashes.back() != uci_root_pos.zobrist_hash) {
                 game_history_hashes.push_back(uci_root_pos.zobrist_hash);
            }

            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    bool legal;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                    game_history_hashes.push_back(uci_root_pos.zobrist_hash);
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }

            long long wtime = -1, btime = -1, winc = 0, binc = 0, fixed_time_per_move = -1;
            int movestogo = 0, max_depth_to_search = MAX_PLY;

            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime; else if (go_param == "btime") ss >> btime;
                else if (go_param == "winc") ss >> winc; else if (go_param == "binc") ss >> binc;
                else if (go_param == "movestogo") ss >> movestogo; else if (go_param == "movetime") ss >> fixed_time_per_move;
                else if (go_param == "depth") ss >> max_depth_to_search;
            }

            if (fixed_time_per_move != -1) {
                search_budget_ms = std::max(10LL, fixed_time_per_move - 50);
            } else {
                long long my_time = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;
                long long my_inc = (uci_root_pos.side_to_move == WHITE) ? winc : binc;
                if (my_time != -1) {
                    long long base_time_slice = my_time / (movestogo > 0 ? std::min(movestogo, 40) : 25);
                    search_budget_ms = base_time_slice + my_inc - 50;
                    if (my_time > 100 && search_budget_ms > my_time * 0.8) search_budget_ms = (long long)(my_time * 0.8);
                } else {
                    search_budget_ms = 2000;
                }
            }
            if (search_budget_ms <= 0) search_budget_ms = 50;

            search_start_timepoint = std::chrono::steady_clock::now();
            reset_search_state();
            uci_best_move_overall = NULL_MOVE;
            std::vector<uint64_t> root_path_hashes;

            int aspiration_alpha = -INF_SCORE, aspiration_beta = INF_SCORE, aspiration_window_delta = 25;

            for (int depth = 1; depth <= max_depth_to_search; ++depth) {
                int current_score;
                if (depth <= 2) {
                     current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true, root_path_hashes);
                } else {
                    current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, root_path_hashes);
                    if (!stop_search_flag && (current_score <= aspiration_alpha || current_score >= aspiration_beta)) {
                        aspiration_alpha = -INF_SCORE; aspiration_beta = INF_SCORE;
                        current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true, root_path_hashes);
                    }
                }

                if (stop_search_flag && depth > 1) break;
                
                TTEntry root_entry;
                if (tt_mask > 0) root_entry = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                if (root_entry.hash == uci_root_pos.zobrist_hash && !root_entry.best_move.is_null()){
                    uci_best_move_overall = root_entry.best_move;
                }
                
                if (abs(current_score) < MATE_THRESHOLD) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 2 + 5;
                    if(aspiration_window_delta > 400) aspiration_window_delta = 400;
                } else {
                    aspiration_alpha = -INF_SCORE; aspiration_beta = INF_SCORE;
                }

                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - search_start_timepoint).count();

                std::cout << "info depth " << depth << " score";
                if (abs(current_score) > MATE_THRESHOLD) {
                    int mate_in_plies = MATE_SCORE - abs(current_score);
                    int mate_in_moves = (mate_in_plies + 1) / 2;
                    std::cout << " mate " << (current_score > 0 ? mate_in_moves : -mate_in_moves);
                } else {
                    std::cout << " cp " << current_score;
                }
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);
                
                if (!uci_best_move_overall.is_null()) {
                    std::cout << " pv " << move_to_uci(uci_best_move_overall);
                }
                std::cout << std::endl;

                if (abs(current_score) > MATE_THRESHOLD) break;
                if (search_budget_ms > 0 && elapsed_ms > 0 && depth > 3 && (elapsed_ms * 2.5 > search_budget_ms)) break;
            }

            if (!uci_best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            } else {
                std::vector<Move> fallback_moves; generate_moves(uci_root_pos, fallback_moves);
                for(const auto& m_fall : fallback_moves) {
                    bool is_legal;
                    Position temp_pos = make_move(uci_root_pos, m_fall, is_legal);
                    if(is_legal) { std::cout << "bestmove " << move_to_uci(m_fall) << std::endl; goto found_fallback; }
                }
                std::cout << "bestmove 0000\n" << std::flush;
                found_fallback:;
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
