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

// Square constants for clarity
enum Square : int {
    A1 = 0, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
    NO_SQUARE = -1, // For EP square when none
    EP_ZOBRIST_INDEX = 64 // Special index for zobrist_ep if no EP square
};

// Castling rights flags
enum CastlingRightsFlags : uint8_t {
    NO_CASTLING = 0,
    WK_CASTLE_FLAG = 1,      // White kingside (0001)
    WQ_CASTLE_FLAG = 2,      // White queenside (0010)
    BK_CASTLE_FLAG = 4,      // Black kingside (0100)
    BQ_CASTLE_FLAG = 8       // Black queenside (1000)
};


constexpr int MAX_PLY = 64;
constexpr int TT_SIZE_MB_DEFAULT = 8;
constexpr int MAX_MOVES_ESTIMATE = 256; // For reserving vector capacity

constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY; // Scores above this are mates
constexpr int INF_SCORE = 32000; // A value clearly larger than any achievable eval or mate score

// Forward Declarations
struct Move;
struct Position;
int evaluate(const Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only = false);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);
bool probe_tt(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move_from_tt, int& score_from_tt);


// --- Zobrist Hashing ---
uint64_t zobrist_pieces[2][6][64];
uint64_t zobrist_castling[16]; // Covers all 2^4 combinations of castling rights
uint64_t zobrist_ep[65]; // 64 squares + 1 for no EP square (index EP_ZOBRIST_INDEX)
uint64_t zobrist_side_to_move;
std::mt19937_64 rng_zobrist(0xCEC); // Constant seed for deterministic hashes

void init_zobrist() {
    for (int c = 0; c < 2; ++c)
        for (int p = 0; p < 6; ++p)
            for (int s = 0; s < 64; ++s)
                zobrist_pieces[c][p][s] = rng_zobrist();
    for (int i = 0; i < 16; ++i)
        zobrist_castling[i] = rng_zobrist();
    for (int i = 0; i < 65; ++i) // Includes EP_ZOBRIST_INDEX
        zobrist_ep[i] = rng_zobrist();
    zobrist_side_to_move = rng_zobrist();
}

// --- Move Structure ---
struct Move {
    int from = 0, to = 0;
    Piece promotion = NO_PIECE;
    int score = 0; // Transient score for move ordering

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
        char promo_char = 'q'; // Default to queen
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
    int count = 0;
    while (!((bb >> count) & 1)) { count++; if (count >= 64) return -1; }
    return count;
#endif
}

// Bitboard directional shifts with file masks
const uint64_t NotAFile = ~0x0101010101010101ULL;
const uint64_t NotHFile = ~0x8080808080808080ULL;
const uint64_t NotABFile = NotAFile & ~(0x0202020202020202ULL);
const uint64_t NotGHFile = NotHFile & ~(0x4040404040404040ULL);


uint64_t north(uint64_t b) { return b << 8; }
uint64_t south(uint64_t b) { return b >> 8; }
uint64_t east(uint64_t b) { return (b << 1) & NotAFile; }
uint64_t west(uint64_t b) { return (b >> 1) & NotHFile; }
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
        ep_square = NO_SQUARE; 
        fullmove_number = 1;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }

    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE;
        uint64_t b = set_bit(sq);
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

        // Knight attacks using the standard masking technique
        knight_attacks_bb[sq] = (
            ((b << 17) & NotAFile) |          // NNE
            ((b << 15) & NotHFile) |          // NNW
            ((b << 10) & NotABFile) |         // ENE
            ((b << 6)  & NotGHFile) |         // WNW
            ((b >> 17) & NotHFile) |          // SSW
            ((b >> 15) & NotAFile) |          // SSE
            ((b >> 10) & NotGHFile) |         // WSW
            ((b >> 6)  & NotABFile)           // ESE
        );

        king_attacks_bb[sq] = north(b) | south(b) | east(b) | west(b) |
                              ne(b) | nw(b) | se(b) | sw(b);
    }
}


// --- Slider Attack Generation ---
uint64_t get_rook_attacks_from_sq(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {8, -8, 1, -1}; // N, S, E, W
    int r_start = sq / 8;
    int c_start = sq % 8;

    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break; 

            if (abs(d) == 1) { 
                if (s / 8 != r_start) break; 
            } else { 
                if (s % 8 != c_start) break; 
            }
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

            int r_curr = s / 8;
            int c_curr = s % 8;
            int r_prev = (s - d) / 8; 
            int c_prev = (s - d) % 8;

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
    moves_list.reserve(MAX_MOVES_ESTIMATE); 

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
        bool is_promotion_sq_for_push = (stm == WHITE && rank == 6) || (stm == BLACK && rank == 1);


        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >=0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (!captures_only) {
                if (is_promotion_sq_for_push) {
                    add_move_to_list(moves_list, from, one_step_sq, QUEEN);
                    add_move_to_list(moves_list, from, one_step_sq, ROOK);
                    add_move_to_list(moves_list, from, one_step_sq, BISHOP);
                    add_move_to_list(moves_list, from, one_step_sq, KNIGHT);
                } else {
                    add_move_to_list(moves_list, from, one_step_sq);
                }
            }
            int start_rank_board_idx = (stm == WHITE) ? 1 : 6;
            if (rank == start_rank_board_idx && !captures_only) {
                int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                if (two_steps_sq >=0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq)) {
                    add_move_to_list(moves_list, from, two_steps_sq);
                }
            }
        }
        
        uint64_t pawn_cap_targets = pawn_attacks_bb[stm][from] & opp_pieces;
        if (pos.ep_square != NO_SQUARE) {
             if (get_bit(pawn_attacks_bb[stm][from], pos.ep_square)) { 
                 pawn_cap_targets |= set_bit(pos.ep_square);
             }
        }
        while (pawn_cap_targets) {
            int to = lsb_index(pawn_cap_targets);
            pawn_cap_targets &= pawn_cap_targets - 1;
            // Promotion on capture: pawn moves from rank 6 to 7 (White) or 1 to 0 (Black)
            bool is_promotion_sq_for_capture = (stm == WHITE && (from/8) == 6 && (to/8) == 7) || (stm == BLACK && (from/8) == 1 && (to/8) == 0);
            if (is_promotion_sq_for_capture) {
                add_move_to_list(moves_list, from, to, QUEEN); 
                add_move_to_list(moves_list, from, to, ROOK);
                add_move_to_list(moves_list, from, to, BISHOP); 
                add_move_to_list(moves_list, from, to, KNIGHT);
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
        int king_sq = lsb_index(pos.piece_bb[KING] & my_pieces); 
        if (king_sq != -1) { 
            if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_FLAG) && king_sq == E1 &&
                    !get_bit(occupied, F1) && !get_bit(occupied, G1) &&
                    !is_square_attacked(pos, E1, BLACK) && !is_square_attacked(pos, F1, BLACK) && !is_square_attacked(pos, G1, BLACK)) {
                    add_move_to_list(moves_list, E1, G1); 
                }
                if ((pos.castling_rights & WQ_CASTLE_FLAG) && king_sq == E1 &&
                    !get_bit(occupied, D1) && !get_bit(occupied, C1) && !get_bit(occupied, B1) &&
                    !is_square_attacked(pos, E1, BLACK) && !is_square_attacked(pos, D1, BLACK) && !is_square_attacked(pos, C1, BLACK)) {
                    add_move_to_list(moves_list, E1, C1); 
                }
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_FLAG) && king_sq == E8 &&
                    !get_bit(occupied, F8) && !get_bit(occupied, G8) &&
                    !is_square_attacked(pos, E8, WHITE) && !is_square_attacked(pos, F8, WHITE) && !is_square_attacked(pos, G8, WHITE)) {
                    add_move_to_list(moves_list, E8, G8);
                }
                if ((pos.castling_rights & BQ_CASTLE_FLAG) && king_sq == E8 &&
                    !get_bit(occupied, D8) && !get_bit(occupied, C8) && !get_bit(occupied, B8) &&
                    !is_square_attacked(pos, E8, WHITE) && !is_square_attacked(pos, D8, WHITE) && !is_square_attacked(pos, C8, WHITE)) {
                    add_move_to_list(moves_list, E8, C8);
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

    Piece piece_moved = pos.piece_on_sq(move.from);
    if (piece_moved == NO_PIECE || pos.color_on_sq(move.from) != stm) {
        return pos; 
    }
    bool is_castling_move = (piece_moved == KING && abs(move.to - move.from) == 2);
    if (!is_castling_move && pos.color_on_sq(move.to) == stm) { 
        return pos;
    }


    uint64_t from_bb = set_bit(move.from);
    uint64_t to_bb = set_bit(move.to);
    Piece piece_captured = pos.piece_on_sq(move.to); 

    next_pos.zobrist_hash = pos.zobrist_hash;

    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;
    next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.from];

    next_pos.halfmove_clock++;
    if (piece_moved == PAWN) next_pos.halfmove_clock = 0;

    if (piece_captured != NO_PIECE && !is_castling_move) { 
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        next_pos.halfmove_clock = 0; 
    }

    int captured_pawn_sq_ep = NO_SQUARE;
    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != NO_SQUARE) {
        captured_pawn_sq_ep = (stm == WHITE) ? pos.ep_square - 8 : pos.ep_square + 8;
        if (captured_pawn_sq_ep >= 0 && captured_pawn_sq_ep < 64) { 
            next_pos.piece_bb[PAWN] &= ~set_bit(captured_pawn_sq_ep);
            next_pos.color_bb[opp] &= ~set_bit(captured_pawn_sq_ep);
            next_pos.zobrist_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq_ep];
        }
    }

    next_pos.zobrist_hash ^= zobrist_ep[(pos.ep_square == NO_SQUARE) ? EP_ZOBRIST_INDEX : pos.ep_square];
    next_pos.ep_square = NO_SQUARE; 
    if (piece_moved == PAWN && abs(move.to - move.from) == 16) { 
        next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    }
    next_pos.zobrist_hash ^= zobrist_ep[(next_pos.ep_square == NO_SQUARE) ? EP_ZOBRIST_INDEX : next_pos.ep_square];

    if (move.promotion != NO_PIECE) {
        if (piece_moved != PAWN) return pos; 
        int promotion_rank_board_idx = (stm == WHITE) ? 7 : 0;
        if (move.to / 8 != promotion_rank_board_idx) return pos; 

        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else {
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    uint8_t old_castling_rights = next_pos.castling_rights; 

    if (piece_moved == KING) {
        if (stm == WHITE) next_pos.castling_rights &= ~(WK_CASTLE_FLAG | WQ_CASTLE_FLAG);
        else next_pos.castling_rights &= ~(BK_CASTLE_FLAG | BQ_CASTLE_FLAG);
    }
    
    // If a rook moves from its original square, or is captured on its original square
    if (move.from == H1 || move.to == H1) next_pos.castling_rights &= ~WK_CASTLE_FLAG;
    if (move.from == A1 || move.to == A1) next_pos.castling_rights &= ~WQ_CASTLE_FLAG;
    if (move.from == H8 || move.to == H8) next_pos.castling_rights &= ~BK_CASTLE_FLAG;
    if (move.from == A8 || move.to == A8) next_pos.castling_rights &= ~BQ_CASTLE_FLAG;
    
    if (old_castling_rights != next_pos.castling_rights) {
        next_pos.zobrist_hash ^= zobrist_castling[old_castling_rights];
        next_pos.zobrist_hash ^= zobrist_castling[next_pos.castling_rights];
    }

    if (is_castling_move) { 
        int rook_from_sq, rook_to_sq;
        if (move.to == G1) { rook_from_sq = H1; rook_to_sq = F1; }        
        else if (move.to == C1) { rook_from_sq = A1; rook_to_sq = D1; }  
        else if (move.to == G8) { rook_from_sq = H8; rook_to_sq = F8; }  
        else { rook_from_sq = A8; rook_to_sq = D8; } // move.to == C8

        uint64_t rook_from_bb = set_bit(rook_from_sq);
        uint64_t rook_to_bb = set_bit(rook_to_sq);

        next_pos.piece_bb[ROOK] &= ~rook_from_bb;
        next_pos.piece_bb[ROOK] |= rook_to_bb;
        next_pos.color_bb[stm] &= ~rook_from_bb;
        next_pos.color_bb[stm] |= rook_to_bb;

        next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
        next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
    }

    next_pos.side_to_move = opp;
    next_pos.zobrist_hash ^= zobrist_side_to_move;

    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply = pos.ply + 1;

    int king_sq_after_move = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq_after_move == -1) { return pos; }
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

int evaluate(const Position& pos) {
    int mg_score = 0;
    int eg_score = 0;
    int game_phase = 0; 

    for (int c_idx = 0; c_idx < 2; ++c_idx) { 
        Color current_eval_color = (Color)c_idx;
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;

        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1; 
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (H8 - sq); 

                mg_score += side_multiplier * (piece_values_mg[p] + pst_mg_all[p][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p] + pst_eg_all[p][mirrored_sq]);
            }
        }
    }
    if (game_phase > 24) game_phase = 24; 
    // game_phase can be 0 in extreme endgames (e.g. K vs K). Division by zero if game_phase_total_max is 0.
    // Max game_phase is 2*(1*2+1*2+2*2+4*1) = 24. Tapering needs game_phase to be in [0, 24].
    // If all pieces are gone except kings, game_phase = 0. (24-game_phase) will be 24.
    // The division by 24 is safe as long as it's the intended maximum.
    
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
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries ) {
        power_of_2_entries *= 2;
    }
     if (power_of_2_entries == 0) { 
         transposition_table.clear(); tt_mask = 0; return;
    }

    try {
        transposition_table.assign(power_of_2_entries, TTEntry());
        tt_mask = power_of_2_entries - 1;
    } catch (const std::bad_alloc&) {
        transposition_table.clear();
        tt_mask = 0;
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

    bool should_replace = (entry.hash == 0) || 
                          (entry.hash != hash) || 
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
std::vector<uint64_t> game_history_hashes; 

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

const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0}; 

void score_moves(const Position& pos, std::vector<Move>& moves, const Move& tt_move, int ply) {
    for (Move& m : moves) {
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
                m.score = history_heuristic[pos.side_to_move][m.from][m.to];
            }
        }
    }
    std::sort(moves.begin(), moves.end(), [](const Move& a, const Move& b){
        return a.score > b.score;
    });
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY -1) return evaluate(pos); 

    int stand_pat = evaluate(pos);
    if (stand_pat >= beta) return beta; 
    if (alpha < stand_pat) alpha = stand_pat;

    std::vector<Move> captures;
    generate_moves(pos, captures, true); 

    Move dummy_tt_move = NULL_MOVE; 
    score_moves(pos, captures, dummy_tt_move, ply); 

    for (const Move& cap_move : captures) {
        bool legal;
        Position next_pos = make_move(pos, cap_move, legal);
        if (!legal) continue;

        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);
        if (score >= beta) return beta; 
        if (score > alpha) alpha = score;
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
    for (size_t i = 0; i < (size_t)pos.halfmove_clock && (game_history_hashes.size() > i) ; ++i) {
        // Check current hash against hashes in game_history_hashes.
        // We need to iterate back from (game_history_hashes.size() - 1 - i)
        // to check hashes of previous positions in the game.
        // The loop condition game_history_hashes.size() > i is correct.
        // The index should be game_history_hashes.size() - 1 - i for the i-th element from the end (0-indexed from end).
        if (game_history_hashes.size() > i) { // Ensure index is valid
             if (game_history_hashes[game_history_hashes.size() - 1 - i] == pos.zobrist_hash) {
                game_reps++;
            }
        }
    }
    if(game_reps >= 2 && ply > 0) return 0; 

    if (pos.halfmove_clock >= 100 && ply > 0) return 0; 

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth++; 

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
         return tt_score; 
    }

    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 && 
        evaluate(pos) >= beta) { 
            
            Position null_next_pos = pos; 
            null_next_pos.side_to_move = 1 - pos.side_to_move;
            
            uint64_t old_ep_z_idx = (pos.ep_square == NO_SQUARE) ? EP_ZOBRIST_INDEX : pos.ep_square;
            null_next_pos.zobrist_hash = pos.zobrist_hash ^ zobrist_ep[old_ep_z_idx] ^ zobrist_ep[EP_ZOBRIST_INDEX] ^ zobrist_side_to_move;
            null_next_pos.ep_square = NO_SQUARE; 
            null_next_pos.ply = pos.ply + 1;

            int R = (depth > 6) ? 3 : 2; 
            current_search_path_hashes.push_back(pos.zobrist_hash); 
            int null_score = -search(null_next_pos, depth - 1 - R, -beta, -beta + 1, ply + 1, false, false, current_search_path_hashes);
            current_search_path_hashes.pop_back();

            if (stop_search_flag) return 0; 
            if (null_score >= beta) {
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
            int r = 0; 
            if (depth >= 3 && i >= (is_pv_node ? 3 : 2) && 
                !in_check && current_move.promotion == NO_PIECE && pos.piece_on_sq(current_move.to) == NO_PIECE && 
                current_move.score < 700000) { 
                
                r = 1;
                if (depth >= 5 && i >= (is_pv_node ? 5 : 4)) r = (depth > 7 ? 2 : 1); 
                r = std::min(r, depth - 2); 
                if (r < 0) r = 0;
            }

            score = -search(next_pos, depth - 1 - r, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);

            if (r > 0 && score > alpha) { 
                 score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_search_path_hashes);
            }
            if (score > alpha && score < beta) { // If null window search failed high (but within [alpha,beta]), re-search with full window
                 score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_search_path_hashes); 
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

            if (p_type != NO_PIECE) {
                int sq = rank * 8 + file;
                if (sq >= 0 && sq < 64) { 
                    pos.piece_bb[p_type] |= set_bit(sq);
                    pos.color_bb[p_color] |= set_bit(sq);
                }
            }
            file++;
        }
    }

    ss >> part; pos.side_to_move = (part == "w") ? WHITE : BLACK;

    ss >> part;
    pos.castling_rights = NO_CASTLING;
    for (char c : part) {
        if (c == 'K') pos.castling_rights |= WK_CASTLE_FLAG;
        else if (c == 'Q') pos.castling_rights |= WQ_CASTLE_FLAG;
        else if (c == 'k') pos.castling_rights |= BK_CASTLE_FLAG;
        else if (c == 'q') pos.castling_rights |= BQ_CASTLE_FLAG;
    }

    ss >> part;
    pos.ep_square = NO_SQUARE;
    if (part != "-") {
        if (part.length() == 2 && part[0] >= 'a' && part[0] <= 'h' && part[1] >= '1' && part[1] <= '8') {
            int ep_file = part[0] - 'a';
            int ep_rank = part[1] - '1';
            pos.ep_square = ep_rank * 8 + ep_file;
        }
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
    h ^= zobrist_ep[(pos.ep_square == NO_SQUARE) ? EP_ZOBRIST_INDEX : pos.ep_square];
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
            std::cout << "id name Amira\n"; 
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
            ss >> name_token; 
            if (name_token == "name") {
                ss >> name_str; 
                ss >> value_token; 
                ss >> value_str_val; 
                
                if (name_str == "Hash") {
                    try {
                        int parsed_size = std::stoi(value_str_val);
                        if (parsed_size >= 0 && parsed_size <= 1024) { 
                             g_configured_tt_size_mb = parsed_size;
                        }
                    } catch (...) {  }
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true; 
                }
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            init_tt(g_configured_tt_size_mb); 
            g_tt_is_initialized = true;
            reset_killers_and_history();
            game_history_hashes.clear(); 
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token; 
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                if (ss.rdbuf()->in_avail() > 0) {
                    std::string next_word; 
                    std::streampos p_before_moves_check = ss.tellg();
                    ss >> next_word;
                    if (next_word == "moves") {
                        token = "moves"; 
                    } else {
                        ss.seekg(p_before_moves_check); 
                        token = ""; 
                    }
                } else {
                    token = ""; 
                }
            } else if (token == "fen") {
                std::string temp_fen_part;
                for (int i = 0; i < 6; ++i) { 
                    if (!(ss >> temp_fen_part)) break; 
                    if (temp_fen_part == "moves") { 
                        token = "moves"; 
                        break; 
                    }
                    fen_str_collector += temp_fen_part + " ";
                }
                if (!fen_str_collector.empty()) fen_str_collector.pop_back(); 
                parse_fen(uci_root_pos, fen_str_collector);

                if (token != "moves" && ss.rdbuf()->in_avail() > 0) {
                    std::string next_word;
                    std::streampos p_before_moves_check = ss.tellg();
                    ss >> next_word;
                    if (next_word == "moves") {
                        token = "moves";
                    } else {
                        ss.seekg(p_before_moves_check);
                    }
                }
            }
            
            game_history_hashes.push_back(uci_root_pos.zobrist_hash); 

            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && move_str_uci != "0000") break; 
                    bool legal;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) {
                        break; 
                    }
                    game_history_hashes.push_back(uci_root_pos.zobrist_hash); 
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) { 
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }

            int wtime = -1, btime = -1, winc = 0, binc = 0, movestogo = 0;
            long long fixed_time_per_move = -1;
            int max_depth_limit = MAX_PLY; 

            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime;
                else if (go_param == "btime") ss >> btime;
                else if (go_param == "winc") ss >> winc;
                else if (go_param == "binc") ss >> binc;
                else if (go_param == "movestogo") ss >> movestogo;
                else if (go_param == "movetime") ss >> fixed_time_per_move;
                else if (go_param == "depth") ss >> max_depth_limit;
            }

            if (fixed_time_per_move != -1) {
                search_budget_ms = fixed_time_per_move - 50; 
            } else {
                int my_time = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;
                int my_inc = (uci_root_pos.side_to_move == WHITE) ? winc : binc;

                if (my_time != -1) { 
                    long long base_time_slice;
                    if (movestogo > 0 && movestogo < 40) { 
                        base_time_slice = my_time / movestogo;
                    } else {
                        base_time_slice = my_time / 25; 
                    }
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
            int best_score_overall = 0;
            std::vector<uint64_t> root_path_hashes; 

            for (int current_iter_depth = 1; current_iter_depth <= max_depth_limit; ++current_iter_depth) {
                Position search_pos_copy = uci_root_pos; 
                int current_score = search(search_pos_copy, current_iter_depth, -INF_SCORE, INF_SCORE, 0, true, true, root_path_hashes);

                if (stop_search_flag && current_iter_depth > 1) { 
                    break;
                }

                if (tt_mask > 0 && g_tt_is_initialized) { 
                    TTEntry& root_entry = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                    if (root_entry.hash == uci_root_pos.zobrist_hash && !root_entry.best_move.is_null()) {
                        uci_best_move_overall = root_entry.best_move;
                    }
                }
                
                best_score_overall = current_score; 

                auto now_tp = std::chrono::steady_clock::now();
                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_tp - search_start_timepoint).count();
                if (elapsed_ms < 0) elapsed_ms = 0; 

                std::cout << "info depth " << current_iter_depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) {
                    int plies_to_mate = MATE_SCORE - best_score_overall; // Should be positive
                    std::cout << " mate " << (plies_to_mate + 1) / 2;
                } else if (best_score_overall < -MATE_THRESHOLD) {
                    int plies_to_mate_for_opponent = MATE_SCORE + best_score_overall; // e.g. 30000 + (-29990) = 10. Should be positive.
                    std::cout << " mate " << -((plies_to_mate_for_opponent + 1) / 2);
                }
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0 && nodes_searched > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);

                if (!uci_best_move_overall.is_null() && tt_mask > 0 && g_tt_is_initialized) {
                    std::cout << " pv";
                    Position temp_pv_pos = uci_root_pos;
                    for (int pv_idx = 0; pv_idx < current_iter_depth; ++pv_idx) {
                        Move pv_move = NULL_MOVE;
                        TTEntry& pv_entry = transposition_table[temp_pv_pos.zobrist_hash & tt_mask];
                        if (pv_entry.hash == temp_pv_pos.zobrist_hash && !pv_entry.best_move.is_null()) {
                            pv_move = pv_entry.best_move;
                        } else {
                            break; 
                        }
                        
                        std::cout << " " << move_to_uci(pv_move);
                        bool pv_legal;
                        temp_pv_pos = make_move(temp_pv_pos, pv_move, pv_legal);
                        if (!pv_legal) break; 
                    }
                }
                std::cout << std::endl; 

                if (abs(best_score_overall) > MATE_THRESHOLD && current_iter_depth > 1) break; 
                if (search_budget_ms > 0 && elapsed_ms > 0 && current_iter_depth > 1) {
                    if (elapsed_ms * 2.5 > search_budget_ms && current_iter_depth > 3) break;
                    else if (elapsed_ms * 1.8 > search_budget_ms ) break;
                }
                if (current_iter_depth == max_depth_limit) break; 
            }

            if (!uci_best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            } else {
                std::vector<Move> legal_moves_fallback;
                generate_moves(uci_root_pos, legal_moves_fallback);
                Move found_legal_fallback_move = NULL_MOVE;
                for(const auto& m_fall : legal_moves_fallback) {
                    bool is_leg_fall;
                    // Position temp_pos_check = uci_root_pos; // Not needed, uci_root_pos is const for make_move
                    Position next_pos_check = make_move(uci_root_pos, m_fall, is_leg_fall); 
                    if(is_leg_fall) {
                        found_legal_fallback_move = m_fall;
                        break;
                    }
                }
                if (!found_legal_fallback_move.is_null()) {
                    std::cout << "bestmove " << move_to_uci(found_legal_fallback_move) << std::endl;
                } else {
                     std::cout << "bestmove 0000" << std::endl; 
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
    reset_killers_and_history();

    uci_loop();
    return 0;
}
