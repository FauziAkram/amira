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
constexpr int TT_SIZE_MB = 8; // Default TT size

// Mate scores
constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY; // Scores near MATE_SCORE are mate

// --- Forward Declarations ---
struct Move;
struct Position;
struct TTEntry;
int evaluate(const Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only = false);
Position make_move(const Position& pos, const Move& move, bool* legal);


// --- Zobrist Hashing ---
uint64_t zobrist_pieces[2][6][64]; // [color][piece_type][square]
uint64_t zobrist_castling[16];
uint64_t zobrist_ep[64];
uint64_t zobrist_side_to_move;
std::mt19937_64 rng_engine(12345); // Fixed seed for deterministic keys

void init_zobrist() {
    for (int c = 0; c < 2; ++c)
        for (int p = 0; p < 6; ++p)
            for (int s = 0; s < 64; ++s)
                zobrist_pieces[c][p][s] = rng_engine();
    for (int i = 0; i < 16; ++i)
        zobrist_castling[i] = rng_engine();
    for (int i = 0; i < 64; ++i)
        zobrist_ep[i] = rng_engine();
    zobrist_side_to_move = rng_engine();
}

uint64_t calculate_zobrist_hash(const Position& pos); // Forward declaration

// --- Move Structure ---
struct Move {
    int from = 0, to = 0;
    Piece promotion = NO_PIECE;
    int score = 0; // For move ordering

    bool operator==(const Move& other) const {
        return from == other.from && to == other.to && promotion == other.promotion;
    }
    bool is_null() const { return from == 0 && to == 0 && promotion == NO_PIECE; }
};

const Move NULL_MOVE = {0, 0, NO_PIECE};

std::string move_to_uci(const Move& move) {
    std::string uci_move;
    uci_move += (char)('a' + (move.from % 8));
    uci_move += (char)('1' + (move.from / 8));
    uci_move += (char)('a' + (move.to % 8));
    uci_move += (char)('1' + (move.to / 8));
    if (move.promotion != NO_PIECE) {
        char promo_char = 'q';
        if (move.promotion == KNIGHT) promo_char = 'n';
        else if (move.promotion == BISHOP) promo_char = 'b';
        else if (move.promotion == ROOK) promo_char = 'r';
        uci_move += promo_char;
    }
    return uci_move;
}

// --- Bitboard Utilities ---
uint64_t set_bit(int sq) { return 1ULL << sq; }
bool get_bit(uint64_t bb, int sq) { return (bb >> sq) & 1; }
int pop_count(uint64_t bb) {
#if defined(_MSC_VER)
    return static_cast<int>(__popcnt64(bb));
#elif defined(__GNUC__) || defined(__clang__)
    return __builtin_popcountll(bb);
#else
    int count = 0;
    while (bb > 0) {
        bb &= (bb - 1);
        count++;
    }
    return count;
#endif
}
int lsb_index(uint64_t bb) {
    if (bb == 0) return -1;
#if defined(_MSC_VER)
    unsigned long idx;
    _BitScanForward64(&idx, bb);
    return static_cast<int>(idx);
#elif defined(__GNUC__) || defined(__clang__)
    return __builtin_ctzll(bb);
#else
    // Fallback for other compilers (less efficient)
    int count = 0;
    while (!((bb >> count) & 1)) {
        count++;
         if (count >= 64) return -1; // Should not happen if bb != 0
    }
    return count;
#endif
}

// --- Board Representation ---
struct Position {
    uint64_t piece_bb[6];    // PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
    uint64_t color_bb[2];    // WHITE, BLACK
    int side_to_move;        // WHITE or BLACK
    int ep_square;           // 0-63, or -1 if no EP
    uint8_t castling_rights; // KQkq (e.g., White King:1, WQ:2, BK:4, BQ:8)
    uint64_t zobrist_hash;
    int halfmove_clock;
    int fullmove_number;
    int ply;                 // plies from root for search

    Position() {
        std::memset(piece_bb, 0, sizeof(piece_bb));
        std::memset(color_bb, 0, sizeof(color_bb));
        side_to_move = WHITE;
        ep_square = -1;
        castling_rights = 0; // Will be set by FEN parser
        zobrist_hash = 0;
        halfmove_clock = 0;
        fullmove_number = 1;
        ply = 0;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }
    Piece piece_on(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE; // Bounds check
        uint64_t b = set_bit(sq);
        for (int p = PAWN; p <= KING; ++p) {
            if (piece_bb[p] & b) return (Piece)p;
        }
        return NO_PIECE;
    }
    Color color_of_piece_on(int sq) const {
        if (sq < 0 || sq >= 64) return NO_COLOR; // Bounds check
        uint64_t b = set_bit(sq);
        if (color_bb[WHITE] & b) return WHITE;
        if (color_bb[BLACK] & b) return BLACK;
        return NO_COLOR;
    }
};


// --- Attack Generation ---
uint64_t pawn_attacks_bb[2][64];
uint64_t knight_attacks_bb[64];
uint64_t king_attacks_bb[64];

// Elementary shifts
uint64_t north(uint64_t b) { return b << 8; }
uint64_t south(uint64_t b) { return b >> 8; }
uint64_t east(uint64_t b) { return (b << 1) & ~0x0101010101010101ULL; } // ~FILE_A
uint64_t west(uint64_t b) { return (b >> 1) & ~0x8080808080808080ULL; } // ~FILE_H
uint64_t ne(uint64_t b) { return north(east(b)); }
uint64_t nw(uint64_t b) { return north(west(b)); }
uint64_t se(uint64_t b) { return south(east(b)); }
uint64_t sw(uint64_t b) { return south(west(b)); }

void init_attack_tables() {
    for (int sq = 0; sq < 64; ++sq) {
        uint64_t b = set_bit(sq);
        pawn_attacks_bb[WHITE][sq] = nw(b) | ne(b);
        pawn_attacks_bb[BLACK][sq] = sw(b) | se(b);

        knight_attacks_bb[sq] = (
            ((b << 17) & ~0x0101010101010101ULL) | // Up 2 Right 1
            ((b << 15) & ~0x8080808080808080ULL) | // Up 2 Left 1
            ((b << 10) & ~0x0303030303030303ULL) | // Up 1 Right 2
            ((b << 6)  & ~0xC0C0C0C0C0C0C0C0ULL) | // Up 1 Left 2
            ((b >> 17) & ~0x8080808080808080ULL) | // Down 2 Left 1
            ((b >> 15) & ~0x0101010101010101ULL) | // Down 2 Right 1
            ((b >> 10) & ~0xC0C0C0C0C0C0C0C0ULL) | // Down 1 Left 2
            ((b >> 6)  & ~0x0303030303030303ULL)   // Down 1 Right 2
        );
        
        king_attacks_bb[sq] = north(b) | south(b) | east(b) | west(b) |
                              ne(b) | nw(b) | se(b) | sw(b);
    }
}

uint64_t get_slider_attacks_for_movegen(int sq, Piece piece_type, uint64_t occupied) {
    uint64_t attacks = 0;
    int deltas[8];
    int num_deltas = 0;

    if (piece_type == BISHOP || piece_type == QUEEN) {
        deltas[num_deltas++] = -9; deltas[num_deltas++] = -7; // NW, NE from white's perspective on board
        deltas[num_deltas++] = 7;  deltas[num_deltas++] = 9;  // SW, SE
    }
    if (piece_type == ROOK || piece_type == QUEEN) {
        deltas[num_deltas++] = -8; deltas[num_deltas++] = -1; // N, W
        deltas[num_deltas++] = 1;  deltas[num_deltas++] = 8;  // E, S
    }

    for (int i = 0; i < num_deltas; ++i) {
        int d = deltas[i];
        for (int current_sq = sq + d; ; current_sq += d) {
            if (current_sq < 0 || current_sq >= 64) break; 
            
            int r_curr = current_sq / 8;
            int c_curr = current_sq % 8;
            int r_prev = (current_sq - d) / 8;
            int c_prev = (current_sq - d) % 8;

            // Check for board edge wrap-around
            int dist_r = abs(r_curr - r_prev);
            int dist_c = abs(c_curr - c_prev);

            if (abs(d) == 1 || abs(d) == 8) { // Rook-like moves (straight)
                 if ( (abs(d) == 1 && dist_r != 0) || (abs(d) == 8 && dist_c != 0) ) break; // Wrapped
            } else { // Bishop-like moves (diagonal)
                 if (dist_r != 1 || dist_c != 1) break; // Wrapped or not diagonal
            }

            attacks |= set_bit(current_sq);
            if (get_bit(occupied, current_sq)) break; 
        }
    }
    return attacks;
}

// Corrected is_square_attacked
bool is_square_attacked(const Position& pos, int sq_to_check, int attacker_c) {
    // Pawn attacks
    uint64_t attacker_pawns = pos.piece_bb[PAWN] & pos.color_bb[attacker_c];
    if (pawn_attacks_bb[1 - attacker_c][sq_to_check] & attacker_pawns) return true;

    // Knight attacks
    uint64_t attacker_knights = pos.piece_bb[KNIGHT] & pos.color_bb[attacker_c];
    if (knight_attacks_bb[sq_to_check] & attacker_knights) return true;

    // King attacks
    uint64_t attacker_king = pos.piece_bb[KING] & pos.color_bb[attacker_c];
    if (king_attacks_bb[sq_to_check] & attacker_king) return true;
    
    // Slider checks (Bishops, Rooks, Queens)
    uint64_t all_pieces = pos.get_occupied_bb();

    // Bishop-like attacks (from sq_to_check's perspective, look for enemy B or Q)
    int bishop_deltas[] = {-9, -7, 7, 9}; 
    for (int d : bishop_deltas) {
        for (int current_sq = sq_to_check + d; ; current_sq += d) {
            if (current_sq < 0 || current_sq >= 64) break;
            
            int r_curr = current_sq / 8;
            int c_curr = current_sq % 8;
            int r_prev = (current_sq - d) / 8;
            int c_prev = (current_sq - d) % 8;
            if (abs(r_curr - r_prev) != 1 || abs(c_curr - c_prev) != 1) break; // Wrapped or not diagonal

            if (get_bit(all_pieces, current_sq)) { // Hit a piece
                if (get_bit(pos.color_bb[attacker_c], current_sq) &&
                    (get_bit(pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN], current_sq))) {
                    return true; // Found attacker
                }
                break; // Blocked (by own or other piece)
            }
        }
    }

    // Rook-like attacks
    int rook_deltas[] = {-8, -1, 1, 8};
    for (int d : rook_deltas) {
        for (int current_sq = sq_to_check + d; ; current_sq += d) {
            if (current_sq < 0 || current_sq >= 64) break;

            int r_curr = current_sq / 8;
            // int c_curr = current_sq % 8; // Not needed for straight line wrap check with rank/file
            int r_prev = (current_sq - d) / 8;
            // int c_prev = (current_sq - d) % 8; // Not needed
            if ( (abs(d) == 1 && r_curr != r_prev) || (abs(d) == 8 && (current_sq % 8) != (sq_to_check % 8)) ) break; // Wrapped

            if (get_bit(all_pieces, current_sq)) { // Hit a piece
                if (get_bit(pos.color_bb[attacker_c], current_sq) &&
                    (get_bit(pos.piece_bb[ROOK] | pos.piece_bb[QUEEN], current_sq))) {
                    return true; // Found attacker
                }
                break; // Blocked
            }
        }
    }
    return false;
}


// --- Move Generation ---
void add_move(std::vector<Move>& moves_list, int from, int to, Piece promotion = NO_PIECE) {
    moves_list.push_back({from, to, promotion});
}

void generate_moves(const Position& pos, std::vector<Move>& moves_list, bool captures_only) {
    moves_list.clear();
    int stm = pos.side_to_move;
    uint64_t my_pieces = pos.color_bb[stm];
    uint64_t opp_pieces = pos.color_bb[1 - stm];
    uint64_t occupied = my_pieces | opp_pieces;

    // Pawn moves
    uint64_t pawns = pos.piece_bb[PAWN] & my_pieces;
    while (pawns) {
        int from = lsb_index(pawns);
        pawns &= pawns - 1;
        int rank = from / 8;
        int promotion_rank = (stm == WHITE) ? 6 : 1; 

        // Pushes
        int one_step = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step >=0 && one_step < 64 && !get_bit(occupied, one_step)) { 
            if (!captures_only) {
                if (rank == promotion_rank) {
                    add_move(moves_list, from, one_step, QUEEN); add_move(moves_list, from, one_step, ROOK);
                    add_move(moves_list, from, one_step, BISHOP); add_move(moves_list, from, one_step, KNIGHT);
                } else {
                    add_move(moves_list, from, one_step);
                }
            }
            // Double push
            int start_rank = (stm == WHITE) ? 1 : 6;
            if (rank == start_rank) {
                int two_steps = (stm == WHITE) ? from + 16 : from - 16;
                 if (two_steps >=0 && two_steps < 64 && !get_bit(occupied, two_steps) && !captures_only) { 
                    add_move(moves_list, from, two_steps);
                }
            }
        }
        // Captures
        uint64_t pawn_cap_targets = pawn_attacks_bb[stm][from] & opp_pieces;
        if (pos.ep_square != -1) {
             if (get_bit(pawn_attacks_bb[stm][from], pos.ep_square)) { 
                 pawn_cap_targets |= set_bit(pos.ep_square);
             }
        }

        while (pawn_cap_targets) {
            int to = lsb_index(pawn_cap_targets);
            pawn_cap_targets &= pawn_cap_targets - 1;
            if (rank == promotion_rank) {
                add_move(moves_list, from, to, QUEEN); add_move(moves_list, from, to, ROOK);
                add_move(moves_list, from, to, BISHOP); add_move(moves_list, from, to, KNIGHT);
            } else {
                add_move(moves_list, from, to);
            }
        }
    }

    // Knight moves
    uint64_t knights = pos.piece_bb[KNIGHT] & my_pieces;
    while (knights) {
        int from = lsb_index(knights);
        knights &= knights - 1;
        uint64_t attacks = knight_attacks_bb[from] & (captures_only ? opp_pieces : ~my_pieces);
        while (attacks) {
            int to = lsb_index(attacks);
            attacks &= attacks - 1;
            add_move(moves_list, from, to);
        }
    }
    
    // King moves
    uint64_t king_bb = pos.piece_bb[KING] & my_pieces;
    if (king_bb) { 
        int from = lsb_index(king_bb);
        uint64_t attacks = king_attacks_bb[from] & (captures_only ? opp_pieces : ~my_pieces);
        while (attacks) {
            int to = lsb_index(attacks);
            attacks &= attacks - 1;
            add_move(moves_list, from, to);
        }
    }

    // Sliding pieces (Bishop, Rook, Queen)
    Piece sliders[] = {BISHOP, ROOK, QUEEN};
    for (Piece p_type : sliders) {
        uint64_t slider_pieces = pos.piece_bb[p_type] & my_pieces;
        while (slider_pieces) {
            int from = lsb_index(slider_pieces);
            slider_pieces &= slider_pieces - 1;
            uint64_t attacks = get_slider_attacks_for_movegen(from, p_type, occupied) & (captures_only ? opp_pieces : ~my_pieces);
            while (attacks) {
                int to = lsb_index(attacks);
                attacks &= attacks - 1;
                add_move(moves_list, from, to);
            }
        }
    }

    // Castling
    if (!captures_only) {
        int king_sq = lsb_index(pos.piece_bb[KING] & my_pieces); 
        if (king_sq != -1) { 
            if (stm == WHITE) {
                if ((pos.castling_rights & 1) && // White Kingside (K) e1g1
                    !get_bit(occupied, 5) && !get_bit(occupied, 6) && 
                    !is_square_attacked(pos, 4, BLACK) && !is_square_attacked(pos, 5, BLACK) && !is_square_attacked(pos, 6, BLACK)) {
                    add_move(moves_list, king_sq, 6);
                }
                if ((pos.castling_rights & 2) && // White Queenside (Q) e1c1
                    !get_bit(occupied, 3) && !get_bit(occupied, 2) && !get_bit(occupied, 1) && 
                    !is_square_attacked(pos, 4, BLACK) && !is_square_attacked(pos, 3, BLACK) && !is_square_attacked(pos, 2, BLACK)) {
                    add_move(moves_list, king_sq, 2);
                }
            } else { // BLACK
                if ((pos.castling_rights & 4) && // Black Kingside (k) e8g8
                    !get_bit(occupied, 61) && !get_bit(occupied, 62) &&
                    !is_square_attacked(pos, 60, WHITE) && !is_square_attacked(pos, 61, WHITE) && !is_square_attacked(pos, 62, WHITE)) {
                    add_move(moves_list, king_sq, 62);
                }
                if ((pos.castling_rights & 8) && // Black Queenside (q) e8c8
                    !get_bit(occupied, 59) && !get_bit(occupied, 58) && !get_bit(occupied, 57) &&
                    !is_square_attacked(pos, 60, WHITE) && !is_square_attacked(pos, 59, WHITE) && !is_square_attacked(pos, 58, WHITE)) {
                    add_move(moves_list, king_sq, 58);
                }
            }
        }
    }
}


// --- Make Move ---
Position make_move(const Position& pos, const Move& move, bool* legal_move_ptr) {
    Position next_pos = pos;
    int stm = pos.side_to_move;
    int opp = 1 - stm;

    uint64_t from_bb = set_bit(move.from);
    uint64_t to_bb = set_bit(move.to);
    Piece piece_moved = pos.piece_on(move.from);
    Piece piece_captured = pos.piece_on(move.to); 

    if (piece_moved == NO_PIECE) { 
        if (legal_move_ptr) *legal_move_ptr = false;
        return pos; 
    }

    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.piece_bb[piece_moved] |= to_bb;
    next_pos.color_bb[stm] &= ~from_bb;
    next_pos.color_bb[stm] |= to_bb;

    next_pos.halfmove_clock++;

    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.halfmove_clock = 0;
    }
    if (piece_moved == PAWN) next_pos.halfmove_clock = 0;

    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) { // Added ep_square != -1 check
        int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
        uint64_t captured_pawn_bb = set_bit(captured_pawn_sq);
        next_pos.piece_bb[PAWN] &= ~captured_pawn_bb;
        next_pos.color_bb[opp] &= ~captured_pawn_bb;
    }

    next_pos.ep_square = -1;
    if (piece_moved == PAWN) {
        if (abs(move.to - move.from) == 16) { 
            next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
        }
    }

    if (move.promotion != NO_PIECE) {
        next_pos.piece_bb[PAWN] &= ~to_bb; 
        next_pos.piece_bb[move.promotion] |= to_bb; 
    }
    
    if (piece_moved == KING && abs(move.to - move.from) == 2) {
        int rook_from_sq, rook_to_sq;
        if (move.to == 6) { rook_from_sq = 7; rook_to_sq = 5; } 
        else if (move.to == 2) { rook_from_sq = 0; rook_to_sq = 3; } 
        else if (move.to == 62) { rook_from_sq = 63; rook_to_sq = 61; } 
        else { rook_from_sq = 56; rook_to_sq = 59; } 

        uint64_t rook_from_bb = set_bit(rook_from_sq);
        uint64_t rook_to_bb = set_bit(rook_to_sq);
        next_pos.piece_bb[ROOK] &= ~rook_from_bb;
        next_pos.piece_bb[ROOK] |= rook_to_bb;
        next_pos.color_bb[stm] &= ~rook_from_bb;
        next_pos.color_bb[stm] |= rook_to_bb;
    }

    uint8_t old_castling_rights = next_pos.castling_rights;
    if (piece_moved == KING) {
        if (stm == WHITE) next_pos.castling_rights &= ~3; 
        else next_pos.castling_rights &= ~12; 
    }
    // If a rook moves from its starting square or is captured on its starting square
    if (move.from == 0 || (piece_captured != NO_PIECE && move.to == 0)) next_pos.castling_rights &= ~2; // WQ castle (a1 rook)
    if (move.from == 7 || (piece_captured != NO_PIECE && move.to == 7)) next_pos.castling_rights &= ~1; // WK castle (h1 rook)
    if (move.from == 56 || (piece_captured != NO_PIECE && move.to == 56)) next_pos.castling_rights &= ~8; // BQ castle (a8 rook)
    if (move.from == 63 || (piece_captured != NO_PIECE && move.to == 63)) next_pos.castling_rights &= ~4; // BK castle (h8 rook)


    next_pos.side_to_move = opp;
    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply = pos.ply + 1;

    uint64_t new_hash = pos.zobrist_hash;
    new_hash ^= zobrist_pieces[stm][piece_moved][move.from];              
    
    Piece piece_for_to_sq_hash = (move.promotion != NO_PIECE) ? move.promotion : piece_moved;
    new_hash ^= zobrist_pieces[stm][piece_for_to_sq_hash][move.to];

    if (piece_captured != NO_PIECE) {
        if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) { // Added ep_square != -1
            int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
            new_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
        } else {
            new_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        }
    }
    if (pos.ep_square != -1) new_hash ^= zobrist_ep[pos.ep_square];
    if (next_pos.ep_square != -1) new_hash ^= zobrist_ep[next_pos.ep_square];
    if (old_castling_rights != next_pos.castling_rights) {
        new_hash ^= zobrist_castling[old_castling_rights];
        new_hash ^= zobrist_castling[next_pos.castling_rights];
    }
    new_hash ^= zobrist_side_to_move;
    
    if (piece_moved == KING && abs(move.to - move.from) == 2) {
        int rook_from_sq, rook_to_sq;
        if (move.to == 6) { rook_from_sq = 7; rook_to_sq = 5; } 
        else if (move.to == 2) { rook_from_sq = 0; rook_to_sq = 3; } 
        else if (move.to == 62) { rook_from_sq = 63; rook_to_sq = 61; } 
        else { rook_from_sq = 56; rook_to_sq = 59; } 
        new_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
        new_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
    }
    next_pos.zobrist_hash = new_hash;

    int king_sq_after_move = lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq_after_move != -1 && is_square_attacked(next_pos, king_sq_after_move, opp)) {
        if (legal_move_ptr) *legal_move_ptr = false;
        return pos; 
    }

    if (legal_move_ptr) *legal_move_ptr = true;
    return next_pos;
}

// --- Evaluation ---
const int piece_values_mg[6] = {100, 320, 330, 500, 900, 20000};
const int piece_values_eg[6] = {120, 320, 330, 530, 950, 20000};

const int pawn_pst_mg[64] = {
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10,-20,-20, 10, 10,  5,
     5, -5,-10,  0,  0,-10, -5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5,  5, 10, 25, 25, 10,  5,  5,
    10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,
     0,  0,  0,  0,  0,  0,  0,  0
};
const int knight_pst_mg[64] = {
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50
};
const int bishop_pst_mg[64] = {
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20
};
const int rook_pst_mg[64] = {
     0,  0,  0,  5,  5,  0,  0,  0,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     5, 10, 10, 10, 10, 10, 10,  5, 
     0,  0,  0,  0,  0,  0,  0,  0
};
const int queen_pst_mg[64] = {
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -10,  5,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5, // Corrected line from image (was -5 start)
    -10,  0,  5,  5,  5,  5,  0,-10,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
};
const int king_pst_mg[64] = { 
    20, 30, 10,  0,  0, 10, 30, 20, 
    20, 20,  0,  0,  0,  0, 20, 20,
   -10,-20,-20,-20,-20,-20,-20,-10,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30
};
const int king_pst_eg[64] = { 
   -50,-30,-30,-30,-30,-30,-30,-50,
   -30,-30,  0,  0,  0,  0,-30,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-30,  0,  0,  0,  0,-30,-30,
   -50,-30,-30,-30,-30,-30,-30,-50
};


const int* pst_lookup_mg[6] = {pawn_pst_mg, knight_pst_mg, bishop_pst_mg, rook_pst_mg, queen_pst_mg, king_pst_mg};
const int pawn_pst_eg[64] = { 
     0,  0,  0,  0,  0,  0,  0,  0,
    10, 20, 20, -5, -5, 20, 20, 10, 
    10,  5,  0, 10, 10,  0,  5, 10,
    20, 20, 30, 40, 40, 30, 20, 20,
    30, 30, 40, 50, 50, 40, 30, 30,
    50, 50, 60, 70, 70, 60, 50, 50,
    80, 80, 80, 80, 80, 80, 80, 80, 
     0,  0,  0,  0,  0,  0,  0,  0
};
const int* pst_lookup_eg[6] = {pawn_pst_eg, knight_pst_mg, bishop_pst_mg, rook_pst_mg, queen_pst_mg, king_pst_eg};


int get_pst_score(Piece p_type, int sq, Color c, bool is_eg) {
    const int* table = is_eg ? pst_lookup_eg[p_type] : pst_lookup_mg[p_type];
    return (c == WHITE) ? table[sq] : table[63 - sq]; 
}

const int piece_phase_values[6] = {0, 1, 1, 2, 4, 0}; 

int evaluate(const Position& pos) {
    int mg_score = 0;
    int eg_score = 0;
    int game_phase = 0;

    for (int c_idx = 0; c_idx < 2; ++c_idx) { 
        Color current_eval_color = (Color)c_idx;
        int side_val_multiplier = (current_eval_color == WHITE) ? 1 : -1; 
        
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * piece_phase_values[p];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                mg_score += side_val_multiplier * (piece_values_mg[p] + get_pst_score((Piece)p, sq, current_eval_color, false));
                eg_score += side_val_multiplier * (piece_values_eg[p] + get_pst_score((Piece)p, sq, current_eval_color, true));
            }
        }
    }
    
    std::vector<Move> moves_eval; 
    generate_moves(pos, moves_eval); 
    int mobility_val = 0;
    for(const auto& m_eval : moves_eval) { 
        bool legal_eval_move;
        make_move(pos, m_eval, &legal_eval_move); // Test if move is legal
        if (legal_eval_move) mobility_val++;
    }
    int mobility_score_component = mobility_val * 2; 

    if (pos.side_to_move == WHITE) {
        mg_score += mobility_score_component;
        eg_score += mobility_score_component;
    } else {
        mg_score -= mobility_score_component;
        eg_score -= mobility_score_component;
    }

    if (game_phase > 24) game_phase = 24; 
    if (game_phase < 0) game_phase = 0; 
    
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
uint64_t tt_mask = 0; // Initialize to 0, effective disables TT until init_tt

void init_tt(size_t mb_size) {
    if (mb_size == 0) { // Explicitly handle 0 MB case
        std::cerr << "Warning: TT size is 0 MB. TT will be disabled." << std::endl;
        transposition_table.clear();
        tt_mask = 0;
        return;
    }
    size_t num_entries = (mb_size * 1024 * 1024) / sizeof(TTEntry);
    size_t power_of_2_entries = 1;
    // Ensure power_of_2_entries doesn't become 0 if num_entries is small but non-zero.
    // And handle num_entries = 0 case by keeping power_of_2_entries = 0 (or 1 then clear).
    if (num_entries == 0) { // Should be caught by mb_size == 0, but defensive
        power_of_2_entries = 0;
    } else {
        while(power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries /* avoid overflow */ ) { 
            power_of_2_entries *= 2;
        }
    }

    if (power_of_2_entries == 0) {
         std::cerr << "Warning: Calculated TT entries is 0. TT will be disabled." << std::endl;
         transposition_table.clear();
         tt_mask = 0;
         return;
    }

    try {
        transposition_table.assign(power_of_2_entries, TTEntry());
        tt_mask = power_of_2_entries - 1;
        std::cout << "info string TT initialized with " << power_of_2_entries << " entries (" << mb_size << "MB)." << std::endl;
    } catch (const std::bad_alloc& e) {
        std::cerr << "Error: Failed to allocate TT memory (" << mb_size << "MB): " << e.what() << ". TT will be disabled." << std::endl;
        transposition_table.clear();
        tt_mask = 0;
    }
}

void clear_tt() {
    for(auto& entry : transposition_table) {
        entry = TTEntry(); 
    }
}

bool probe_tt(uint64_t hash, int depth, int& alpha, int& beta, Move& move_to_try, int& score_from_tt) {
    if (transposition_table.empty() || tt_mask == 0) return false; 
    TTEntry& entry = transposition_table[hash & tt_mask];
    if (entry.hash == hash && entry.bound != TT_NONE) {
        move_to_try = entry.best_move; 
        if (entry.depth >= depth) {
            score_from_tt = entry.score;
            if (entry.bound == TT_EXACT) return true;
            if (entry.bound == TT_LOWER && score_from_tt >= beta) return true;
            if (entry.bound == TT_UPPER && score_from_tt <= alpha) return true;
        }
    }
    return false;
}

void store_tt(uint64_t hash, int depth, int score, TTBound bound, const Move& best_move, int ply_from_root) {
    if (transposition_table.empty() || tt_mask == 0) return; 
    TTEntry& entry = transposition_table[hash & tt_mask];
    
    // Prefer to keep entries that are deeper, or if same depth, prefer EXACT bounds.
    // Also, always write if it's a new hash (different from current entry.hash).
    bool should_replace = (entry.hash != hash) || 
                          (depth > entry.depth) || 
                          (depth == entry.depth && bound == TT_EXACT && entry.bound != TT_EXACT) ||
                          (depth == entry.depth && bound != TT_NONE && entry.bound == TT_NONE); // Overwrite empty slot of same depth
    
    if (should_replace) {
        entry.hash = hash;
        entry.depth = depth;
        entry.score = score;
        entry.bound = bound;
        // Store best_move if it's not null, or if it's an exact score or lower bound (PV move),
        // or if it's a new hash entry (then it might be null if no good move was found yet for this TT_UPPER).
        if (!best_move.is_null() || bound == TT_EXACT || bound == TT_LOWER || entry.hash != hash ) {
             entry.best_move = best_move;
        }
    }
}

// --- Search Globals & Helpers ---
std::chrono::steady_clock::time_point search_start_timepoint;
long long search_budget_ms = 0;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;
Move killer_moves[MAX_PLY][2];
int history_heuristic[2][64][64]; 
std::vector<uint64_t> game_history_hashes; 

void reset_search_globals() {
    nodes_searched = 0;
    stop_search_flag = false;
    for(int i=0; i<MAX_PLY; ++i) {
        killer_moves[i][0] = NULL_MOVE;
        killer_moves[i][1] = NULL_MOVE;
    }
    std::memset(history_heuristic, 0, sizeof(history_heuristic));
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) { 
        if (search_budget_ms > 0) { // Only check if a budget is set
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

// --- Search ---
int quiescence_search(Position& pos, int alpha, int beta) {
    nodes_searched++;
    if (check_time()) return 0; 

    int stand_pat = evaluate(pos);
    if (stand_pat >= beta) return beta;
    if (alpha < stand_pat) alpha = stand_pat;

    std::vector<Move> captures;
    generate_moves(pos, captures, true); 

    for (Move& m : captures) {
        Piece moved = pos.piece_on(m.from);
        Piece captured = pos.piece_on(m.to); // Could be NO_PIECE if EP
        if (moved == NO_PIECE) continue; // Should not happen for generated moves

        if (captured != NO_PIECE) { 
            m.score = piece_values_mg[captured] * 10 - piece_values_mg[moved];
        } else if (pos.ep_square == m.to && moved == PAWN && pos.ep_square != -1) { 
            m.score = piece_values_mg[PAWN] * 10 - piece_values_mg[PAWN] + 1; 
        } else {
            m.score = 0; 
        }
    }
    std::sort(captures.begin(), captures.end(), [](const Move& a, const Move& b){ return a.score > b.score; });

    for (const Move& cap_move : captures) {
        bool legal;
        Position next_pos = make_move(pos, cap_move, &legal);
        if (!legal) continue;

        int score = -quiescence_search(next_pos, -beta, -alpha);
        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }
    return alpha;
}


int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, std::vector<uint64_t>& current_path_hashes) {
    nodes_searched++;
    if (check_time()) return 0;
    if (depth <= 0) return quiescence_search(pos, alpha, beta);
    if (ply >= MAX_PLY -1) return evaluate(pos); 

    int original_alpha = alpha; // For TT bound type

    // Repetition detection
    if (ply > 0) { // No repetition at root
        for(size_t i = 0; i < current_path_hashes.size(); ++i) { // Check current path
            if(current_path_hashes[i] == pos.zobrist_hash) {
                 // If this is the second time we see this hash on the path (first time was when it was added)
                 // it means a repetition has occurred in the search.
                 return 0; // Draw by repetition
            }
        }
        // Check game history for third repetition
        int game_reps = 0;
        for(uint64_t h : game_history_hashes) if(h == pos.zobrist_hash) game_reps++;
        if(game_reps >= 2) return 0; // Third occurrence of this position in game
    }


    if (pos.halfmove_clock >= 100) return 0;

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth++; 

    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, alpha, beta, tt_move, tt_score)) {
         return tt_score;
    }

    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0) {
        uint64_t our_pieces_non_pawn_king = pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING]);
        if (our_pieces_non_pawn_king != 0) { // Has at least one minor/major piece
            Position null_next_pos = pos; 
            null_next_pos.side_to_move = 1 - pos.side_to_move;
            uint64_t old_ep_hash_part = (pos.ep_square != -1) ? zobrist_ep[pos.ep_square] : 0;
            null_next_pos.ep_square = -1;
            null_next_pos.ply = pos.ply + 1; // Increment ply for null move child
            
            null_next_pos.zobrist_hash = pos.zobrist_hash ^ zobrist_side_to_move ^ old_ep_hash_part;
            
            int r = (depth > 6) ? 3 : 2; 
            current_path_hashes.push_back(pos.zobrist_hash);
            int null_score = -search(null_next_pos, depth - 1 - r, -beta, -beta + 1, ply + 1, false, false, current_path_hashes);
            current_path_hashes.pop_back();

            if (stop_search_flag) return 0; 
            if (null_score >= beta) {
                 // Verification search for very high depths (optional, can be complex)
                 // if (null_score >= MATE_THRESHOLD) return beta; // Avoid returning unverified mate
                 store_tt(pos.zobrist_hash, depth, beta, TT_LOWER, NULL_MOVE, ply); // Store beta, as it's a lower bound
                 return beta; 
            }
        }
    }


    std::vector<Move> moves;
    generate_moves(pos, moves);

    for (Move& m : moves) {
        if (!tt_move.is_null() && m == tt_move) m.score = 100000; 
        else {
            Piece captured = pos.piece_on(m.to);
            if (captured != NO_PIECE) { 
                Piece moved = pos.piece_on(m.from); 
                m.score = 80000 + ( (captured!=NO_PIECE ? piece_values_mg[captured]:0) * 10 - ( (moved!=NO_PIECE) ? piece_values_mg[moved] : 0) );
            } else if (!killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) m.score = 70000;
            else if (!killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) m.score = 60000;
            else m.score = history_heuristic[pos.side_to_move][m.from][m.to];
        }
    }
    std::sort(moves.begin(), moves.end(), [](const Move& a, const Move& b){ return a.score > b.score; });

    int best_score = -MATE_SCORE - 10; // Slightly lower than -MATE_SCORE to distinguish from actual mate
    Move best_move_found = NULL_MOVE;
    int legal_moves_played = 0;

    current_path_hashes.push_back(pos.zobrist_hash);

    for (int i = 0; i < (int)moves.size(); ++i) {
        const Move& current_move = moves[i];
        bool legal;
        Position next_pos = make_move(pos, current_move, &legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;
        int current_eval_depth = depth -1; 

        // LMR
        if (depth >= 3 && i >= (is_pv_node ? 4 : 2) && !in_check && current_move.promotion == NO_PIECE && pos.piece_on(current_move.to)==NO_PIECE) {
            int reduction = 1;
            if (current_move.score < 60000) { // Not TT move or killer, potentially reduce more
                 reduction += (depth > 5 && i > (is_pv_node ? 6:4) ) ? 1 : 0;
            }
            reduction = std::min(reduction, current_eval_depth -1); 
            if (reduction < 0) reduction = 0;

            current_eval_depth -= reduction;
            
            score = -search(next_pos, current_eval_depth, -alpha - 1, -alpha, ply + 1, false, true, current_path_hashes);
            if (score > alpha && reduction > 0) { 
                 current_eval_depth += reduction; 
                 score = -search(next_pos, current_eval_depth, -alpha-1, -alpha, ply+1, false, true, current_path_hashes);
            }
        } else {
            // No LMR, current_eval_depth remains depth-1 for the first few/forcing moves
        }
        if (stop_search_flag) { current_path_hashes.pop_back(); return 0; }


        if ((is_pv_node && legal_moves_played > 1) || (!is_pv_node && score > alpha && current_eval_depth == depth-1) ) { 
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, is_pv_node && (legal_moves_played==1), true, current_path_hashes);
        } else if (legal_moves_played == 1 && is_pv_node) { 
             score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_path_hashes);
        }
        // else: score from LMR or initial ZWS was not > alpha, so it's the score (unless it was an LMR re-search).

        if (stop_search_flag) { current_path_hashes.pop_back(); return 0; }

        if (score > best_score) {
            best_score = score;
            best_move_found = current_move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) {
                    if (pos.piece_on(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) { 
                        if (! (current_move == killer_moves[ply][0])) { 
                            killer_moves[ply][1] = killer_moves[ply][0];
                            killer_moves[ply][0] = current_move;
                        }
                        history_heuristic[pos.side_to_move][current_move.from][current_move.to] += depth * depth;
                        for(int j=0; j < i; ++j) {
                            if(pos.piece_on(moves[j].to) == NO_PIECE && moves[j].promotion == NO_PIECE) {
                                history_heuristic[pos.side_to_move][moves[j].from][moves[j].to] -= (depth*depth/4); // Less penalty
                                if(history_heuristic[pos.side_to_move][moves[j].from][moves[j].to] < 0)
                                    history_heuristic[pos.side_to_move][moves[j].from][moves[j].to] = 0;
                            }
                        }
                    }
                    current_path_hashes.pop_back();
                    store_tt(pos.zobrist_hash, depth, beta, TT_LOWER, best_move_found, ply);
                    return beta; 
                }
            }
        }
    }
    current_path_hashes.pop_back();

    if (legal_moves_played == 0) { 
        return in_check ? (-MATE_SCORE + ply) : 0; 
    }
    
    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, best_score, final_bound_type, best_move_found, ply);
    return best_score;
}

// --- UCI ---
Position root_pos;
Move best_move_overall; 

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
            c = std::tolower(c);
            if (c == 'p') p_type = PAWN; else if (c == 'n') p_type = KNIGHT;
            else if (c == 'b') p_type = BISHOP; else if (c == 'r') p_type = ROOK;
            else if (c == 'q') p_type = QUEEN; else if (c == 'k') p_type = KING;

            if (p_type != NO_PIECE) {
                int sq = rank * 8 + file;
                if (sq >=0 && sq < 64) { // Basic bounds check for safety
                    pos.piece_bb[p_type] |= set_bit(sq);
                    pos.color_bb[p_color] |= set_bit(sq);
                }
            }
            file++;
        }
    }

    ss >> part; pos.side_to_move = (part == "w") ? WHITE : BLACK;
    
    ss >> part;
    pos.castling_rights = 0;
    for (char c : part) {
        if (c == 'K') pos.castling_rights |= 1; else if (c == 'Q') pos.castling_rights |= 2;
        else if (c == 'k') pos.castling_rights |= 4; else if (c == 'q') pos.castling_rights |= 8;
    }

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
    
    if (ss >> part) { try {pos.halfmove_clock = std::stoi(part);} catch(...) {pos.halfmove_clock = 0;} } else pos.halfmove_clock = 0;
    if (ss >> part) { try {pos.fullmove_number = std::stoi(part);} catch(...){pos.fullmove_number = 1;} } else pos.fullmove_number = 1;
    
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_hashes.clear(); 
}

Move parse_uci_move(const Position& pos, const std::string& uci_move_str) {
    Move m;
    if (uci_move_str.length() < 4) return NULL_MOVE; // Invalid format

    m.from = (uci_move_str[0] - 'a') + (uci_move_str[1] - '1') * 8;
    m.to = (uci_move_str[2] - 'a') + (uci_move_str[3] - '1') * 8;

    if (m.from < 0 || m.from > 63 || m.to < 0 || m.to > 63) return NULL_MOVE; // Invalid square

    if (uci_move_str.length() == 5) {
        char promo_char = uci_move_str[4];
        if (promo_char == 'q') m.promotion = QUEEN;
        else if (promo_char == 'n') m.promotion = KNIGHT;
        else if (promo_char == 'b') m.promotion = BISHOP;
        else if (promo_char == 'r') m.promotion = ROOK;
        else m.promotion = NO_PIECE; // Invalid promotion char
    } else {
        m.promotion = NO_PIECE;
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
    if (pos.ep_square != -1) h ^= zobrist_ep[pos.ep_square];
    if (pos.side_to_move == BLACK) h ^= zobrist_side_to_move;
    return h;
}

void uci_loop() {
    std::string line, token;
    parse_fen(root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"); 

    while (std::getline(std::cin, line)) {
        std::stringstream ss(line);
        ss >> token;

        if (token == "uci") {
            std::cout << "id name SimpleEngineOneFile\n";
            std::cout << "id author YourName\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB << " min 0 max 128\n"; // min 0 to allow disabling
            std::cout << "uciok\n";
        } else if (token == "isready") {
            std::cout << "readyok\n";
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str;
            ss >> name_token; 
            if (name_token == "name") {
                ss >> name_str; 
                ss >> value_token; 
                if (value_token == "value") {
                    ss >> value_str;
                     if (name_str == "Hash") {
                        try {
                            init_tt(std::stoi(value_str));
                        } catch (...) {
                            std::cerr << "info string Error parsing Hash value. Using default." << std::endl;
                            init_tt(TT_SIZE_MB);
                        }
                    }
                }
            }
        }
        else if (token == "ucinewgame") {
            parse_fen(root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            clear_tt();
            std::memset(history_heuristic, 0, sizeof(history_heuristic));
            game_history_hashes.clear();
        } else if (token == "position") {
            ss >> token;
            if (token == "startpos") {
                parse_fen(root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                if (ss.peek() != EOF && ss.str().find("moves") != std::string::npos) ss >> token; // Consume "moves" if present
                 else token = ""; // No moves token
            } else if (token == "fen") {
                std::string fen_str_parts;
                std::string temp_part;
                for(int i=0; i<6; ++i) { 
                    if(!(ss >> temp_part)) break; 
                    fen_str_parts += temp_part + (i==5 ? "" : " ");
                }
                parse_fen(root_pos, fen_str_parts);
                 if (ss.peek() != EOF && ss.str().find("moves") != std::string::npos) ss >> token; // Consume "moves" if present
                 else token = ""; // No moves token
            }
            
            if (token == "moves") {
                // game_history_hashes should track states *after* a move is made.
                // The initial position (after FEN or startpos) is the first state.
                game_history_hashes.push_back(root_pos.zobrist_hash); 
                while (ss >> token) { // For each UCI move like "e2e4", "d7d5"
                    Move m = parse_uci_move(root_pos, token);
                    if (m.is_null()) { // Invalid move string format
                        std::cerr << "info string Error parsing UCI move: " << token << std::endl;
                        break;
                    }
                    bool legal;
                    root_pos = make_move(root_pos, m, &legal);
                    if (!legal) { 
                        std::cerr << "info string Illegal UCI move received: " << token << std::endl;
                        // GUI sent an illegal move. root_pos is reverted.
                        // The game_history_hashes should not include the hash of the illegal state.
                        // The last valid hash is already in game_history_hashes.
                        break;
                    } 
                    game_history_hashes.push_back(root_pos.zobrist_hash);
                }
            }
        } else if (token == "go") {
            int wtime = -1, btime = -1, winc = 0, binc = 0, movestogo = 0;
            long long time_for_move = 5000; // Default 5s if no time control

            while (ss >> token) {
                if (token == "wtime") ss >> wtime;
                else if (token == "btime") ss >> btime;
                else if (token == "winc") ss >> winc;
                else if (token == "binc") ss >> binc;
                else if (token == "movestogo") ss >> movestogo;
            }

            int my_time = (root_pos.side_to_move == WHITE) ? wtime : btime;
            int my_inc = (root_pos.side_to_move == WHITE) ? winc : binc;

            if (my_time != -1) { 
                if (movestogo > 0 && movestogo < 30) { // Avoid dividing by large movestogo
                    time_for_move = (my_time / movestogo);
                } else { // More dynamic time allocation
                    time_for_move = (my_time / 25); 
                    if (my_time < 10000) time_for_move = my_time / 15; // Use more time if low on time
                }
                time_for_move += my_inc;
                time_for_move -= 50; // Small buffer for overhead
                
                if (time_for_move < 100 && my_time > 100) time_for_move = 100; 
                else if (time_for_move <= 0) time_for_move = 50; 
                if (my_time > 0 && time_for_move > my_time * 0.9) time_for_move = (long long)(my_time * 0.9); 
            }
             if (time_for_move <= 0) time_for_move = 100; // Absolute minimum if all else fails
            
            search_budget_ms = time_for_move;
            search_start_timepoint = std::chrono::steady_clock::now();
            reset_search_globals();
            
            best_move_overall = NULL_MOVE;
            int best_score_overall = 0;

            std::vector<uint64_t> root_path_hashes; 
            for (int depth = 1; depth <= MAX_PLY; ++depth) {
                int current_score = search(root_pos, depth, -MATE_SCORE-100, MATE_SCORE+100, 0, true, true, root_path_hashes);
                
                if (stop_search_flag && depth > 1) { 
                    break;
                }
                
                best_score_overall = current_score;
                Move tt_move_probe = NULL_MOVE; int tt_score_probe; int dummy_alpha = -MATE_SCORE-100, dummy_beta = MATE_SCORE+100;
                // Probe TT specifically for the root position after search of this depth
                if(probe_tt(root_pos.zobrist_hash, depth, dummy_alpha, dummy_beta, tt_move_probe, tt_score_probe)){
                     if(!tt_move_probe.is_null()) best_move_overall = tt_move_probe;
                }

                auto now_tp = std::chrono::steady_clock::now();
                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_tp - search_start_timepoint).count();
                if (elapsed_ms < 0) elapsed_ms = 0; // Clock might rarely go backwards slightly on some systems
                
                std::cout << "info depth " << depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) std::cout << " mate " << (MATE_SCORE - best_score_overall + root_pos.ply +1 )/2 ; 
                else if (best_score_overall < -MATE_THRESHOLD) std::cout << " mate " << -(MATE_SCORE + best_score_overall + root_pos.ply)/2; 
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);
                if (!best_move_overall.is_null()) {
                     std::cout << " pv " << move_to_uci(best_move_overall);
                }
                std::cout << std::endl;

                if (abs(best_score_overall) > MATE_THRESHOLD) break; 
                if (search_budget_ms > 0 && elapsed_ms > 0 && elapsed_ms * 1.8 > search_budget_ms && depth > 2) break; // Be more conservative
            }

            if (!best_move_overall.is_null()) {
                 std::cout << "bestmove " << move_to_uci(best_move_overall) << std::endl;
            } else { 
                std::vector<Move> legal_moves_fallback;
                generate_moves(root_pos, legal_moves_fallback);
                bool found_one_legal = false;
                for(const auto& m_fall : legal_moves_fallback) {
                    bool is_leg_fall;
                    make_move(root_pos, m_fall, &is_leg_fall); 
                    if(is_leg_fall) {
                        std::cout << "bestmove " << move_to_uci(m_fall) << std::endl;
                        found_one_legal = true;
                        break;
                    }
                }
                if(!found_one_legal && !legal_moves_fallback.empty()) { 
                     // This case means all generated moves were illegal, which is bad.
                     // Outputting the first generated might still be illegal.
                     // For safety, output a null move if possible or handle error.
                     // Here, we'll just output the first one and hope for the best (not ideal).
                     std::cout << "bestmove " << move_to_uci(legal_moves_fallback[0]) << std::endl; 
                } else if (legal_moves_fallback.empty()) {
                    // No legal moves means checkmate or stalemate. Search should have reported mate.
                    // If best_move_overall is still null here, it's a problem.
                    // GUIs usually handle game end based on engine's mate scores or lack of moves.
                    // std::cout << "info string No legal moves found at root!" << std::endl; // For debugging
                }
            }

        } else if (token == "quit") {
            break;
        }
    }
}


// --- Main ---
int main(int argc, char* argv[]) {
    std::ios_base::sync_with_stdio(false); 
    init_zobrist();
    init_attack_tables();
    init_tt(TT_SIZE_MB); 
    uci_loop();
    return 0;
}
