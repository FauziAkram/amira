#include <cstdio>   // For printf, fgets, sscanf, sprintf, fflush
#include <cstring>  // For std::memset, strcpy, strlen, strcmp
#include <cstdint>
#include <cstdlib>  // For atoi
#include <ctime>    // For clock_t, clock, CLOCKS_PER_SEC
#include <cctype>   // For std::isdigit, std::islower, std::tolower

// Bit manipulation builtins (MSVC/GCC specific)
#if defined(_MSC_VER)
#include <intrin.h>
#endif

// --- Constants ---
enum Piece { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE };
enum Color { WHITE, BLACK, NO_COLOR };

constexpr int MAX_PLY = 64;
constexpr int MAX_MOVES_PER_PLY = 256; // Max legal moves in a position
constexpr int MAX_GAME_HISTORY_PLY = 1024; // Max ply for game history (for 3-fold rep)

// --- TT placeholders (will likely be removed/simplified for 4k) ---
constexpr int TT_SIZE_MB_DEFAULT = 0; // Defaulting to 0MB, essentially disabling for now
// --- End TT placeholders ---


constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY;
constexpr int INF_SCORE = 32000;

// Forward Declarations
struct Move;
struct Position;
int evaluate(const Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
int generate_moves(const Position& pos, Move moves_list[], bool captures_only = false); // Returns count
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);


// --- Tiny PRNG for Zobrist (if kept) ---
uint64_t g_zobrist_seed = 0xCEC;
uint64_t tiny_rand64() {
    g_zobrist_seed = g_zobrist_seed * 6364136223846793005ULL + 1442695040888963407ULL;
    return g_zobrist_seed;
}

// --- Zobrist Hashing ---
uint64_t zobrist_pieces[2][6][64];
uint64_t zobrist_castling[16];
uint64_t zobrist_ep[65]; // 64 squares + 1 for no EP square (index 64)
uint64_t zobrist_side_to_move;

void init_zobrist() {
    for (int c = 0; c < 2; ++c)
        for (int p = 0; p < 6; ++p)
            for (int s = 0; s < 64; ++s)
                zobrist_pieces[c][p][s] = tiny_rand64();
    for (int i = 0; i < 16; ++i)
        zobrist_castling[i] = tiny_rand64();
    for (int i = 0; i < 65; ++i)
        zobrist_ep[i] = tiny_rand64();
    zobrist_side_to_move = tiny_rand64();
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

// Buffer for move_to_uci must be at least 6 chars ("a1a2q\0")
void move_to_uci(const Move& move, char* uci_move_str) {
    if (move.is_null()) {
        strcpy(uci_move_str, "0000");
        return;
    }
    uci_move_str[0] = (char)('a' + (move.from % 8));
    uci_move_str[1] = (char)('1' + (move.from / 8));
    uci_move_str[2] = (char)('a' + (move.to % 8));
    uci_move_str[3] = (char)('1' + (move.to / 8));
    int len = 4;
    if (move.promotion != NO_PIECE) {
        char promo_char = 'q';
        if (move.promotion == KNIGHT) promo_char = 'n';
        else if (move.promotion == BISHOP) promo_char = 'b';
        else if (move.promotion == ROOK) promo_char = 'r';
        uci_move_str[len++] = promo_char;
    }
    uci_move_str[len] = '\0';
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
        for (int p_idx = PAWN; p_idx <= KING; ++p_idx) {
            if (piece_bb[p_idx] & b) return (Piece)p_idx;
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
    const int deltas[] = {1, -1, 8, -8}; // E, W, N, S
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (abs(d) == 1 && r_curr != r_prev) break;
            if (abs(d) == 8 && c_curr != c_prev && sq%8 != s%8) break; 

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
// Now returns count and takes array + pointer to count
void add_move_to_list(Move moves_array[], int& move_count, int from, int to, Piece promotion = NO_PIECE, int score = 0) {
    if (move_count < MAX_MOVES_PER_PLY) {
        moves_array[move_count++] = {from, to, promotion, score};
    }
}

int generate_moves(const Position& pos, Move moves_list[], bool captures_only) {
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
        int promotion_rank_idx = (stm == WHITE) ? 6 : 1;

        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >=0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (!captures_only) {
                if (rank == promotion_rank_idx) {
                    add_move_to_list(moves_list, move_count, from, one_step_sq, QUEEN); add_move_to_list(moves_list, move_count, from, one_step_sq, ROOK);
                    add_move_to_list(moves_list, move_count, from, one_step_sq, BISHOP); add_move_to_list(moves_list, move_count, from, one_step_sq, KNIGHT);
                } else {
                    add_move_to_list(moves_list, move_count, from, one_step_sq);
                }
            }
            int start_rank_idx = (stm == WHITE) ? 1 : 6;
            if (rank == start_rank_idx) {
                int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                if (two_steps_sq >=0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq) && !captures_only) {
                    add_move_to_list(moves_list, move_count, from, two_steps_sq);
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
                add_move_to_list(moves_list, move_count, from, to, QUEEN); add_move_to_list(moves_list, move_count, from, to, ROOK);
                add_move_to_list(moves_list, move_count, from, to, BISHOP); add_move_to_list(moves_list, move_count, from, to, KNIGHT);
            } else {
                add_move_to_list(moves_list, move_count, from, to);
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
                add_move_to_list(moves_list, move_count, from, to);
            }
        }
    }

    if (!captures_only) {
        int king_sq = lsb_index(pos.piece_bb[KING] & my_pieces);
        if (king_sq != -1) {
            if (stm == WHITE) {
                if ((pos.castling_rights & 1) && king_sq == 4 &&
                    !get_bit(occupied, 5) && !get_bit(occupied, 6) &&
                    !is_square_attacked(pos, 4, BLACK) && !is_square_attacked(pos, 5, BLACK) && !is_square_attacked(pos, 6, BLACK)) {
                    add_move_to_list(moves_list, move_count, king_sq, 6);
                }
                if ((pos.castling_rights & 2) && king_sq == 4 &&
                    !get_bit(occupied, 3) && !get_bit(occupied, 2) && !get_bit(occupied, 1) &&
                    !is_square_attacked(pos, 4, BLACK) && !is_square_attacked(pos, 3, BLACK) && !is_square_attacked(pos, 2, BLACK)) {
                    add_move_to_list(moves_list, move_count, king_sq, 2);
                }
            } else { // BLACK
                if ((pos.castling_rights & 4) && king_sq == 60 &&
                    !get_bit(occupied, 61) && !get_bit(occupied, 62) &&
                    !is_square_attacked(pos, 60, WHITE) && !is_square_attacked(pos, 61, WHITE) && !is_square_attacked(pos, 62, WHITE)) {
                    add_move_to_list(moves_list, move_count, king_sq, 62);
                }
                if ((pos.castling_rights & 8) && king_sq == 60 &&
                    !get_bit(occupied, 59) && !get_bit(occupied, 58) && !get_bit(occupied, 57) &&
                    !is_square_attacked(pos, 60, WHITE) && !is_square_attacked(pos, 59, WHITE) && !is_square_attacked(pos, 58, WHITE)) {
                    add_move_to_list(moves_list, move_count, king_sq, 58);
                }
            }
        }
    }
    return move_count;
}

// --- Make Move --- (Zobrist parts are still here, will be removed if TT is fully removed)
// (Identical to original, but Zobrist will be removed with TT)
Position make_move(const Position& pos, const Move& move, bool& legal_move_flag) {
    legal_move_flag = false;
    Position next_pos = pos;
    int stm = pos.side_to_move;
    int opp = 1 - stm;

    uint64_t from_bb = set_bit(move.from);
    uint64_t to_bb = set_bit(move.to);
    Piece piece_moved = pos.piece_on_sq(move.from);
    Piece piece_captured = pos.piece_on_sq(move.to);

    if (piece_moved == NO_PIECE || pos.color_on_sq(move.from) != stm) {
        return pos;
    }
    if (piece_captured != NO_PIECE && pos.color_on_sq(move.to) == stm) { 
        return pos;
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

    uint8_t old_castling_rights = next_pos.castling_rights;
    if (piece_moved == KING) {
        if (stm == WHITE) next_pos.castling_rights &= ~0x3;
        else next_pos.castling_rights &= ~0xC;

        if (abs(move.to - move.from) == 2) {
            int rook_from_sq, rook_to_sq;
            if (move.to == 6) { rook_from_sq = 7; rook_to_sq = 5; }
            else if (move.to == 2) { rook_from_sq = 0; rook_to_sq = 3; }
            else if (move.to == 62) { rook_from_sq = 63; rook_to_sq = 61; }
            else { rook_from_sq = 56; rook_to_sq = 59; }

            next_pos.piece_bb[ROOK] &= ~set_bit(rook_from_sq);
            next_pos.piece_bb[ROOK] |= set_bit(rook_to_sq);
            next_pos.color_bb[stm] &= ~set_bit(rook_from_sq);
            next_pos.color_bb[stm] |= set_bit(rook_to_sq);
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_from_sq];
            next_pos.zobrist_hash ^= zobrist_pieces[stm][ROOK][rook_to_sq];
        }
    }
    if (move.from == 0 || move.to == 0 && piece_captured == ROOK && pos.color_on_sq(move.to) == WHITE) next_pos.castling_rights &= ~0x2;
    if (move.from == 7 || move.to == 7 && piece_captured == ROOK && pos.color_on_sq(move.to) == WHITE) next_pos.castling_rights &= ~0x1;
    if (move.from == 56 || move.to == 56 && piece_captured == ROOK && pos.color_on_sq(move.to) == BLACK) next_pos.castling_rights &= ~0x8;
    if (move.from == 63 || move.to == 63 && piece_captured == ROOK && pos.color_on_sq(move.to) == BLACK) next_pos.castling_rights &= ~0x4;

    if (old_castling_rights != next_pos.castling_rights) {
        next_pos.zobrist_hash ^= zobrist_castling[old_castling_rights];
        next_pos.zobrist_hash ^= zobrist_castling[next_pos.castling_rights];
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


// --- Evaluation (PSTs are large, potential target for removal/simplification later) ---
// (Identical to original)
const int piece_values_mg[6] = {100, 320, 330, 500, 900, 0};
const int piece_values_eg[6] = {120, 320, 330, 530, 950, 0};
const int pawn_pst[64] = {0,0,0,0,0,0,0,0,5,10,10,-20,-20,10,10,5,5,-5,-10,0,0,-10,-5,5,0,0,0,20,20,0,0,0,5,5,10,25,25,10,5,5,10,10,20,30,30,20,10,10,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,0};
const int knight_pst[64]={-50,-40,-30,-30,-30,-30,-40,-50,-40,-20,0,5,5,0,-20,-40,-30,5,10,15,15,10,5,-30,-30,0,15,20,20,15,0,-30,-30,5,15,20,20,15,5,-30,-30,0,10,15,15,10,0,-30,-40,-20,0,0,0,0,-20,-40,-50,-40,-30,-30,-30,-30,-40,-50};
const int bishop_pst[64]={-20,-10,-10,-10,-10,-10,-10,-20,-10,0,0,0,0,0,0,-10,-10,0,5,10,10,5,0,-10,-10,5,5,10,10,5,5,-10,-10,0,10,10,10,10,0,-10,-10,10,10,10,10,10,10,-10,-10,5,0,0,0,0,5,-10,-20,-10,-10,-10,-10,-10,-10,-20};
const int rook_pst[64]={0,0,0,5,5,0,0,0,-5,0,0,0,0,0,0,-5,-5,0,0,0,0,0,0,-5,-5,0,0,0,0,0,0,-5,-5,0,0,0,0,0,0,-5,-5,0,0,0,0,0,0,-5,5,10,10,10,10,10,10,5,0,0,0,0,0,0,0,0};
const int queen_pst[64]={-20,-10,-10,-5,-5,-10,-10,-20,-10,0,0,0,0,0,0,-10,-10,0,5,5,5,5,0,-10,-5,0,5,5,5,5,0,-5,0,0,5,5,5,5,0,-5,-10,0,5,5,5,5,0,-10,-10,0,0,0,0,0,0,-10,-20,-10,-10,-5,-5,-10,-10,-20};
const int king_pst_mg[64]={20,30,10,0,0,10,30,20,20,20,0,0,0,0,20,20,-10,-20,-20,-20,-20,-20,-20,-10,-20,-30,-30,-40,-40,-30,-30,-20,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30,-30,-40,-40,-50,-50,-40,-40,-30};
const int king_pst_eg[64]={-50,-30,-30,-30,-30,-30,-30,-50,-30,-10,20,30,30,20,-10,-30,-30,10,30,40,40,30,10,-30,-30,10,40,50,50,40,10,-30,-30,10,40,50,50,40,10,-30,-30,10,30,40,40,30,10,-30,-30,-10,20,30,30,20,-10,-30,-50,-30,-30,-30,-30,-30,-30,-50};
const int* pst_mg_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst_mg};
const int* pst_eg_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst_eg};
const int game_phase_inc[6] = {0, 1, 1, 2, 4, 0};
int evaluate(const Position& pos) {
    int mg_score = 0; int eg_score = 0; int game_phase = 0;
    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        int side_multiplier = (current_eval_color == WHITE) ? 1 : -1;
        for (int p_idx = PAWN; p_idx <= KING; ++p_idx) {
            uint64_t b = pos.piece_bb[p_idx] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p_idx];
            while (b) {
                int sq = lsb_index(b); b &= b - 1;
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);
                mg_score += side_multiplier * (piece_values_mg[p_idx] + pst_mg_all[p_idx][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p_idx] + pst_eg_all[p_idx][mirrored_sq]);
            }
        }
    }
    if (game_phase > 24) game_phase = 24; if (game_phase < 0) game_phase = 0;
    int final_score_from_white_pov = (mg_score * game_phase + eg_score * (24 - game_phase)) / 24;
    return (pos.side_to_move == WHITE) ? final_score_from_white_pov : -final_score_from_white_pov;
}

// --- Transposition Table (Using C-style array, will be simplified/removed later) ---
enum TTBound { TT_EXACT, TT_LOWER, TT_UPPER, TT_NONE };
struct TTEntry {
    uint64_t hash = 0; Move best_move = NULL_MOVE; int score = 0; int depth = 0; TTBound bound = TT_NONE;
};
constexpr size_t TT_MAX_ENTRIES = (TT_SIZE_MB_DEFAULT > 0) ? (TT_SIZE_MB_DEFAULT * 1024 * 1024 / sizeof(TTEntry)) : 0; // Placeholder
TTEntry transposition_table[TT_MAX_ENTRIES > 0 ? TT_MAX_ENTRIES : 1]; // Avoid zero-size array if TT_SIZE_MB_DEFAULT is 0
uint64_t tt_mask = (TT_MAX_ENTRIES > 0) ? TT_MAX_ENTRIES -1 : 0; // Ensure it's power of 2 if used
bool g_tt_is_initialized = false;
int g_configured_tt_size_mb = TT_SIZE_MB_DEFAULT;

void init_tt(size_t mb_size) { /* TT init logic simplified/deferred for 4k. For now, just clear. */
    if (TT_MAX_ENTRIES > 0) {
        std::memset(transposition_table, 0, sizeof(transposition_table));
        // Real init would calculate power_of_2_entries and set tt_mask
        // For now, if TT_SIZE_MB_DEFAULT is 0, tt_mask will be 0, disabling TT
        if (mb_size > 0) {
             size_t num_entries = (mb_size * 1024 * 1024) / sizeof(TTEntry);
             size_t power_of_2_entries = 1;
             while(power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries) power_of_2_entries *=2;
             if(power_of_2_entries > 0) tt_mask = power_of_2_entries -1; else tt_mask = 0;
             // Note: This doesn't resize the global array, which is fixed at compile time now.
             // This init_tt is mostly for setting the mask if a size > 0 was intended.
        } else {
            tt_mask = 0;
        }
    } else {
        tt_mask = 0;
    }
}
void clear_tt() { if (TT_MAX_ENTRIES > 0) std::memset(transposition_table, 0, sizeof(transposition_table)); }

bool probe_tt(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move_from_tt, int& score_from_tt) {
    if (tt_mask == 0 || !g_tt_is_initialized || TT_MAX_ENTRIES == 0) return false;
    TTEntry& entry = transposition_table[hash & tt_mask]; // tt_mask needs to be calculated correctly if TT is used
    // ... rest of probe_tt logic (identical)
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
    if (tt_mask == 0 || !g_tt_is_initialized || TT_MAX_ENTRIES == 0) return;
    TTEntry& entry = transposition_table[hash & tt_mask]; // tt_mask needs to be calculated
    // ... rest of store_tt logic (identical)
    if (score > MATE_THRESHOLD) score += ply; else if (score < -MATE_THRESHOLD) score -= ply;
    bool should_replace = (entry.hash == 0)||(entry.hash != hash)||(depth > entry.depth)||(depth == entry.depth && bound == TT_EXACT && entry.bound != TT_EXACT)||(depth == entry.depth && entry.bound == TT_NONE);
    if(should_replace){entry.hash=hash;entry.depth=depth;entry.score=score;entry.bound=bound;if(!best_move.is_null()||entry.hash!=hash||bound==TT_EXACT||bound==TT_LOWER){entry.best_move=best_move;}}
}

// --- Search ---
clock_t search_start_clock_time; // Replaces std::chrono::steady_clock::time_point
long long search_budget_ms = 0;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;

Move killer_moves[MAX_PLY][2];
int history_heuristic[2][64][64]; // This is huge (32KB), must remove for 4k
uint64_t game_history_hashes[MAX_GAME_HISTORY_PLY]; // Fixed-size array
int game_history_count = 0;

void reset_search_state() { nodes_searched = 0; stop_search_flag = false; }
void reset_killers_and_history() {
    for(int i=0; i<MAX_PLY; ++i) { killer_moves[i][0] = NULL_MOVE; killer_moves[i][1] = NULL_MOVE; }
    std::memset(history_heuristic, 0, sizeof(history_heuristic));
}

bool check_time() {
    if (stop_search_flag) return true;
    if ((nodes_searched & 2047) == 0) { // Check every 2048 nodes
        if (search_budget_ms > 0) {
            clock_t current_clock = clock();
            long long elapsed_ms = (current_clock - search_start_clock_time) * 1000LL / CLOCKS_PER_SEC;
            if (elapsed_ms >= search_budget_ms) {
                stop_search_flag = true;
                return true;
            }
        }
    }
    return false;
}

const int mvv_lva_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0};

// Basic insertion sort
void sort_moves_array(Move arr[], int n) {
    int i, j;
    Move key;
    for (i = 1; i < n; i++) {
        key = arr[i];
        j = i - 1;
        while (j >= 0 && arr[j].score < key.score) { // Sort descending
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key;
    }
}

void score_moves(const Position& pos, Move moves[], int num_moves, const Move& tt_move, int ply) {
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
                m.score = history_heuristic[pos.side_to_move][m.from][m.to]; // History heuristic is a target for removal
            }
        }
    }
    sort_moves_array(moves, num_moves);
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY -1) return evaluate(pos);

    int stand_pat = evaluate(pos);
    if (stand_pat >= beta) return beta;
    if (alpha < stand_pat) alpha = stand_pat;

    Move captures[MAX_MOVES_PER_PLY];
    int num_captures = generate_moves(pos, captures, true);

    Move dummy_tt_move = NULL_MOVE;
    score_moves(pos, captures, num_captures, dummy_tt_move, ply);

    for (int i=0; i < num_captures; ++i) {
        const Move& cap_move = captures[i];
        bool legal;
        Position next_pos = make_move(pos, cap_move, legal);
        if (!legal) continue;

        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);
        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }
    return alpha;
}

// current_search_path_hashes changed to C-array
uint64_t current_search_path_hashes_arr[MAX_PLY];

int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move) {
    nodes_searched++;
    if (check_time()) return 0;
    if (ply >= MAX_PLY -1) return evaluate(pos);

    // Repetition check with current path
    for (int i = 0; i < ply; ++i) { // Only check up to current ply
        if (current_search_path_hashes_arr[i] == pos.zobrist_hash) return 0;
    }
    // Game history repetition check
    int game_reps = 0;
    for(int i=0; i < game_history_count; ++i) if(game_history_hashes[i] == pos.zobrist_hash) game_reps++;
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
            null_next_pos.zobrist_hash = pos.zobrist_hash;
            null_next_pos.zobrist_hash ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
            null_next_pos.ep_square = -1;
            null_next_pos.zobrist_hash ^= zobrist_ep[64];
            null_next_pos.zobrist_hash ^= zobrist_side_to_move;
            null_next_pos.ply = pos.ply + 1;
            int R = (depth > 6) ? 3 : 2;
            
            current_search_path_hashes_arr[ply] = pos.zobrist_hash; // Add to path
            int null_score = -search(null_next_pos, depth - 1 - R, -beta, -beta + 1, ply + 1, false, false);
            // No explicit pop needed as it's overwritten or not used beyond ply

            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 return beta;
            }
    }

    Move moves[MAX_MOVES_PER_PLY];
    int num_moves = generate_moves(pos, moves);
    score_moves(pos, moves, num_moves, tt_move, ply);

    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;

    current_search_path_hashes_arr[ply] = pos.zobrist_hash; // Add to path

    for (int i = 0; i < num_moves; ++i) {
        const Move& current_move = moves[i];
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        legal_moves_played++;
        int score;

        if (legal_moves_played == 1) {
            score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true);
        } else {
            int r = 0; 
            if (depth >= 3 && i >= (is_pv_node ? 3 : 2) &&
                !in_check && current_move.promotion == NO_PIECE && pos.piece_on_sq(current_move.to) == NO_PIECE &&
                current_move.score < 700000) { 
                r = 1;
                if (depth >= 5 && i >= (is_pv_node ? 5 : 4)) r = (depth > 7 ? 2 : 1);
                // r = std::min(r, depth - 2); // std::min from <algorithm>
                if (r > depth - 2) r = depth - 2;
                if (r < 0) r = 0;
            }
            score = -search(next_pos, depth - 1 - r, -alpha - 1, -alpha, ply + 1, false, true);
            if (r > 0 && score > alpha) {
                 score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true);
            }
            if (score > alpha && score < beta) {
                 score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true);
            }
        }

        if (stop_search_flag) { /* No pop needed for C-array */ return 0; }

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
                    /* No pop needed for C-array */
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found);
                    return beta;
                }
            }
        }
    }
    /* No pop needed for C-array */

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

// Helper to skip whitespace
const char* skip_whitespace(const char* str) {
    while (*str && isspace((unsigned char)*str)) {
        str++;
    }
    return str;
}

// Helper to get next token
const char* get_token(const char* str, char* buffer, size_t buffer_len) {
    str = skip_whitespace(str);
    size_t i = 0;
    while (*str && !isspace((unsigned char)*str) && i < buffer_len - 1) {
        buffer[i++] = *str++;
    }
    buffer[i] = '\0';
    return str;
}


void parse_fen(Position& pos, const char* fen_str_c) {
    pos = Position(); // Reset
    char part[256];
    const char* p = fen_str_c;

    p = get_token(p, part, sizeof(part));
    int rank = 7, file = 0;
    for (int i = 0; part[i]; ++i) {
        char c = part[i];
        if (isdigit(c)) {
            file += (c - '0');
        } else if (c == '/') {
            rank--; file = 0;
        } else {
            Piece p_type = NO_PIECE; Color p_color = NO_COLOR;
            if (islower(c)) p_color = BLACK; else p_color = WHITE;
            char lower_c = tolower(c);
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

    p = get_token(p, part, sizeof(part)); pos.side_to_move = (part[0] == 'w') ? WHITE : BLACK;

    p = get_token(p, part, sizeof(part));
    pos.castling_rights = 0;
    for (int i=0; part[i]; ++i) {
        char c = part[i];
        if (c == 'K') pos.castling_rights |= 1; else if (c == 'Q') pos.castling_rights |= 2;
        else if (c == 'k') pos.castling_rights |= 4; else if (c == 'q') pos.castling_rights |= 8;
    }

    p = get_token(p, part, sizeof(part));
    if (part[0] != '-') {
        if (strlen(part) == 2 && part[0] >= 'a' && part[0] <= 'h' && part[1] >= '1' && part[1] <= '8') {
            int ep_file = part[0] - 'a';
            int ep_rank = part[1] - '1';
            pos.ep_square = ep_rank * 8 + ep_file;
        } else {
            pos.ep_square = -1;
        }
    } else {
        pos.ep_square = -1;
    }

    p = get_token(p, part, sizeof(part)); if (part[0]) pos.halfmove_clock = atoi(part); else pos.halfmove_clock = 0;
    p = get_token(p, part, sizeof(part)); if (part[0]) pos.fullmove_number = atoi(part); else pos.fullmove_number = 1;
    if (pos.fullmove_number < 1) pos.fullmove_number = 1;


    pos.ply = 0;
    pos.zobrist_hash = calculate_zobrist_hash(pos);
    game_history_count = 0; // Reset history
}

Move parse_uci_move_from_string(const Position& current_pos, const char* uci_move_str) {
    Move m = NULL_MOVE;
    if (strlen(uci_move_str) < 4) return m;

    m.from = (uci_move_str[0] - 'a') + (uci_move_str[1] - '1') * 8;
    m.to = (uci_move_str[2] - 'a') + (uci_move_str[3] - '1') * 8;

    if (m.from < 0 || m.from > 63 || m.to < 0 || m.to > 63) return NULL_MOVE;

    if (strlen(uci_move_str) == 5) {
        char promo_char = uci_move_str[4];
        if (promo_char == 'q') m.promotion = QUEEN;
        else if (promo_char == 'n') m.promotion = KNIGHT;
        else if (promo_char == 'b') m.promotion = BISHOP;
        else if (promo_char == 'r') m.promotion = ROOK;
    }
    return m;
}

uint64_t calculate_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c) {
        for (int p_idx = PAWN; p_idx <= KING; ++p_idx) {
            uint64_t b = pos.piece_bb[p_idx] & pos.color_bb[c];
            while (b) {
                int sq = lsb_index(b); b &= b - 1;
                h ^= zobrist_pieces[c][p_idx][sq];
            }
        }
    }
    h ^= zobrist_castling[pos.castling_rights];
    h ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    if (pos.side_to_move == BLACK) h ^= zobrist_side_to_move;
    return h;
}

void uci_loop() {
    char line_buffer[1024];
    char token[256];
    char uci_move_buffer[10]; // For move_to_uci

    parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    while (fgets(line_buffer, sizeof(line_buffer), stdin) != NULL) {
        const char* p_line = line_buffer;
        p_line = get_token(p_line, token, sizeof(token));

        if (strcmp(token, "uci") == 0) {
            printf("id name AmiraC\n"); // AmiraC for C-style
            printf("id author ChessTubeTree\n");
            printf("option name Hash type spin default %d min 0 max 1024\n", TT_SIZE_MB_DEFAULT);
            printf("uciok\n"); fflush(stdout);
        } else if (strcmp(token, "isready") == 0) {
            if (!g_tt_is_initialized && TT_MAX_ENTRIES > 0) { // Only init if TT is enabled
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            printf("readyok\n"); fflush(stdout);
        } else if (strcmp(token, "setoption") == 0) {
            char name_token_buf[64], value_token_buf[64], name_str_buf[64], value_str_val_buf[64];
            p_line = get_token(p_line, name_token_buf, sizeof(name_token_buf)); // "name"
            if (strcmp(name_token_buf, "name") == 0) {
                p_line = get_token(p_line, name_str_buf, sizeof(name_str_buf)); // Option name e.g. "Hash"
                p_line = get_token(p_line, value_token_buf, sizeof(value_token_buf)); // "value"
                p_line = get_token(p_line, value_str_val_buf, sizeof(value_str_val_buf)); // Option value

                if (strcmp(name_str_buf, "Hash") == 0 && TT_MAX_ENTRIES > 0) {
                    int parsed_size = atoi(value_str_val_buf);
                    g_configured_tt_size_mb = parsed_size;
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true;
                }
            }
        } else if (strcmp(token, "ucinewgame") == 0) {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (TT_MAX_ENTRIES > 0) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            reset_killers_and_history();
            game_history_count = 0;
        } else if (strcmp(token, "position") == 0) {
            char fen_str_collector[512] = "";
            bool fen_mode = false;
            p_line = get_token(p_line, token, sizeof(token)); 
            
            if (strcmp(token, "startpos") == 0) {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                p_line = get_token(p_line, token, sizeof(token)); // Check for "moves"
            } else if (strcmp(token, "fen") == 0) {
                fen_mode = true;
                // Collect FEN parts
                char temp_fen_part[128];
                int fen_parts_count = 0;
                while(fen_parts_count < 6) {
                    const char* next_p = get_token(p_line, temp_fen_part, sizeof(temp_fen_part));
                    if (temp_fen_part[0] == '\0' || strcmp(temp_fen_part, "moves") == 0) {
                        if(strcmp(temp_fen_part, "moves") == 0) strcpy(token, "moves"); else token[0] = '\0';
                        p_line = next_p; // Consume "moves" if present
                        break;
                    }
                    strcat(fen_str_collector, temp_fen_part);
                    strcat(fen_str_collector, " ");
                    p_line = next_p;
                    fen_parts_count++;
                }
                if (strlen(fen_str_collector) > 0) fen_str_collector[strlen(fen_str_collector)-1] = '\0'; // Remove trailing space
                parse_fen(uci_root_pos, fen_str_collector);
            }
            
            if (game_history_count < MAX_GAME_HISTORY_PLY) {
                 game_history_hashes[game_history_count++] = uci_root_pos.zobrist_hash;
            }


            if (strcmp(token, "moves") == 0) {
                char move_str_uci[10];
                while(true){
                    p_line = get_token(p_line, move_str_uci, sizeof(move_str_uci));
                    if(move_str_uci[0] == '\0') break;

                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && strcmp(move_str_uci,"0000") != 0) break; // Invalid move string
                    if (m.is_null() && strcmp(move_str_uci,"0000") == 0 && strlen(move_str_uci) > 0) { /* handle null move if needed */ }
                     else if (m.is_null() && strlen(move_str_uci) == 0) break; // No more moves

                    bool legal;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                    if (game_history_count < MAX_GAME_HISTORY_PLY) {
                         game_history_hashes[game_history_count++] = uci_root_pos.zobrist_hash;
                    }
                }
            }
        } else if (strcmp(token, "go") == 0) {
            if (!g_tt_is_initialized && TT_MAX_ENTRIES > 0) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }

            int wtime = -1, btime = -1, winc = 0, binc = 0, movestogo = 0;
            long long fixed_time_per_move = -1;
            char go_param[64];

            while(true){
                p_line = get_token(p_line, go_param, sizeof(go_param));
                if(go_param[0] == '\0') break;

                if (strcmp(go_param, "wtime")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); wtime = atoi(go_param); }
                else if (strcmp(go_param, "btime")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); btime = atoi(go_param); }
                else if (strcmp(go_param, "winc")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); winc = atoi(go_param); }
                else if (strcmp(go_param, "binc")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); binc = atoi(go_param); }
                else if (strcmp(go_param, "movestogo")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); movestogo = atoi(go_param); }
                else if (strcmp(go_param, "movetime")==0) { p_line = get_token(p_line, go_param, sizeof(go_param)); fixed_time_per_move = atoll(go_param); } // Use atoll for long long
            }
            
            if (fixed_time_per_move != -1) {
                search_budget_ms = fixed_time_per_move - 50; // Small buffer
            } else {
                int my_time = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;
                int my_inc = (uci_root_pos.side_to_move == WHITE) ? winc : binc;
                if (my_time != -1) {
                    long long base_time_slice;
                    if (movestogo > 0 && movestogo < 40) base_time_slice = my_time / movestogo;
                    else base_time_slice = my_time / 25;
                    search_budget_ms = base_time_slice + my_inc - 50;
                    if (search_budget_ms > my_time * 0.8 && my_time > 100) search_budget_ms = (long long)(my_time * 0.8);
                } else { search_budget_ms = 2000; } // Default time
            }
            if (search_budget_ms <= 0) search_budget_ms = 50;

            search_start_clock_time = clock();
            reset_search_state();
            uci_best_move_overall = NULL_MOVE;
            int best_score_overall = 0;
            
            for (int depth = 1; depth <= MAX_PLY; ++depth) {
                int current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true);
                if (stop_search_flag && depth > 1) break;

                Move tt_root_move = NULL_MOVE; int tt_root_score;
                int dummy_alpha = -INF_SCORE, dummy_beta = INF_SCORE; // prevent modification by probe
                if (TT_MAX_ENTRIES > 0 && probe_tt(uci_root_pos.zobrist_hash, depth, 0, dummy_alpha, dummy_beta, tt_root_move, tt_root_score)) {
                     if (!tt_root_move.is_null()) uci_best_move_overall = tt_root_move;
                     best_score_overall = current_score; // Use score from search, not TT for root
                } else {
                     best_score_overall = current_score;
                     if (TT_MAX_ENTRIES > 0 && tt_mask > 0) { // Only access TT if it's valid
                        TTEntry& root_entry_check = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                        if (root_entry_check.hash == uci_root_pos.zobrist_hash && !root_entry_check.best_move.is_null()) {
                             uci_best_move_overall = root_entry_check.best_move;
                        }
                     }
                }

                clock_t now_clock = clock();
                long long elapsed_ms = (now_clock - search_start_clock_time) * 1000LL / CLOCKS_PER_SEC;
                if (elapsed_ms < 0) elapsed_ms = 0;

                printf("info depth %d score cp %d", depth, best_score_overall);
                if (best_score_overall > MATE_THRESHOLD) printf(" mate %d", (MATE_SCORE - best_score_overall + 1)/2);
                else if (best_score_overall < -MATE_THRESHOLD) printf(" mate %d", -(MATE_SCORE + best_score_overall)/2);
                printf(" nodes %llu time %lld", nodes_searched, elapsed_ms);
                if (elapsed_ms > 0 && nodes_searched > 0) printf(" nps %llu", (nodes_searched * 1000 / elapsed_ms));

                if (!uci_best_move_overall.is_null()) {
                     move_to_uci(uci_best_move_overall, uci_move_buffer);
                     printf(" pv %s", uci_move_buffer);
                }
                printf("\n"); fflush(stdout);

                if (abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break;
                if (search_budget_ms > 0 && elapsed_ms > 0 && depth > 1) {
                    if (elapsed_ms * 2.5 > search_budget_ms && depth > 3) break;
                    else if (elapsed_ms * 1.8 > search_budget_ms ) break;
                }
            }

            if (!uci_best_move_overall.is_null()) {
                 move_to_uci(uci_best_move_overall, uci_move_buffer);
                 printf("bestmove %s\n", uci_move_buffer);
            } else {
                Move legal_moves_fallback[MAX_MOVES_PER_PLY];
                int num_legal_fallback = generate_moves(uci_root_pos, legal_moves_fallback);
                bool found_one_legal_fallback = false;
                for(int i=0; i < num_legal_fallback; ++i) {
                    const Move& m_fall = legal_moves_fallback[i];
                    bool is_leg_fall;
                    Position temp_pos = make_move(uci_root_pos, m_fall, is_leg_fall); // Check legality
                    if(is_leg_fall) {
                        move_to_uci(m_fall, uci_move_buffer);
                        printf("bestmove %s\n", uci_move_buffer);
                        found_one_legal_fallback = true;
                        break;
                    }
                }
                if (!found_one_legal_fallback && num_legal_fallback > 0){
                     move_to_uci(legal_moves_fallback[0], uci_move_buffer); // Send first pseudolegal if none legal (should not happen ideally)
                     printf("bestmove %s\n", uci_move_buffer);
                } else if (!found_one_legal_fallback && num_legal_fallback == 0){
                     printf("bestmove 0000\n");
                }
            }
            fflush(stdout);

        } else if (strcmp(token, "quit") == 0 || strcmp(token, "stop") == 0) {
            stop_search_flag = true;
            if (strcmp(token, "quit") == 0) break;
        }
    }
}

int main(int argc, char* argv[]) {
    // std::ios_base::sync_with_stdio(false); // Not needed with printf/fgets
    // std::cin.tie(NULL);                   // Not needed with printf/fgets

    init_zobrist();
    init_attack_tables();
    if (TT_MAX_ENTRIES > 0) { // Only initialize if TT is conceptually enabled
        init_tt(g_configured_tt_size_mb);
        g_tt_is_initialized = true;
    }
    reset_killers_and_history();

    uci_loop();
    return 0;
}
