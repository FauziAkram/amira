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
#include <cmath>    // For std::log
#include <cassert>  // For assert()
#include <cstdio>   // For printf and fflush

// Bit manipulation builtins (MSVC/GCC specific)
#if defined(_MSC_VER)
#include <intrin.h>
#endif

// --- Constants ---
enum Piece { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE };
enum Color { WHITE, BLACK, NO_COLOR };

constexpr int MAX_PLY = 128;
constexpr int TT_SIZE_MB_DEFAULT = 256;
constexpr int PAWN_CACHE_SIZE_ENTRIES = 131072; // 2^17 entries
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

// Forward Declarations
struct Move;
struct Position;
int evaluate(Position& pos);
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
int generate_moves(const Position& pos, Move* moves_list, bool captures_only);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);
uint64_t calculate_pawn_zobrist_hash(const Position& pos);

// --- Pawn Cache ---
struct PawnCacheEntry {
    uint64_t key = 0;
    int mg_score = 0;
    int eg_score = 0;
    uint64_t white_passed_pawns = 0;
    uint64_t black_passed_pawns = 0;
};

std::vector<PawnCacheEntry> pawn_evaluation_cache;
uint64_t pawn_cache_mask = 0;

void init_pawn_cache() {
    pawn_evaluation_cache.assign(PAWN_CACHE_SIZE_ENTRIES, PawnCacheEntry());
    pawn_cache_mask = PAWN_CACHE_SIZE_ENTRIES - 1;
}

void clear_pawn_cache() {
     if (!pawn_evaluation_cache.empty()) {
        std::memset(pawn_evaluation_cache.data(), 0, pawn_evaluation_cache.size() * sizeof(PawnCacheEntry));
    }
}

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

// --- START OF BULLETPROOF BITWISE OPERATIONS ---
inline uint64_t set_bit(int sq) { return 1ULL << sq; }
inline bool get_bit(uint64_t bb, int sq) { return (bb >> sq) & 1; }

// Safe, fast, universally compatible pop_count using a 64-bit SWAR algorithm
inline int pop_count(uint64_t bb) {
    bb -= (bb >> 1) & 0x5555555555555555ULL;
    bb = (bb & 0x3333333333333333ULL) + ((bb >> 2) & 0x3333333333333333ULL);
    bb = (bb + (bb >> 4)) & 0x0F0F0F0F0F0F0F0FULL;
    return static_cast<int>((bb * 0x0101010101010101ULL) >> 56);
}

// Safe, universally compatible lsb_index using De Bruijn bitscan
const int index64[64] = {
    0, 47,  1, 56, 48, 27,  2, 60,
   57, 49, 41, 37, 28, 16,  3, 61,
   54, 58, 35, 52, 50, 42, 21, 44,
   38, 32, 29, 24, 17, 12,  4, 62,
   46, 55, 26, 59, 40, 36, 15, 53,
   34, 51, 20, 43, 31, 23, 11, 45,
   25, 39, 14, 33, 19, 30, 10, 22,
   13, 18,  9,  8,  7,  6,  5, 63
};
inline int lsb_index(uint64_t bb) {
    assert(bb != 0);
    const uint64_t debruijn64 = 0x03f79d71b4cb0a89ULL;
    return index64[((bb & -bb) * debruijn64) >> 58];
}

// CRITICAL NEW FUNCTION: Atomically finds and clears the LSB
inline int pop_lsb(uint64_t& bb) {
    assert(bb != 0);
    int index = lsb_index(bb);
    bb &= bb - 1; // Clear the least significant bit
    return index;
}
// --- END OF BULLETPROOF BITWISE OPERATIONS ---


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
    uint64_t pawn_zobrist_key; // For pawn cache
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
            ((b >> 10) & ~0xC0C0C0C0C0C0C0C0ULL) | ((b >> 6)  & ~0x03030303030303ULL)
        );

        king_attacks_bb[sq] = north(b) | south(b) | east(b) | west(b) |
                              ne(b) | nw(b) | se(b) | sw(b);
    }
}

// --- Magic Bitboard Slider Attack Generation ---
const uint64_t magic_rook_mask[64] = {
  0x000101010101017Eull, 0x000202020202027Cull, 0x000404040404047Aull, 0x0008080808080876ull, 0x001010101010106Eull, 0x002020202020205Eull, 0x004040404040403Eull, 0x008080808080807Eull,
  0x0001010101017E00ull, 0x0002020202027C00ull, 0x0004040404047A00ull, 0x0008080808087600ull, 0x0010101010106E00ull, 0x0020202020205E00ull, 0x0040404040403E00ull, 0x0080808080807E00ull,
  0x00010101017E0100ull, 0x00020202027C0200ull, 0x00040404047A0400ull, 0x0008080808760800ull, 0x00101010106E1000ull, 0x00202020205E2000ull, 0x00404040403E4000ull, 0x00808080807E8000ull,
  0x000101017E010100ull, 0x000202027C020200ull, 0x000404047A040400ull, 0x0008080876080800ull, 0x001010106E101000ull, 0x002020205E202000ull, 0x004040403E404000ull, 0x008080807E808000ull,
  0x0001017E01010100ull, 0x0002027C02020200ull, 0x0004047A04040400ull, 0x0008087608080800ull, 0x0010106E10101000ull, 0x0020205E20202000ull, 0x0040403E40404000ull, 0x0080807E80808000ull,
  0x00017E0101010100ull, 0x00027C0202020200ull, 0x00047A0404040400ull, 0x0008760808080800ull, 0x00106E1010101000ull, 0x00205E2020202000ull, 0x00403E4040404000ull, 0x00807E8080808000ull,
  0x007E010101010100ull, 0x007C020202020200ull, 0x007A040404040400ull, 0x0076080808080800ull, 0x006E101010101000ull, 0x005E202020202000ull, 0x003E404040404000ull, 0x007E808080808000ull,
  0x7E01010101010100ull, 0x7C02020202020200ull, 0x7A04040404040400ull, 0x7608080808080800ull, 0x6E10101010101000ull, 0x5E20202020202000ull, 0x3E40404040404000ull, 0x7E80808080808000ull
};
const uint64_t magic_rook[64] = {
  0x0080001020400080ull, 0x0040001000200040ull, 0x0080081000200080ull, 0x0080040800100080ull, 0x0080020400080080ull, 0x0080010200040080ull, 0x0080008001000200ull, 0x0080002040800100ull,
  0x0000800020400080ull, 0x0000400020005000ull, 0x0000801000200080ull, 0x0000800800100080ull, 0x0000800400080080ull, 0x0000800200040080ull, 0x0000800100020080ull, 0x0000800040800100ull,
  0x0000208000400080ull, 0x0000404000201000ull, 0x0000808010002000ull, 0x0000808008001000ull, 0x0000808004000800ull, 0x0000808002000400ull, 0x0000010100020004ull, 0x0000020000408104ull,
  0x0000208080004000ull, 0x0000200040005000ull, 0x0000100080200080ull, 0x0000080080100080ull, 0x0000040080080080ull, 0x0000020080040080ull, 0x0000010080800200ull, 0x0000800080004100ull,
  0x0000204000800080ull, 0x0000200040401000ull, 0x0000100080802000ull, 0x0000080080801000ull, 0x0000040080800800ull, 0x0000020080800400ull, 0x0000020001010004ull, 0x0000800040800100ull,
  0x0000204000808000ull, 0x0000200040008080ull, 0x0000100020008080ull, 0x0000080010008080ull, 0x0000040008008080ull, 0x0000020004008080ull, 0x0000010002008080ull, 0x0000004081020004ull,
  0x0000204000800080ull, 0x0000200040008080ull, 0x0000100020008080ull, 0x0000080010008080ull, 0x0000040008008080ull, 0x0000020004008080ull, 0x0000800100020080ull, 0x0000800041000080ull,
  0x00FFFCDDFCED714Aull, 0x007FFCDDFCED714Aull, 0x003FFFCDFFD88096ull, 0x0000040810002101ull, 0x0001000204080011ull, 0x0001000204000801ull, 0x0001000082000401ull, 0x0001FFFAABFAD1A2ull
};
const uint32_t magic_rook_shift[64] = { 52, 53, 53, 53, 53, 53, 53, 52, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 53, 53, 53, 53, 53 };

const uint64_t magic_bishop_mask[64] = {
  0x0040201008040200ull, 0x0000402010080400ull, 0x0000004020100A00ull, 0x0000000040221400ull, 0x0000000002442800ull, 0x0000000204085000ull, 0x0000020408102000ull, 0x0002040810204000ull,
  0x0020100804020000ull, 0x0040201008040000ull, 0x00004020100A0000ull, 0x0000004022140000ull, 0x0000000244280000ull, 0x0000020408500000ull, 0x0002040810200000ull, 0x0004081020400000ull,
  0x0010080402000200ull, 0x0020100804000400ull, 0x004020100A000A00ull, 0x0000402214001400ull, 0x0000024428002800ull, 0x0002040850005000ull, 0x0004081020002000ull, 0x0008102040004000ull,
  0x0008040200020400ull, 0x0010080400040800ull, 0x0020100A000A1000ull, 0x0040221400142200ull, 0x0002442800284400ull, 0x0004085000500800ull, 0x0008102000201000ull, 0x0010204000402000ull,
  0x0004020002040800ull, 0x0008040004081000ull, 0x00100A000A102000ull, 0x0022140014224000ull, 0x0044280028440200ull, 0x0008500050080400ull, 0x0010200020100800ull, 0x0020400040201000ull,
  0x0002000204081000ull, 0x0004000408102000ull, 0x000A000A10204000ull, 0x0014001422400000ull, 0x0028002844020000ull, 0x0050005008040200ull, 0x0020002010080400ull, 0x0040004020100800ull,
  0x0000020408102000ull, 0x0000040810204000ull, 0x00000A1020400000ull, 0x0000142240000000ull, 0x0000284402000000ull, 0x0000500804020000ull, 0x0000201008040200ull, 0x0000402010080400ull,
  0x0002040810204000ull, 0x0004081020400000ull, 0x000A102040000000ull, 0x0014224000000000ull, 0x0028440200000000ull, 0x0050080402000000ull, 0x0020100804020000ull, 0x0040201008040200ull
};
const uint64_t magic_bishop[64] = {
  0x0002020202020200ull, 0x0002020202020000ull, 0x0004010202000000ull, 0x0004040080000000ull, 0x0001104000000000ull, 0x0000821040000000ull, 0x0000410410400000ull, 0x0000104104104000ull,
  0x0000040404040400ull, 0x0000020202020200ull, 0x0000040102020000ull, 0x0000040400800000ull, 0x0000011040000000ull, 0x0000008210400000ull, 0x0000004104104000ull, 0x0000002082082000ull,
  0x0004000808080800ull, 0x0002000404040400ull, 0x0001000202020200ull, 0x0000800802004000ull, 0x0000800400A00000ull, 0x0000200100884000ull, 0x0000400082082000ull, 0x0000200041041000ull,
  0x0002080010101000ull, 0x0001040008080800ull, 0x0000208004010400ull, 0x0000404004010200ull, 0x0000840000802000ull, 0x0000404002011000ull, 0x0000808001041000ull, 0x0000404000820800ull,
  0x0001041000202000ull, 0x0000820800101000ull, 0x0000104400080800ull, 0x0000020080080080ull, 0x0000404040040100ull, 0x0000808100020100ull, 0x0001010100020800ull, 0x0000808080010400ull,
  0x0000820820004000ull, 0x0000410410002000ull, 0x0000082088001000ull, 0x0000002011000800ull, 0x0000080100400400ull, 0x0001010101000200ull, 0x0002020202000400ull, 0x0001010101000200ull,
  0x0000410410400000ull, 0x0000208208200000ull, 0x0000002084100000ull, 0x0000000020880000ull, 0x0000001002020000ull, 0x0000040408020000ull, 0x0004040404040000ull, 0x0002020202020000ull,
  0x0000104104104000ull, 0x0000002082082000ull, 0x0000000020841000ull, 0x0000000000208800ull, 0x0000000010020200ull, 0x0000000404080200ull, 0x0000040404040400ull, 0x0002020202020200ull
};
const uint32_t magic_bishop_shift[64] = { 58, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 58 };

uint64_t magic_rook_table[102400];
uint64_t* magic_rook_indices[64] = {
  magic_rook_table + 86016, magic_rook_table + 73728, magic_rook_table + 36864, magic_rook_table + 43008, magic_rook_table + 47104, magic_rook_table + 51200, magic_rook_table + 77824, magic_rook_table + 94208,
  magic_rook_table + 69632, magic_rook_table + 32768, magic_rook_table + 38912, magic_rook_table + 10240, magic_rook_table + 14336, magic_rook_table + 53248, magic_rook_table + 57344, magic_rook_table + 81920,
  magic_rook_table + 24576, magic_rook_table + 33792, magic_rook_table + 6144,  magic_rook_table + 11264, magic_rook_table + 15360, magic_rook_table + 18432, magic_rook_table + 58368, magic_rook_table + 61440,
  magic_rook_table + 26624, magic_rook_table + 4096,  magic_rook_table + 7168,  magic_rook_table + 0,     magic_rook_table + 2048,  magic_rook_table + 19456, magic_rook_table + 22528, magic_rook_table + 63488,
  magic_rook_table + 28672, magic_rook_table + 5120,  magic_rook_table + 8192,  magic_rook_table + 1024,  magic_rook_table + 3072,  magic_rook_table + 20480, magic_rook_table + 23552, magic_rook_table + 65536,
  magic_rook_table + 30720, magic_rook_table + 34816, magic_rook_table + 9216,  magic_rook_table + 12288, magic_rook_table + 16384, magic_rook_table + 21504, magic_rook_table + 59392, magic_rook_table + 67584,
  magic_rook_table + 71680, magic_rook_table + 35840, magic_rook_table + 39936, magic_rook_table + 13312, magic_rook_table + 17408, magic_rook_table + 54272, magic_rook_table + 60416, magic_rook_table + 83968,
  magic_rook_table + 90112, magic_rook_table + 75776, magic_rook_table + 40960, magic_rook_table + 45056, magic_rook_table + 49152, magic_rook_table + 55296, magic_rook_table + 79872, magic_rook_table + 98304
};
uint64_t magic_bishop_table[5248];
uint64_t* magic_bishop_indices[64] = {
  magic_bishop_table + 4992, magic_bishop_table + 2624, magic_bishop_table + 256,  magic_bishop_table + 896,  magic_bishop_table + 1280, magic_bishop_table + 1664, magic_bishop_table + 4800, magic_bishop_table + 5120,
  magic_bishop_table + 2560, magic_bishop_table + 2656, magic_bishop_table + 288,  magic_bishop_table + 928,  magic_bishop_table + 1312, magic_bishop_table + 1696, magic_bishop_table + 4832, magic_bishop_table + 4928,
  magic_bishop_table + 0,    magic_bishop_table + 128,  magic_bishop_table + 320,  magic_bishop_table + 960,  magic_bishop_table + 1344, magic_bishop_table + 1728, magic_bishop_table + 2304, magic_bishop_table + 2432,
  magic_bishop_table + 32,   magic_bishop_table + 160,  magic_bishop_table + 448,  magic_bishop_table + 2752, magic_bishop_table + 3776, magic_bishop_table + 1856, magic_bishop_table + 2336, magic_bishop_table + 2464,
  magic_bishop_table + 64,   magic_bishop_table + 192,  magic_bishop_table + 576,  magic_bishop_table + 3264, magic_bishop_table + 4288, magic_bishop_table + 1984, magic_bishop_table + 2368, magic_bishop_table + 2496,
  magic_bishop_table + 96,   magic_bishop_table + 224,  magic_bishop_table + 704,  magic_bishop_table + 1088, magic_bishop_table + 1472, magic_bishop_table + 2112, magic_bishop_table + 2400, magic_bishop_table + 2528,
  magic_bishop_table + 2592, magic_bishop_table + 2688, magic_bishop_table + 832,  magic_bishop_table + 1216, magic_bishop_table + 1600, magic_bishop_table + 2240, magic_bishop_table + 4864, magic_bishop_table + 4960,
  magic_bishop_table + 5056, magic_bishop_table + 2720, magic_bishop_table + 864,  magic_bishop_table + 1248, magic_bishop_table + 1632, magic_bishop_table + 2272, magic_bishop_table + 4896, magic_bishop_table + 5184
};

uint64_t reference_rook_attacks(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {1, -1, 8, -8};
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (std::abs(d) == 1 && r_curr != r_prev) break;
            if (std::abs(d) == 8 && c_curr != c_prev) break;
            attacks |= set_bit(s);
            if (get_bit(occupied, s)) break;
        }
    }
    return attacks;
}

uint64_t reference_bishop_attacks(int sq, uint64_t occupied) {
    uint64_t attacks = 0;
    const int deltas[] = {9, -9, 7, -7};
    for (int d : deltas) {
        for (int s = sq + d; ; s += d) {
            if (s < 0 || s >= 64) break;
            int r_curr = s / 8, c_curr = s % 8;
            int r_prev = (s-d) / 8, c_prev = (s-d) % 8;
            if (std::abs(r_curr - r_prev) != 1 || std::abs(c_curr - c_prev) != 1) break;
            attacks |= set_bit(s);
            if (get_bit(occupied, s)) break;
        }
    }
    return attacks;
}

void init_magic_bitboards() {
    for (int sq = 0; sq < 64; ++sq) {
        uint64_t mask = magic_rook_mask[sq];
        int num_mask_bits = pop_count(mask);
        for (int i = 0; i < (1 << num_mask_bits); ++i) {
            uint64_t blockers = 0;
            uint64_t temp_mask = mask;
            for (int j = 0; j < num_mask_bits; ++j) {
                int bit_pos = pop_lsb(temp_mask);
                if ((i >> j) & 1) {
                    blockers |= set_bit(bit_pos);
                }
            }
            uint64_t index = (blockers * magic_rook[sq]) >> magic_rook_shift[sq];
            magic_rook_indices[sq][index] = reference_rook_attacks(sq, blockers);
        }

        mask = magic_bishop_mask[sq];
        num_mask_bits = pop_count(mask);
        for (int i = 0; i < (1 << num_mask_bits); ++i) {
            uint64_t blockers = 0;
            uint64_t temp_mask = mask;
            for (int j = 0; j < num_mask_bits; ++j) {
                int bit_pos = pop_lsb(temp_mask);
                if ((i >> j) & 1) {
                    blockers |= set_bit(bit_pos);
                }
            }
            uint64_t index = (blockers * magic_bishop[sq]) >> magic_bishop_shift[sq];
            magic_bishop_indices[sq][index] = reference_bishop_attacks(sq, blockers);
        }
    }
}


inline uint64_t get_rook_attacks(int sq, uint64_t occupied) {
    uint64_t blockers = occupied & magic_rook_mask[sq];
    uint64_t index = (blockers * magic_rook[sq]) >> magic_rook_shift[sq];
    return magic_rook_indices[sq][index];
}

inline uint64_t get_bishop_attacks(int sq, uint64_t occupied) {
    uint64_t blockers = occupied & magic_bishop_mask[sq];
    uint64_t index = (blockers * magic_bishop[sq]) >> magic_bishop_shift[sq];
    return magic_bishop_indices[sq][index];
}

inline uint64_t get_queen_attacks(int sq, uint64_t occupied) {
    return get_rook_attacks(sq, occupied) | get_bishop_attacks(sq, occupied);
}

bool is_square_attacked(const Position& pos, int sq_to_check, int attacker_c) {
    if (sq_to_check < 0 || sq_to_check >= 64) return false;
    uint64_t attacker_pawns = pos.piece_bb[PAWN] & pos.color_bb[attacker_c];
    if (pawn_attacks_bb[1 - attacker_c][sq_to_check] & attacker_pawns) return true;

    uint64_t attacker_knights = pos.piece_bb[KNIGHT] & pos.color_bb[attacker_c];
    if (knight_attacks_bb[sq_to_check] & attacker_knights) return true;

    uint64_t attacker_king = pos.piece_bb[KING] & pos.color_bb[attacker_c];
    if (king_attacks_bb[sq_to_check] & attacker_king) return true;

    uint64_t occupied = pos.get_occupied_bb();

    uint64_t rook_queen_attackers = (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_c];
    if (get_rook_attacks(sq_to_check, occupied) & rook_queen_attackers) return true;

    uint64_t bishop_queen_attackers = (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_c];
    if (get_bishop_attacks(sq_to_check, occupied) & bishop_queen_attackers) return true;

    return false;
}

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
        int from = pop_lsb(pawns);
        int rank = from / 8;
        int promotion_rank_idx = (stm == WHITE) ? 6 : 1;

        int one_step_sq = (stm == WHITE) ? from + 8 : from - 8;
        if (one_step_sq >= 0 && one_step_sq < 64 && get_bit(empty_squares, one_step_sq)) {
            if (rank == promotion_rank_idx) {
                moves_list[move_count++] = {from, one_step_sq, QUEEN}; moves_list[move_count++] = {from, one_step_sq, ROOK};
                moves_list[move_count++] = {from, one_step_sq, BISHOP}; moves_list[move_count++] = {from, one_step_sq, KNIGHT};
            } else if (!captures_only) {
                moves_list[move_count++] = {from, one_step_sq};
            }
            
            if (!captures_only && rank == ((stm == WHITE) ? 1 : 6)) {
                int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                if (two_steps_sq >= 0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq)) {
                    moves_list[move_count++] = {from, two_steps_sq};
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
            int to = pop_lsb(pawn_cap_targets);
            if (rank == promotion_rank_idx) {
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
            int from = pop_lsb(pieces_of_type);
            uint64_t attacks = 0;
            if (p_type == KNIGHT) attacks = knight_attacks_bb[from];
            else if (p_type == BISHOP) attacks = get_bishop_attacks(from, occupied);
            else if (p_type == ROOK) attacks = get_rook_attacks(from, occupied);
            else if (p_type == QUEEN) attacks = get_queen_attacks(from, occupied);
            else if (p_type == KING) attacks = king_attacks_bb[from];
            attacks &= (captures_only ? opp_pieces : ~my_pieces);
            while (attacks) {
                int to = pop_lsb(attacks);
                moves_list[move_count++] = {from, to};
            }
        }
    }

    if (!captures_only) {
        uint64_t king_bb = pos.piece_bb[KING] & my_pieces;
        if (king_bb) {
            int king_sq_idx = lsb_index(king_bb);
            if (stm == WHITE) {
                if ((pos.castling_rights & WK_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ + 1) && !get_bit(occupied, E1_SQ + 2) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ + 1, BLACK) && !is_square_attacked(pos, E1_SQ + 2, BLACK)) {
                    moves_list[move_count++] = {king_sq_idx, G1_SQ};
                }
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    !get_bit(occupied, E1_SQ - 1) && !get_bit(occupied, E1_SQ - 2) && !get_bit(occupied, E1_SQ - 3) &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ - 1, BLACK) && !is_square_attacked(pos, E1_SQ - 2, BLACK)) {
                    moves_list[move_count++] = {king_sq_idx, C1_SQ};
                }
            } else {
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ + 1) && !get_bit(occupied, E8_SQ + 2) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ + 1, WHITE) && !is_square_attacked(pos, E8_SQ + 2, WHITE)) {
                    moves_list[move_count++] = {king_sq_idx, G8_SQ};
                }
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    !get_bit(occupied, E8_SQ - 1) && !get_bit(occupied, E8_SQ - 2) && !get_bit(occupied, E8_SQ - 3) &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ - 1, WHITE) && !is_square_attacked(pos, E8_SQ - 2, WHITE)) {
                    moves_list[move_count++] = {king_sq_idx, C8_SQ};
                }
            }
        }
    }
    return move_count;
}

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
    next_pos.pawn_zobrist_key = pos.pawn_zobrist_key;
    next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.from];

    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;

    next_pos.halfmove_clock++;

    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[opp][piece_captured][move.to];
        if(piece_captured == PAWN) {
            next_pos.pawn_zobrist_key ^= zobrist_pieces[opp][PAWN][move.to];
        }
        next_pos.halfmove_clock = 0;
    }
    if (piece_moved == PAWN) {
        next_pos.halfmove_clock = 0;
        next_pos.pawn_zobrist_key ^= zobrist_pieces[stm][PAWN][move.from];
    }

    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) {
        int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
        next_pos.piece_bb[PAWN] &= ~set_bit(captured_pawn_sq);
        next_pos.color_bb[opp] &= ~set_bit(captured_pawn_sq);
        next_pos.zobrist_hash ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
        next_pos.pawn_zobrist_key ^= zobrist_pieces[opp][PAWN][captured_pawn_sq];
    }

    next_pos.zobrist_hash ^= zobrist_ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    next_pos.ep_square = -1;
    if (piece_moved == PAWN && std::abs(move.to - move.from) == 16) {
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
        if (piece_moved == PAWN) {
             next_pos.pawn_zobrist_key ^= zobrist_pieces[stm][PAWN][move.to];
        }
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    uint8_t old_castling_rights = pos.castling_rights;
    uint8_t new_castling_rights = pos.castling_rights;

    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);

        if (std::abs(move.to - move.from) == 2) {
            int rook_from_sq, rook_to_sq;
            if (move.to == G1_SQ)      { rook_from_sq = H1_SQ; rook_to_sq = E1_SQ + 1; }
            else if (move.to == C1_SQ) { rook_from_sq = A1_SQ; rook_to_sq = E1_SQ - 1; }
            else if (move.to == G8_SQ) { rook_from_sq = H8_SQ; rook_to_sq = E8_SQ + 1; }
            else                       { rook_from_sq = A8_SQ; rook_to_sq = E8_SQ - 1; }

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

    uint64_t king_bb = next_pos.piece_bb[KING] & next_pos.color_bb[stm];
    if (king_bb) {
        int king_sq_after_move = lsb_index(king_bb);
        if (is_square_attacked(next_pos, king_sq_after_move, opp)) {
            return pos;
        }
    } else {
        return pos;
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
uint64_t adjacent_files_mask[8];

const int passed_pawn_bonus_mg[8] = {0, 5, 15, 25, 40, 60, 80, 0}; // Bonus by rank (0-indexed from own side)
const int passed_pawn_bonus_eg[8] = {0, 10, 25, 40, 60, 90, 120, 0};

// --- Evaluation Constants ---
const int TEMPO_BONUS = 8;
const int BISHOP_PAIR_BONUS_MG = 30;
const int BISHOP_PAIR_BONUS_EG = 50;
const int PAWN_CONNECTED_BONUS_MG = 10;
const int PAWN_CONNECTED_BONUS_EG = 14;
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
const int passed_pawn_enemy_king_dist_bonus_eg = 4; // bonus per square of Chebyshev distance in endgame
const int ROOK_ON_OPEN_FILE_MG = 20;
const int ROOK_ON_OPEN_FILE_EG = 15;
const int ROOK_ON_SEMI_OPEN_FILE_MG = 10;
const int ROOK_ON_SEMI_OPEN_FILE_EG = 5;

// --- Evaluation Constants for King Safety and Rook on 7th ---
// Piece Tropism: Bonus for pieces near the enemy king (by Chebyshev distance)
const int knight_tropism_bonus[8] = { 20, 15, 10, 5, 2, 1, 0, 0 };
const int bishop_tropism_bonus[8] = { 15, 12, 9,  4, 2, 1, 0, 0 };
const int rook_tropism_bonus[8]   = { 25, 20, 15, 10, 5, 2, 1, 0 };
const int queen_tropism_bonus[8]  = { 40, 30, 20, 10, 5, 2, 1, 0 };

// King Threat Penalty Table (indexed by combined tropism score)
const int king_threat_penalty_mg[31] = {
    0,  0,  4,  8, 16, 25, 35, 46, 58, 70, 85, 100, 115, 130, 150,
  170,190,210,230,250,270,290,310,330,350,370, 390, 410, 430, 450, 470
};
const int king_threat_penalty_eg[31] = {
    0,  0,  2,  4,  8, 12, 16, 20, 25, 30, 35,  40,  45,  50,  55,
   60, 65, 70, 75, 80, 85, 90, 95,100,105,110, 115, 120, 125, 130, 135
};

// King Shelter Penalties (Middlegame only)
const int SHIELD_PAWN_PRESENT_BONUS = 10;
const int SHIELD_PAWN_MISSING_PENALTY = -20;
const int SHIELD_PAWN_ADVANCED_PENALTY = -12;
const int SHIELD_OPEN_FILE_PENALTY = -15;

// Rook on 7th Rank Bonuses
const int ROOK_ATTACK_RANK_MG = 25;
const int ROOK_ATTACK_RANK_EG = 35;
const int ATTACK_RANK_KING_TRAP_MG = 15;
const int ATTACK_RANK_KING_TRAP_EG = 20;
const int ATTACK_RANK_CONNECTIVITY_MG = 12;
const int ATTACK_RANK_CONNECTIVITY_EG = 18;


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
    if (pos.piece_bb[PAWN] != 0 || pos.piece_bb[ROOK] != 0 || pos.piece_bb[QUEEN] != 0) return false;

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
    if ((white_minors == 1 && black_minors == 0) || (white_minors == 0 && black_minors == 1)) return true;

    // Case: K + minor vs K + minor (covers K+N vs K+N, K+N vs K+B, K+B vs K+B)
    if (white_minors == 1 && black_minors == 1) return true;

    // Case: K+N+N vs K (generally treated as a draw)
    if ((white_minors == 2 && black_minors == 0 && white_knights == 2) ||
        (white_minors == 0 && black_minors == 2 && black_knights == 2)) return true;

    // All other cases (like K+B+N vs K, K+B+B vs K) are not considered drawn by default.
    return false;
}

// Helper function to evaluate pawn structure for one color
void evaluate_pawn_structure_for_color(const Position& pos, Color current_eval_color,
                                       int& mg_pawn_score, int& eg_pawn_score,
                                       uint64_t& passed_pawns) {
    uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[current_eval_color];
    uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1 - current_eval_color];
    uint64_t temp_pawns = all_friendly_pawns;
    
    uint64_t enemy_pawn_attacks = 0;
    uint64_t temp_enemy_pawns = all_enemy_pawns;
    while(temp_enemy_pawns) {
        int pawn_sq = lsb_index(temp_enemy_pawns);
        enemy_pawn_attacks |= pawn_attacks_bb[1 - current_eval_color][pawn_sq];
        temp_enemy_pawns &= temp_enemy_pawns - 1;
    }

    while (temp_pawns) {
        int sq = lsb_index(temp_pawns);
        temp_pawns &= temp_pawns - 1;
        int f = sq % 8;

        // Isolated Pawn Evaluation
        if ((adjacent_files_mask[f] & all_friendly_pawns) == 0) {
            mg_pawn_score += isolated_pawn_penalty_mg;
            eg_pawn_score += isolated_pawn_penalty_eg;
        }

        // Protected Pawn Evaluation
        if (pawn_attacks_bb[1 - current_eval_color][sq] & all_friendly_pawns) {
            mg_pawn_score += protected_pawn_bonus_mg;
            eg_pawn_score += protected_pawn_bonus_eg;
        }

        // Connected Pawn (Phalanx) Bonus
        if (get_bit(east(set_bit(sq)), all_friendly_pawns)) {
            mg_pawn_score += PAWN_CONNECTED_BONUS_MG;
            eg_pawn_score += PAWN_CONNECTED_BONUS_EG;
        }
        
        // Doubled Pawn Evaluation
        uint64_t forward_file_squares = (current_eval_color == WHITE) ? north(set_bit(sq)) : south(set_bit(sq));
        if ((file_bb_mask[f] & forward_file_squares & all_friendly_pawns) != 0) {
                mg_pawn_score += doubled_pawn_liability_mg;
                eg_pawn_score += doubled_pawn_liability_eg;
        }
        
        // Backward Pawn Evaluation
        uint64_t front_span = (current_eval_color == WHITE) ? white_passed_pawn_block_mask[sq] : black_passed_pawn_block_mask[sq];
        uint64_t adjacent_pawns = adjacent_files_mask[f] & all_friendly_pawns;
        if ((front_span & adjacent_pawns) == 0) { // No pawns on adjacent files ahead of us
            int push_sq = (current_eval_color == WHITE) ? sq + 8 : sq - 8;
            if (get_bit(enemy_pawn_attacks, push_sq)) {
                mg_pawn_score += hindered_pawn_penalty_mg;
                eg_pawn_score += hindered_pawn_penalty_eg;
            }
        }

        // Passed Pawn Detection
        bool is_passed = false;
        if (current_eval_color == WHITE) {
            if ((white_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0) is_passed = true;
        } else {
            if ((black_passed_pawn_block_mask[sq] & all_enemy_pawns) == 0) is_passed = true;
        }
        if (is_passed) {
            passed_pawns |= set_bit(sq);
            int rank_from_own_side = (current_eval_color == WHITE) ? (sq / 8) : (7 - (sq / 8));
            mg_pawn_score += passed_pawn_bonus_mg[rank_from_own_side];
            eg_pawn_score += passed_pawn_bonus_eg[rank_from_own_side];
        }
    }
}

int evaluate(Position& pos) {
    // Check for insufficient material draw at the beginning of evaluation.
    if (is_insufficient_material(pos)) return 0; // Draw score

    int mg_score = 0, eg_score = 0, game_phase = 0;
    int mg_mobility_score = 0, eg_mobility_score = 0;
    
    // --- Pawn Evaluation (with Caching) ---
    int mg_pawn_score = 0;
    int eg_pawn_score = 0;
    uint64_t white_passed_pawns = 0;
    uint64_t black_passed_pawns = 0;

    PawnCacheEntry& pawn_entry = pawn_evaluation_cache[pos.pawn_zobrist_key & pawn_cache_mask];
    if (pawn_entry.key == pos.pawn_zobrist_key) {
        // Cache hit
        mg_pawn_score = pawn_entry.mg_score;
        eg_pawn_score = pawn_entry.eg_score;
        white_passed_pawns = pawn_entry.white_passed_pawns;
        black_passed_pawns = pawn_entry.black_passed_pawns;
    } else {
        // Cache miss: evaluate and store
        int white_mg = 0, white_eg = 0;
        int black_mg = 0, black_eg = 0;
        evaluate_pawn_structure_for_color(pos, WHITE, white_mg, white_eg, white_passed_pawns);
        evaluate_pawn_structure_for_color(pos, BLACK, black_mg, black_eg, black_passed_pawns);
        
        mg_pawn_score = white_mg - black_mg;
        eg_pawn_score = white_eg - black_eg;
        
        pawn_entry.key = pos.pawn_zobrist_key;
        pawn_entry.mg_score = mg_pawn_score;
        pawn_entry.eg_score = eg_pawn_score;
        pawn_entry.white_passed_pawns = white_passed_pawns;
        pawn_entry.black_passed_pawns = black_passed_pawns;
    }
    mg_score += mg_pawn_score;
    eg_score += eg_pawn_score;
    // --- End of Pawn Evaluation ---

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
        uint64_t temp_enemy_pawns_for_attack_map = all_enemy_pawns;
        while(temp_enemy_pawns_for_attack_map) {
            enemy_pawn_attacks |= pawn_attacks_bb[enemy_color][pop_lsb(temp_enemy_pawns_for_attack_map)];
        }
        
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = pop_lsb(b);
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);

                mg_score += side_multiplier * (piece_values_mg[p] + pst_mg_all[p][mirrored_sq]);
                eg_score += side_multiplier * (piece_values_eg[p] + pst_eg_all[p][mirrored_sq]);

                if ((Piece)p == PAWN) {
                    uint64_t current_passed_pawns = (current_eval_color == WHITE) ? white_passed_pawns : black_passed_pawns;
                    if (get_bit(current_passed_pawns, sq)) {
                        uint64_t enemy_king_bb = pos.piece_bb[KING] & enemy_pieces;
                        if (enemy_king_bb) {
                            int enemy_king_sq = lsb_index(enemy_king_bb);
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
                        if (!enemy_pawn_on_file) {
                            mg_score += side_multiplier * ROOK_ON_OPEN_FILE_MG; 
                            eg_score += side_multiplier * ROOK_ON_OPEN_FILE_EG; 
                        } else {
                            mg_score += side_multiplier * ROOK_ON_SEMI_OPEN_FILE_MG; 
                            eg_score += side_multiplier * ROOK_ON_SEMI_OPEN_FILE_EG;
                        } 
                    }
                    
                    int attack_rank = (current_eval_color == WHITE) ? 6 : 1;
                    if (sq / 8 == attack_rank) {
                        mg_score += side_multiplier * ROOK_ATTACK_RANK_MG;
                        eg_score += side_multiplier * ROOK_ATTACK_RANK_EG;

                        uint64_t enemy_king_bb = pos.piece_bb[KING] & enemy_pieces;
                        if (enemy_king_bb) {
                            int enemy_king_sq = lsb_index(enemy_king_bb);
                            int enemy_back_rank = (enemy_color == WHITE) ? 0 : 7;
                            if (enemy_king_sq / 8 == enemy_back_rank) {
                                mg_score += side_multiplier * ATTACK_RANK_KING_TRAP_MG;
                                eg_score += side_multiplier * ATTACK_RANK_KING_TRAP_EG;
                            }
                        }
                        
                        uint64_t friendly_heavies = (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & friendly_pieces;
                        if (get_rook_attacks(sq, occupied) & friendly_heavies & ~set_bit(sq)) {
                            mg_score += side_multiplier * ATTACK_RANK_CONNECTIVITY_MG;
                            eg_score += side_multiplier * ATTACK_RANK_CONNECTIVITY_EG;
                        }
                    }

                    uint64_t mobility_attacks = get_rook_attacks(sq, occupied);
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
                        if (get_bit(pawn_attacks_bb[enemy_color][sq], all_friendly_pawns) && !get_bit(enemy_pawn_attacks, sq)) {
                            mg_score += side_multiplier * dominant_knight_bonus_mg;
                            eg_score += side_multiplier * dominant_knight_bonus_eg;
                        }
                    }
                    uint64_t potential_outpost_moves = mobility_attacks & ~occupied;
                    while(potential_outpost_moves) {
                        int to_sq = pop_lsb(potential_outpost_moves);
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
                    uint64_t mobility_attacks = get_bishop_attacks(sq, occupied);
                    piece_attacks_bb[c_idx][BISHOP] |= mobility_attacks;
                    int mobility_count = pop_count(mobility_attacks & attackable_squares);
                    mg_mobility_score += side_multiplier * mobility_count * bishop_mobility_bonus_mg;
                    eg_mobility_score += side_multiplier * mobility_count * bishop_mobility_bonus_eg;

                    int rank = sq / 8;
                    int relative_rank_idx = (current_eval_color == WHITE) ? rank : 7 - rank;
                     if (relative_rank_idx >= 3 && relative_rank_idx <= 5) {
                        if (get_bit(pawn_attacks_bb[enemy_color][sq], all_friendly_pawns) && !get_bit(enemy_pawn_attacks, sq)) {
                            mg_score += side_multiplier * dominant_bishop_bonus_mg;
                            eg_score += side_multiplier * dominant_bishop_bonus_eg;
                        }
                    }
                } else if ((Piece)p == QUEEN) {
                    uint64_t mobility_attacks = get_queen_attacks(sq, occupied);
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
            mg_score += side_multiplier * BISHOP_PAIR_BONUS_MG; 
            eg_score += side_multiplier * BISHOP_PAIR_BONUS_EG;
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

        uint64_t king_bb = pos.piece_bb[KING] & friendly_pieces;
        if(king_bb) {
            int king_sq = lsb_index(king_bb);
            int king_file = king_sq % 8;
            int king_rank = king_sq / 8;

            int shelter_score = 0;
            int shield_f_start = std::max(0, king_file - 1);
            int shield_f_end = std::min(7, king_file + 1);
            int shield_rank_1 = (current_eval_color == WHITE) ? king_rank + 1 : king_rank - 1;
            int shield_rank_2 = (current_eval_color == WHITE) ? king_rank + 2 : king_rank - 2;

            if (shield_rank_1 >= 0 && shield_rank_1 < 8) {
                for (int f = shield_f_start; f <= shield_f_end; ++f) {
                    uint64_t friendly_pawns_on_file = file_bb_mask[f] & all_friendly_pawns;

                    if (!friendly_pawns_on_file) {
                        shelter_score += SHIELD_PAWN_MISSING_PENALTY;
                        if (!(file_bb_mask[f] & all_enemy_pawns)) {
                            shelter_score += SHIELD_OPEN_FILE_PENALTY;
                        }
                    } else {
                        bool pawn_on_shield_rank_1 = get_bit(friendly_pawns_on_file, shield_rank_1 * 8 + f);
                        bool pawn_on_shield_rank_2 = (shield_rank_2 >= 0 && shield_rank_2 < 8) ? get_bit(friendly_pawns_on_file, shield_rank_2 * 8 + f) : false;

                        if (pawn_on_shield_rank_1) {
                            shelter_score += SHIELD_PAWN_PRESENT_BONUS;
                        } else if (pawn_on_shield_rank_2) {
                            shelter_score += SHIELD_PAWN_ADVANCED_PENALTY;
                        } else {
                            shelter_score += SHIELD_PAWN_MISSING_PENALTY;
                        }
                    }
                }
            }

            int home_rank = (current_eval_color == WHITE) ? 0 : 7;
            if (king_rank == home_rank || (current_eval_color == WHITE && king_rank == 0 && (king_file == 6 || king_file == 2)) || (current_eval_color == BLACK && king_rank == 7 && (king_file == 6 || king_file == 2))) {
                mg_score += side_multiplier * shelter_score;
            }

            int attack_proximity_score = 0;
            uint64_t temp_b = enemy_pieces & ~pos.piece_bb[PAWN];
            while (temp_b) {
                int attacker_sq = pop_lsb(temp_b);
                int dist = std::max(std::abs(king_rank - attacker_sq/8), std::abs(king_file - attacker_sq%8));
                Piece p_type = pos.piece_on_sq(attacker_sq);
                if (p_type == KNIGHT) attack_proximity_score += knight_tropism_bonus[dist];
                else if (p_type == BISHOP) attack_proximity_score += bishop_tropism_bonus[dist];
                else if (p_type == ROOK) attack_proximity_score += rook_tropism_bonus[dist];
                else if (p_type == QUEEN) attack_proximity_score += queen_tropism_bonus[dist];
            }

            int danger_idx = std::min(attack_proximity_score / 10, 30);
            mg_score -= side_multiplier * king_threat_penalty_mg[danger_idx];
            eg_score -= side_multiplier * king_threat_penalty_eg[danger_idx];
        }
    }

    mg_score += mg_mobility_score;
    eg_score += eg_mobility_score;

    if (pos.side_to_move == WHITE) {
        mg_score += TEMPO_BONUS;
        eg_score += TEMPO_BONUS;
    } else {
        mg_score -= TEMPO_BONUS;
        eg_score -= TEMPO_BONUS;
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

// Search Heuristics
Move killer_moves[MAX_PLY][2];
int move_history_score[2][64][64];
Move refutation_moves[64][64];       // For Counter-Move Heuristic
int search_reductions[MAX_PLY][256]; // For Table-Driven LMR
constexpr int MAX_HISTORY_SCORE = 24000;

// Repetition Detection Data Structures
uint64_t game_history_hashes[256]; // Stores hashes of positions for repetition checks
int game_history_length = 0;
uint64_t search_path_hashes[MAX_PLY]; // Stores hashes of positions in the current search path

void reset_search_state() {
    nodes_searched = 0;
    stop_search_flag = false;
    use_time_limits = false;
}

void reset_search_heuristics() {
    std::memset(killer_moves, 0, sizeof(killer_moves));
    std::memset(move_history_score, 0, sizeof(move_history_score));
    std::memset(refutation_moves, 0, sizeof(refutation_moves));

    // Initialize LMR table
    for (int d = 1; d < MAX_PLY; ++d) {
        for (int m = 1; m < 256; ++m) {
            // Formula: log(depth) * log(moves_searched) / C
            double reduction = (log(d) * log(m)) / 2.3;
            
            int r = static_cast<int>(reduction);

            // Basic caps and conditions
            if (d < 3 || m < 2) r = 0;
            if (d > 8 && m > 4) r++;
            
            r = std::max(0, r);
            r = std::min(r, d - 2); // Ensure at least 1 ply of search remains after reduction
            
            search_reductions[d][m] = r;
        }
    }
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

void score_moves(const Position& pos, Move* moves, int num_moves, const Move& tt_move, int ply, const Move& prev_move) {
    Move refutation = (ply > 0 && !prev_move.is_null()) ? refutation_moves[prev_move.from][prev_move.to] : NULL_MOVE;

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
            } else if (!refutation.is_null() && m == refutation) {
                m.score = 850000; // Refutation move
            } else if (ply < MAX_PLY && !killer_moves[ply][0].is_null() && m == killer_moves[ply][0]) {
                m.score = 800000; // First killer move
            } else if (ply < MAX_PLY && !killer_moves[ply][1].is_null() && m == killer_moves[ply][1]) {
                m.score = 700000; // Second killer move
            } else {
                // History heuristic for quiet moves
                m.score = move_history_score[pos.side_to_move][m.from][m.to];
            }
        }
    }
    // Sort moves by score in descending order
    std::sort(moves, moves + num_moves, [](const Move& a, const Move& b){ return a.score > b.score; });
}

int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return evaluate(pos);

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

    score_moves(pos, q_moves, num_q_moves, NULL_MOVE, ply, NULL_MOVE);

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

// Search function signature and repetition logic
int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, const Move& prev_move) {
    if (ply >= MAX_PLY - 1) return evaluate(pos);
    
    if (ply > 0) {
        if (pos.halfmove_clock >= 100 || is_insufficient_material(pos)) return 0;
        for (int k = ply - 2; k >= 0 && k >= ply - pos.halfmove_clock; k -= 2) {
            if (search_path_hashes[k] == pos.zobrist_hash) return 0;
        }
    }
    
    search_path_hashes[ply] = pos.zobrist_hash;

    if (check_time()) return 0;

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score)) {
        return tt_score;
    }

    nodes_searched++;

    uint64_t king_bb = pos.piece_bb[KING] & pos.color_bb[pos.side_to_move];
    bool in_check = king_bb ? is_square_attacked(pos, lsb_index(king_bb), 1 - pos.side_to_move) : false;
    if (in_check) depth++;

    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

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
            null_next_pos.zobrist_hash ^= zobrist_side_to_move;
            if (pos.ep_square != -1) {
                null_next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];
                null_next_pos.ep_square = -1;
                null_next_pos.zobrist_hash ^= zobrist_ep[64];
            }
            null_next_pos.ply = pos.ply + 1;

            int R_nmp = (depth > 6) ? 3 : 2;
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false, NULL_MOVE);
            
            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 if (null_score >= MATE_THRESHOLD) null_score = beta;
                 store_tt(pos.zobrist_hash, depth, ply, null_score, TT_LOWER, NULL_MOVE);
                 return beta;
            }
    }

    Move moves[256];
    int num_moves = generate_moves(pos, moves, false);
    score_moves(pos, moves, num_moves, tt_move, ply, prev_move);

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
        for (int k = ply - 1; k >= 0 && k >= ply - pos.halfmove_clock; k -= 2) {
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
                score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_move);
            } else {
                int R_lmr = 0;
                if (depth >= 3 && i >= 1 && !in_check && current_move.promotion == NO_PIECE &&
                    pos.piece_on_sq(current_move.to) == NO_PIECE) {
                    R_lmr = search_reductions[depth][i];
                }

                score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_move);
                if (score > alpha && R_lmr > 0) {
                     score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_move);
                }
                if (score > alpha && score < beta) {
                     score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, current_move);
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
                    if (pos.piece_on_sq(current_move.to) == NO_PIECE && current_move.promotion == NO_PIECE) {
                        if (ply < MAX_PLY) {
                            if (!(current_move == killer_moves[ply][0])) {
                                killer_moves[ply][1] = killer_moves[ply][0];
                                killer_moves[ply][0] = current_move;
                            }
                        }
                        if (ply > 0 && !prev_move.is_null()) {
                            refutation_moves[prev_move.from][prev_move.to] = current_move;
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

void uci_loop() {
    char line_buffer[16384]; // Large buffer for UCI commands
    parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    while (fgets(line_buffer, sizeof(line_buffer), stdin)) {
        std::string line(line_buffer);
        std::istringstream ss(line);
        std::string token;
        ss >> token;

        if (token == "uci") {
            printf("id name Amira 1.4\n");
            printf("id author ChessTubeTree\n");
            printf("option name Hash type spin default %d min 0 max 1024\n", TT_SIZE_MB_DEFAULT);
            printf("uciok\n");
            fflush(stdout);
        } else if (token == "isready") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }
            printf("readyok\n");
            fflush(stdout);
        } else if (token == "setoption") {
            std::string name_token, value_token, name_str, value_str_val;
            ss >> name_token >> name_str >> value_token >> value_str_val;
            if (name_str == "Hash") {
                try {
                    g_configured_tt_size_mb = std::stoi(value_str_val);
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true;
                } catch (...) {}
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (g_tt_is_initialized) clear_tt();
            else { init_tt(g_configured_tt_size_mb); g_tt_is_initialized = true; }
            clear_pawn_cache();
            reset_search_heuristics();
            game_history_length = 0;
        } else if (token == "position") {
            std::string fen_str_collector;
            ss >> token;
            if (token == "startpos") {
                parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                std::string next_token_check;
                if (ss >> next_token_check && next_token_check == "moves") {
                    token = "moves";
                } else {
                    token = "";
                }
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

            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null()) break;

                    if (game_history_length < 256) {
                        game_history_hashes[game_history_length++] = uci_root_pos.zobrist_hash;
                    }
                    Piece moved_piece = uci_root_pos.piece_on_sq(m.from);
                    Piece captured_piece = uci_root_pos.piece_on_sq(m.to);
                    if (moved_piece == PAWN || captured_piece != NO_PIECE) {
                        game_history_length = 0;
                    }
                    bool legal;
                    uci_last_root_move = m;
                    uci_root_pos = make_move(uci_root_pos, m, legal);
                    if (!legal) break;
                }
            }
        } else if (token == "go") {
            if (!g_tt_is_initialized) {
                init_tt(g_configured_tt_size_mb);
                g_tt_is_initialized = true;
            }

            Move root_pseudo_moves[256];
            int num_pseudo_moves = generate_moves(uci_root_pos, root_pseudo_moves, false);
            std::vector<Move> root_legal_moves;
            for (int i = 0; i < num_pseudo_moves; ++i) {
                bool is_legal_flag;
                make_move(uci_root_pos, root_pseudo_moves[i], is_legal_flag);
                if (is_legal_flag) {
                    root_legal_moves.push_back(root_pseudo_moves[i]);
                }
            }

            if (root_legal_moves.empty()) {
                printf("bestmove 0000\n");
                fflush(stdout);
                continue;
            }
            if (root_legal_moves.size() == 1) {
                printf("bestmove %s\n", move_to_uci(root_legal_moves[0]).c_str());
                fflush(stdout);
                continue;
            }

            long long wtime = -1, btime = -1, winc = 0, binc = 0;
            int movestogo = 0;
            int max_depth_to_search = MAX_PLY;

            std::string go_param;
            while(ss >> go_param) {
                if (go_param == "wtime") ss >> wtime;
                else if (go_param == "btime") ss >> btime;
                else if (go_param == "winc") ss >> winc;
                else if (go_param == "binc") ss >> binc;
                else if (go_param == "movestogo") ss >> movestogo;
                else if (go_param == "depth") ss >> max_depth_to_search;
            }

            reset_search_state();
            search_start_timepoint = std::chrono::steady_clock::now();

            long long my_time = (uci_root_pos.side_to_move == WHITE) ? wtime : btime;
            long long my_inc = (uci_root_pos.side_to_move == WHITE) ? winc : binc;

            if (my_time != -1) {
                use_time_limits = true;
                long long allocated_time_ms;
                if (my_inc > 0) {
                    allocated_time_ms = (my_time / 20) + (my_inc * 3 / 4);
                } else {
                    int divisor = (movestogo > 0) ? movestogo : 25;
                    allocated_time_ms = my_time / divisor;
                }
                allocated_time_ms = std::min(allocated_time_ms, my_time - 100);
                if (allocated_time_ms < 0) allocated_time_ms = 5;
                soft_limit_timepoint = search_start_timepoint + std::chrono::milliseconds(allocated_time_ms);
                hard_limit_timepoint = search_start_timepoint + std::chrono::milliseconds(allocated_time_ms * 2);
            } else {
                use_time_limits = false;
            }

            uci_best_move_overall = NULL_MOVE;
            int best_score_overall = 0;

            int aspiration_alpha = -INF_SCORE;
            int aspiration_beta = INF_SCORE;
            int aspiration_window_delta = 25;

            for (int depth = 1; depth <= max_depth_to_search; ++depth) {
                int current_score;
                if (depth <= 1)
                     current_score = search(uci_root_pos, depth, -INF_SCORE, INF_SCORE, 0, true, true, uci_last_root_move);
                else {
                    current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, uci_last_root_move);
                    if (!stop_search_flag && (current_score <= aspiration_alpha || current_score >= aspiration_beta)) {
                        aspiration_alpha = -INF_SCORE;
                        aspiration_beta = INF_SCORE;
                        current_score = search(uci_root_pos, depth, aspiration_alpha, aspiration_beta, 0, true, true, uci_last_root_move);
                    }
                }

                if (stop_search_flag && depth > 1) break;

                if (std::abs(current_score) < MATE_THRESHOLD) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 3 + 5;
                    if (aspiration_window_delta > 300) aspiration_window_delta = 300;
                } else {
                    aspiration_alpha = -INF_SCORE;
                    aspiration_beta = INF_SCORE;
                }

                Move tt_root_move = NULL_MOVE;
                if (!stop_search_flag) {
                    TTEntry& root_entry = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                    if (root_entry.hash == uci_root_pos.zobrist_hash && !root_entry.best_move.is_null()) {
                        uci_best_move_overall = root_entry.best_move;
                    }
                    best_score_overall = current_score;
                }

                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - search_start_timepoint).count();

                printf("info depth %d score cp %d", depth, best_score_overall);
                if (best_score_overall > MATE_THRESHOLD) printf(" mate %d", (MATE_SCORE - best_score_overall + 1)/2);
                else if (best_score_overall < -MATE_THRESHOLD) printf(" mate %d", -(MATE_SCORE + best_score_overall + 1)/2);
                printf(" nodes %llu time %lld", nodes_searched, elapsed_ms > 0 ? elapsed_ms : 0);
                if (elapsed_ms > 0) printf(" nps %llu", (nodes_searched * 1000 / elapsed_ms));

                if (!uci_best_move_overall.is_null()) {
                    printf(" pv");
                    Position temp_pos = uci_root_pos;
                    std::vector<uint64_t> pv_hashes;
                    pv_hashes.push_back(temp_pos.zobrist_hash);
                    for (int pv_idx = 0; pv_idx < depth; ++pv_idx) {
                        TTEntry& pv_entry = transposition_table[temp_pos.zobrist_hash & tt_mask];
                        if (pv_entry.hash == temp_pos.zobrist_hash && !pv_entry.best_move.is_null()) {
                            Move pv_m = pv_entry.best_move;
                            bool legal_pv;
                            Position next_temp_pos = make_move(temp_pos, pv_m, legal_pv);
                            
                            bool is_cycle = false;
                            for(uint64_t hash : pv_hashes) { if (hash == next_temp_pos.zobrist_hash) { is_cycle = true; break; } }
                            if (is_cycle) break;

                            if (legal_pv) {
                                printf(" %s", move_to_uci(pv_m).c_str());
                                temp_pos = next_temp_pos;
                                pv_hashes.push_back(temp_pos.zobrist_hash);
                            } else { break; }
                        } else { break; }
                        if (stop_search_flag) break;
                    }
                }
                printf("\n");
                fflush(stdout);

                if (use_time_limits && std::chrono::steady_clock::now() > soft_limit_timepoint) break;
                if (std::abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break;
            }

            if (!uci_best_move_overall.is_null()) {
                 printf("bestmove %s\n", move_to_uci(uci_best_move_overall).c_str());
            } else {
                printf("bestmove %s\n", move_to_uci(root_legal_moves[0]).c_str());
            }
            fflush(stdout);

        } else if (token == "quit" || token == "stop") {
            stop_search_flag = true;
            if (token == "quit") break;
        }
    }
}

int main(int argc, char* argv[]) {
    init_zobrist();
    init_attack_tables();
    init_magic_bitboards();
    init_eval_masks();
    init_pawn_cache();
    reset_search_heuristics();

    uci_loop();
    return 0;
}
