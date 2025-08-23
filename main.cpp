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
#include <functional> // For std::partition

// Bit manipulation builtins (MSVC/GCC specific)
#if defined(_MSC_VER)
#include <intrin.h>
#endif

// --- PhaseScore Structure ---
struct PhaseScore {
    int mg = 0;
    int eg = 0;

    PhaseScore& operator+=(const PhaseScore& other) {
        mg += other.mg;
        eg += other.eg;
        return *this;
    }
    PhaseScore& operator-=(const PhaseScore& other) {
        mg -= other.mg;
        eg -= other.eg;
        return *this;
    }
    PhaseScore operator+(const PhaseScore& other) const {
        PhaseScore result = *this;
        result += other;
        return result;
    }
    PhaseScore operator-(const PhaseScore& other) const {
        PhaseScore result = *this;
        result -= other;
        return result;
    }
    PhaseScore operator-() const {
        return {-mg, -eg};
    }
    PhaseScore& operator*=(int scalar) {
        mg *= scalar;
        eg *= scalar;
        return *this;
    }
    PhaseScore operator*(int scalar) const {
        PhaseScore result = *this;
        result *= scalar;
        return result;
    }
};


// --- Constants ---
enum Piece { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE };
enum Color { WHITE, BLACK, NO_COLOR };

constexpr int MAX_PLY = 128;
constexpr int TT_SIZE_MB_DEFAULT = 512;
constexpr int PAWN_CACHE_SIZE_ENTRIES = 131072; // 2^17 entries
constexpr int MATE_SCORE = 30000;
constexpr int MATE_THRESHOLD = MATE_SCORE - MAX_PLY;
constexpr int INF_SCORE = 32000;
constexpr int NO_EVAL_STORED = INF_SCORE + 1; // A value that evaluate() should not return

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
constexpr uint64_t WK_CASTLE_PATH = (1ULL << (E1_SQ + 1)) | (1ULL << (E1_SQ + 2));
constexpr uint64_t WQ_CASTLE_PATH = (1ULL << (E1_SQ - 1)) | (1ULL << (E1_SQ - 2)) | (1ULL << (E1_SQ - 3));
constexpr uint64_t BK_CASTLE_PATH = (1ULL << (E8_SQ + 1)) | (1ULL << (E8_SQ + 2));
constexpr uint64_t BQ_CASTLE_PATH = (1ULL << (E8_SQ - 1)) | (1ULL << (E8_SQ - 2)) | (1ULL << (E8_SQ - 3));

// Forward Declarations
struct Move;
struct Position;
int evaluate(Position& pos); // Made non-const to update pawn cache stats
bool is_square_attacked(const Position& pos, int sq, int attacker_color);
int generate_moves(const Position& pos, Move* moves_list, bool captures_only);
Position make_move(const Position& pos, const Move& move, bool& legal);
uint64_t calculate_zobrist_hash(const Position& pos);
uint64_t calculate_pawn_zobrist_hash(const Position& pos);
int see(const Position& pos, const Move& move);

// --- Pawn Cache ---
struct PawnCacheEntry {
    uint64_t key = 0;
    PhaseScore score = {};
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
     if (!pawn_evaluation_cache.empty())
        std::memset(pawn_evaluation_cache.data(), 0, pawn_evaluation_cache.size() * sizeof(PawnCacheEntry));
}

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
inline int file_of(int sq) { return sq % 8; }

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
    int static_eval; // Store the last evaluation for dynamic LMR

    Position() {
        std::memset(this, 0, sizeof(Position)); // Efficiently zero out
        side_to_move = WHITE;
        ep_square = -1;
        fullmove_number = 1;
        static_eval = 0;
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }

    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64) return NO_PIECE;
        uint64_t b = set_bit(sq);
        if (!((color_bb[WHITE] | color_bb[BLACK]) & b)) return NO_PIECE; // Optimization
        for (int p = PAWN; p <= KING; ++p)
            if (piece_bb[p] & b) return (Piece)p;
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

// Slower reference functions for attack generation during initialization
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

// Function to initialize the magic bitboard tables
void init_magic_bitboards() {
    for (int sq = 0; sq < 64; ++sq) {
        uint64_t mask = magic_rook_mask[sq];
        int num_mask_bits = pop_count(mask);
        for (int i = 0; i < (1 << num_mask_bits); ++i) {
            uint64_t blockers = 0;
            uint64_t temp_mask = mask;
            for (int j = 0; j < num_mask_bits; ++j) {
                int bit_pos = lsb_index(temp_mask);
                temp_mask &= temp_mask - 1;
                if ((i >> j) & 1)
                    blockers |= set_bit(bit_pos);
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
                int bit_pos = lsb_index(temp_mask);
                temp_mask &= temp_mask - 1;
                if ((i >> j) & 1)
                    blockers |= set_bit(bit_pos);
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
    if (get_rook_attacks(sq_to_check, occupied) & rook_queen_attackers) return true;

    uint64_t bishop_queen_attackers = (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_c];
    if (get_bishop_attacks(sq_to_check, occupied) & bishop_queen_attackers) return true;

    return false;
}

uint64_t get_attackers_to_sq(const Position& pos, int sq_to_check, uint64_t occupied) {
    uint64_t attackers = 0;
    // Note: This gets attackers from BOTH sides. The calling function must filter by color.
    attackers |= pawn_attacks_bb[WHITE][sq_to_check] & pos.piece_bb[PAWN] & pos.color_bb[BLACK];
    attackers |= pawn_attacks_bb[BLACK][sq_to_check] & pos.piece_bb[PAWN] & pos.color_bb[WHITE];
    attackers |= knight_attacks_bb[sq_to_check] & pos.piece_bb[KNIGHT];
    attackers |= king_attacks_bb[sq_to_check] & pos.piece_bb[KING];
    attackers |= get_bishop_attacks(sq_to_check, occupied) & (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]);
    attackers |= get_rook_attacks(sq_to_check, occupied) & (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]);
    return attackers;
}

// --- Move Generation ---
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
            } else if (!captures_only)
                moves_list[move_count++] = {from, one_step_sq};
            // Double pawn push (only if single push is also possible and not captures_only)
            if (!captures_only) {
                int start_rank_idx = (stm == WHITE) ? 1 : 6;
                if (rank == start_rank_idx) {
                    int two_steps_sq = (stm == WHITE) ? from + 16 : from - 16;
                    if (two_steps_sq >=0 && two_steps_sq < 64 && get_bit(empty_squares, two_steps_sq))
                        moves_list[move_count++] = {from, two_steps_sq};
                }
            }
        }
        // Pawn captures (including EP)
        uint64_t pawn_cap_targets = pawn_attacks_bb[stm][from] & opp_pieces;
        if (pos.ep_square != -1)
             if (get_bit(pawn_attacks_bb[stm][from], pos.ep_square)) // Is the EP square a valid capture target?
                 pawn_cap_targets |= set_bit(pos.ep_square);
        while (pawn_cap_targets) {
            int to = lsb_index(pawn_cap_targets);
            pawn_cap_targets &= pawn_cap_targets - 1;
            if (rank == promotion_rank_idx) { // Reached promotion rank by capture
                moves_list[move_count++] = {from, to, QUEEN}; moves_list[move_count++] = {from, to, ROOK};
                moves_list[move_count++] = {from, to, BISHOP}; moves_list[move_count++] = {from, to, KNIGHT};
            } else
                moves_list[move_count++] = {from, to};
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
            else if (p_type == BISHOP) attacks = get_bishop_attacks(from, occupied);
            else if (p_type == ROOK) attacks = get_rook_attacks(from, occupied);
            else if (p_type == QUEEN) attacks = get_queen_attacks(from, occupied);
            else if (p_type == KING) attacks = king_attacks_bb[from];

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
                    (occupied & WK_CASTLE_PATH) == 0 &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ + 1, BLACK) && !is_square_attacked(pos, E1_SQ + 2, BLACK))
                    moves_list[move_count++] = {king_sq_idx, E1_SQ + 2}; // King to G1
                if ((pos.castling_rights & WQ_CASTLE_MASK) && king_sq_idx == E1_SQ &&
                    (occupied & WQ_CASTLE_PATH) == 0 &&
                    !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ - 1, BLACK) && !is_square_attacked(pos, E1_SQ - 2, BLACK))
                    moves_list[move_count++] = {king_sq_idx, E1_SQ - 2}; // King to C1
            } else { // BLACK
                if ((pos.castling_rights & BK_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    (occupied & BK_CASTLE_PATH) == 0 &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ + 1, WHITE) && !is_square_attacked(pos, E8_SQ + 2, WHITE))
                    moves_list[move_count++] = {king_sq_idx, E8_SQ + 2}; // King to G8
                if ((pos.castling_rights & BQ_CASTLE_MASK) && king_sq_idx == E8_SQ &&
                    (occupied & BQ_CASTLE_PATH) == 0 &&
                    !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ - 1, WHITE) && !is_square_attacked(pos, E8_SQ - 2, WHITE))
                    moves_list[move_count++] = {king_sq_idx, E8_SQ - 2}; // King to C8
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
        if (piece_captured == PAWN)
            next_pos.pawn_zobrist_key ^= zobrist_pieces[opp][PAWN][move.to];
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
    if (piece_moved == PAWN && std::abs(move.to - move.from) == 16)
        next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    next_pos.zobrist_hash ^= zobrist_ep[(next_pos.ep_square == -1) ? 64 : next_pos.ep_square];

    if (move.promotion != NO_PIECE) {
        if (piece_moved != PAWN) return pos;
        int promotion_rank_actual = (stm == WHITE) ? 7 : 0;
        if (move.to / 8 != promotion_rank_actual) return pos;
        
        next_pos.piece_bb[move.promotion] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][move.promotion][move.to];
    } else {
        if (piece_moved == PAWN)
             next_pos.pawn_zobrist_key ^= zobrist_pieces[stm][PAWN][move.to];
        next_pos.piece_bb[piece_moved] |= to_bb;
        next_pos.color_bb[stm] |= to_bb;
        next_pos.zobrist_hash ^= zobrist_pieces[stm][piece_moved][move.to];
    }

    uint8_t old_castling_rights = pos.castling_rights;
    uint8_t new_castling_rights = pos.castling_rights;

    if (piece_moved == KING) {
        if (stm == WHITE) new_castling_rights &= ~(WK_CASTLE_MASK | WQ_CASTLE_MASK);
        else new_castling_rights &= ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);

        // Handle castling move itself (rook movement)
        if (std::abs(move.to - move.from) == 2) {
            int rook_from_sq, rook_to_sq;
            if (move.to == E1_SQ + 2) { rook_from_sq = H1_SQ; rook_to_sq = E1_SQ + 1; } // White Kingside
            else if (move.to == E1_SQ - 2) { rook_from_sq = A1_SQ; rook_to_sq = E1_SQ - 1; } // White Queenside
            else if (move.to == E8_SQ + 2) { rook_from_sq = H8_SQ; rook_to_sq = E8_SQ + 1; } // Black Kingside
            else { rook_from_sq = A8_SQ; rook_to_sq = E8_SQ - 1; } // Black Queenside

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
    if (king_sq_after_move == -1) { return pos; }
    if (is_square_attacked(next_pos, king_sq_after_move, opp))
        return pos;

    legal_move_flag = true;
    return next_pos;
}

// --- Evaluation ---
const PhaseScore piece_phase_values[6] = {
    {91, 132}, {373, 398}, {386, 420}, {564, 667}, {1096, 1287}, {0, 0}
};
const int see_piece_values[7] = {100, 320, 330, 500, 900, 10000, 0};

// --- PIECE-SQUARE TABLES ---

const PhaseScore pawn_pst[64] = {
    {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0},
    {  1,  10}, {  2,  10}, {  0,   9}, { -2,   0}, {  0,  11}, {  1,   8}, {  0,  10}, { -1,   4},
    {  0,   2}, { -5,   3}, {  8,  -2}, { 13,   1}, { 20,  -1}, { 13,   0}, {  1,  -3}, { -8,  -1},
    {  3,   9}, { -8,   5}, {  3,   0}, { 12,   3}, { 25,  -3}, { 10,  -3}, { -3,  -1}, {  0,   2},
    {  9,  18}, {  5,  13}, {  0,  11}, {  5,   2}, { 10,   3}, {  5,   4}, { -2,  19}, {  7,  16},
    { 25,  55}, { 25,  58}, { 25,  59}, { 40,  69}, { 30,  70}, { 25,  54}, { 20,  53}, { 20,  57},
    { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160},
    {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}
};
const PhaseScore knight_pst[64] = {
    {-85, -70}, {-50, -45}, {-40, -35}, {-40, -25}, {-40, -25}, {-40, -35}, {-50, -45}, {-85, -70},
    {-45, -45}, {-20, -30}, { -5, -10}, {  0,  10}, {  0,  10}, { -5, -10}, {-20, -30}, {-45, -45},
    {-40, -30}, { -5, -10}, { 10,   5}, { 15,  25}, { 15,  25}, { 10,   5}, { -5, -10}, {-40, -30},
    {-35, -30}, {  5,   5}, { 20,  20}, { 25,  35}, { 25,  35}, { 20,  20}, {  5,   5}, {-35, -30},
    {-35, -35}, { 10,   0}, { 22,  20}, { 25,  35}, { 25,  35}, { 22,  20}, { 10,   0}, {-35, -35},
    {-40, -40}, { 10, -15}, { 30,   0}, { 25,  20}, { 25,  20}, { 30,   0}, { 10, -15}, {-40, -40},
    {-50, -50}, {-30, -30}, {  0, -20}, {  10,  0}, {  10,  0}, {  0, -20}, {-30, -30}, {-50, -50},
    {-90, -75}, {-50, -50}, {-40, -40}, {-30, -20}, {-30, -20}, {-40, -40}, {-50, -50}, {-90, -75}
};
const PhaseScore bishop_pst[64] = {
    {-28, -25}, {-10, -15}, {-10, -15}, {-10, -10}, {-10, -10}, {-10, -15}, {-10, -15}, {-28, -25},
    {-10, -15}, {  5,  -5}, {  0,  -5}, {  3,   0}, {  3,   0}, {  0,  -5}, {  5,  -5}, {-10, -15},
    {-10, -10}, {  5,   0}, {  0,   5}, { 10,  10}, { 10,  10}, {  0,   5}, {  5,   0}, {-10, -10},
    {-10, -10}, {  5,   0}, {  8,   5}, { 15,  15}, { 15,  15}, {  8,   5}, {  5,   0}, {-10, -10},
    {-10, -10}, { 10,   0}, { 10,   0}, { 15,  15}, { 15,  15}, { 10,   0}, { 10,   0}, {-10, -10},
    {-10, -15}, {  5,   5}, {  5,   5}, { 10,  10}, { 10,  10}, {  5,   5}, {  5,   5}, {-10, -15},
    {-10, -15}, {  0, -10}, {  5,  -5}, {  0,   0}, {  0,   0}, {  5,  -5}, {  0, -10}, {-10, -15},
    {-28, -25}, {-10, -20}, {-10, -15}, {-10, -10}, {-10, -10}, {-10, -15}, {-10, -20}, {-28, -25}
};
const PhaseScore rook_pst[64] = {
    {-15,   0}, {-10,   0}, { -5,   0}, { -2,   0}, { -2,   0}, { -5,   0}, {-10,   0}, {-15,   0},
    {-12,  -5}, { -5,  -5}, { -2,  -2}, {  5,  -2}, {  5,  -2}, { -2,  -2}, { -5,  -5}, {-12,  -5},
    {-12,  -5}, { -5,  -5}, { -2,  -2}, {  5,  -2}, {  5,  -2}, { -2,  -2}, { -5,  -5}, {-12,  -5},
    {-10,  -5}, { -5,   0}, { -2,   0}, {  0,   0}, {  0,   0}, { -2,   0}, { -5,   0}, {-10,  -5},
    {-15,  -5}, { -5,   2}, { -2,   2}, {  0,   0}, {  0,   0}, { -2,   2}, { -5,   2}, {-15,  -5},
    {-12,   0}, { -2,   0}, {  5,   0}, { 10,   5}, { 10,   5}, {  5,   0}, { -2,   0}, {-12,   0},
    { -1,  10}, {  5,  10}, {  8,  15}, { 12,  10}, { 12,  10}, {  8,  15}, {  5,  10}, { -1,  10},
    {-10,  10}, {-10,   5}, {  0,  10}, {  5,  10}, {  5,  10}, {  0,  10}, {-10,   5}, {-10,  10}
};
const PhaseScore queen_pst[64] = {
    { -10, -40}, {  -5, -30}, {  -5, -25}, {   0, -15}, {   0, -15}, {  -5, -25}, {  -5, -30}, { -10, -40},
    {  -5, -30}, {   0, -15}, {   2, -10}, {   5,  -2}, {   5,  -2}, {   2, -10}, {   0, -15}, {  -5, -30},
    {  -5, -20}, {   0, -10}, {   5,  -5}, {   5,   0}, {   5,   0}, {   5,  -5}, {   0, -10}, {  -5, -20},
    {   0, -12}, {   2,  -2}, {   5,   5}, {   5,  10}, {   5,  10}, {   5,   5}, {   2,  -2}, {   0, -12},
    {  -2, -15}, {   5,  -5}, {   5,   5}, {   5,  10}, {   5,  10}, {   5,   5}, {   5,  -5}, {  -2, -15},
    {  -5, -20}, {   5, -10}, {   2,  -5}, {   5,   0}, {   5,   0}, {   2,  -5}, {   5, -10}, {  -5, -20},
    {  -5, -30}, {   0, -15}, {   5, -12}, {   5, -10}, {   5, -10}, {   5, -12}, {   0, -15}, {  -5, -30},
    { -10, -45}, { -10, -30}, {   0, -25}, {  -2, -20}, {  -2, -20}, {   0, -25}, { -10, -30}, { -10, -45}
};
const PhaseScore king_pst[64] = {
    { 150,  -1}, { 180,  20}, { 150,  50}, { 100,  45}, { 100,  45}, { 150,  50}, { 180,  20}, { 150,  -1},
    { 150,  25}, { 165,  60}, { 130,  80}, { 100,  80}, { 100,  80}, { 130,  80}, { 165,  60}, { 150,  25},
    { 100,  50}, { 140,  80}, {  90, 110}, {  70, 120}, {  70, 120}, {  90, 110}, { 140,  80}, { 100,  50},
    {  90,  60}, { 110, 100}, {  80, 120}, {  50, 120}, {  50, 120}, {  80, 120}, { 110, 100}, {  90,  60},
    {  80,  55}, { 100, 110}, {  60, 140}, {  40, 140}, {  40, 140}, {  60, 140}, { 100, 110}, {  80,  55},
    {  70,  50}, {  80, 120}, {  40, 130}, {  10, 140}, {  10, 140}, {  40, 130}, {  80, 120}, {  70,  50},
    {  50,  20}, {  70,  70}, {  40,  70}, {  15,  80}, {  15,  80}, {  40,  70}, {  70,  70}, {  50,  20},
    {  30,   0}, {  50,  30}, {  20,  40}, {   0,  45}, {   0,  45}, {  20,  40}, {  50,  30}, {  30,   0}
};

const PhaseScore* pst_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst};
const int game_phase_inc[6] = {0, 1, 1, 2, 4, 0}; // P,N,B,R,Q,K

// Evaluation helper masks
uint64_t FILE_BB_MASK[8];
uint64_t RANK_BB_MASK[8];
uint64_t WHITE_PASSED_PAWN_MASK[64];
uint64_t BLACK_PASSED_PAWN_MASK[64];
uint64_t ADJACENT_FILES_MASK[8];
uint64_t PAWN_SHIELD_MASK[2][64]; // [color][square]
constexpr uint64_t LIGHT_SQUARES = 0x55AA55AA55AA55AAULL;

const PhaseScore PASSED_PAWN_BONUS_PS[8] = {
    {0, 0}, {5, 12}, {15, 28}, {25, 42}, {40, 65}, {60, 95}, {80, 125}, {0, 0}
};

// --- Evaluation Constants ---
const PhaseScore TEMPO_BONUS_PS                   = {15, 15};
const PhaseScore BISHOP_PAIR_BONUS_PS             = {27, 72};
const PhaseScore CONNECTED_PAWN_BONUS_PS          = {9, 15};
const PhaseScore SUPPORTED_PAWN_BONUS_PS          = {8, 13};
const PhaseScore ISOLATED_PAWN_PENALTY         = {-14, -22};
const PhaseScore DOUBLED_PAWN_PENALTY          = {-12, -18};
const PhaseScore BACKWARD_PAWN_PENALTY         = {-9, -14};
const PhaseScore KNIGHT_MOBILITY_BONUS_PS         = {2, 3};
const PhaseScore BISHOP_MOBILITY_BONUS_PS         = {3, 4};
const PhaseScore ROOK_MOBILITY_BONUS_PS           = {3, 5};
const PhaseScore QUEEN_MOBILITY_BONUS_PS          = {2, 3};
const PhaseScore KNIGHT_OUTPOST_BONUS_PS          = {30, 20};
const PhaseScore BISHOP_OUTPOST_BONUS_PS          = {25, 18};
const PhaseScore IMPENDING_OUTPOST_BONUS_PS     = {6, 4};
const PhaseScore ROOK_ON_OPEN_FILE_PS             = {28, 12};
const PhaseScore ROOK_ON_SEMI_OPEN_FILE_PS        = {11, 8};
const PhaseScore ROOK_ON_7TH_RANK_BONUS_PS        = {12, 39};
const int PASSED_PAWN_KING_DISTANCE_BONUS_EG = 4; // bonus per square of Chebyshev distance in endgame
const PhaseScore THREAT_BY_MINOR_PS[7] = {
    {0,0}, {1,9}, {16,12}, {20,14}, {25,32}, {20,40}, {0,0} // Pawn, Knight, Bishop, Rook, Queen, King
};
const PhaseScore THREAT_BY_ROOK_PS[7] = {
    {0,0}, {0,11}, {9,17}, {11,14}, {0,9}, {15,9}, {0,0}
};
const PhaseScore THREAT_BY_KING_PS = {6, 21};
const PhaseScore HANGING_PIECE_BONUS_PS = {18, 10};
const PhaseScore ATTACK_ON_QUEEN_DEFENDED_PIECE_PS = {3, 0};
const PhaseScore SQUARE_CONTROL_BONUS_PS = {1, 1};
const PhaseScore SAFE_PAWN_ATTACK_BONUS_PS = {41, 24};
const PhaseScore PAWN_PUSH_THREAT_BONUS_PS = {12, 9};

// --- NEW EVALUATION CONSTANTS ---
const PhaseScore BAD_BISHOP_PENALTY_PS             = {-4, -8};
const PhaseScore PAWN_STORM_THREAT_PENALTY_PS      = {-6, -11};
const PhaseScore KNIGHT_IN_CLOSED_POSITION_BONUS_PS = {1, 1};
const PhaseScore ROOK_IN_CLOSED_POSITION_PENALTY_PS = {1, 0};


// --- Evaluation Constants for King Safety ---
const int KING_PROXIMITY_BONUS[8] = { 0, 1, 2, 4, 8, 12, 15, 20 }; // Reversed for easier use (dist 0 = max bonus)

// Weights for different attacking pieces
constexpr int KING_ATTACK_WEIGHT_QUEEN = 5;
constexpr int KING_ATTACK_WEIGHT_ROOK = 3;
constexpr int KING_ATTACK_WEIGHT_BISHOP = 2;
constexpr int KING_ATTACK_WEIGHT_KNIGHT = 2;

// King Threat Penalty Table (indexed by combined weighted tropism score)
const int KING_DANGER_PENALTY_MG[31] = {
    0,  0,  3,  7, 15, 24, 34, 45, 57, 69, 83, 98, 113, 128, 148,
  168,188,208,228,248,268,288,308,328,348,368, 388, 408, 428, 448, 468
};
const int KING_DANGER_PENALTY_EG[31] = {
    0,  0,  2,  4,  8, 12, 16, 20, 25, 30, 35,  40,  45,  50,  55,
   60, 65, 70, 75, 80, 85, 90, 95,100,105,110, 115, 120, 125, 130, 135
};

// King Shelter Penalties (Middlegame only)
const int PAWN_SHIELD_BONUS = 10;
const int MISSING_PAWN_SHIELD_PENALTY = -20;
const int ADVANCED_PAWN_SHIELD_PENALTY = -12;
const int OPEN_FILE_NEAR_KING_PENALTY = -15;

void init_eval_masks() {
    for (int f = 0; f < 8; ++f) {
        FILE_BB_MASK[f] = 0ULL;
        for (int r = 0; r < 8; ++r) FILE_BB_MASK[f] |= set_bit(r * 8 + f);
        ADJACENT_FILES_MASK[f] = 0ULL;
        if (f > 0) ADJACENT_FILES_MASK[f] |= FILE_BB_MASK[f-1];
        if (f < 7) ADJACENT_FILES_MASK[f] |= FILE_BB_MASK[f+1];
    }
    for (int r = 0; r < 8; ++r) {
        RANK_BB_MASK[r] = 0ULL;
        for (int f = 0; f < 8; ++f) RANK_BB_MASK[r] |= set_bit(r * 8 + f);
    }

    for (int sq = 0; sq < 64; ++sq) {
        WHITE_PASSED_PAWN_MASK[sq] = 0ULL;
        BLACK_PASSED_PAWN_MASK[sq] = 0ULL;
        PAWN_SHIELD_MASK[WHITE][sq] = 0ULL;
        PAWN_SHIELD_MASK[BLACK][sq] = 0ULL;

        int r = sq / 8, f = sq % 8;
        for (int cur_r = r + 1; cur_r < 8; ++cur_r) { // Squares in front for White
            WHITE_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) WHITE_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) WHITE_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + (f + 1));
        }
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) { // Squares in front for Black
            BLACK_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + f);
            if (f > 0) BLACK_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + (f - 1));
            if (f < 7) BLACK_PASSED_PAWN_MASK[sq] |= set_bit(cur_r * 8 + (f + 1));
        }

        // Outpost defense masks: Squares on adjacent files "behind" the outpost square from enemy's view.
        // This is equivalent to the squares on adjacent files that are part of the *enemy's* passed pawn blocking mask for that square.
        PAWN_SHIELD_MASK[WHITE][sq] = BLACK_PASSED_PAWN_MASK[sq] & ADJACENT_FILES_MASK[f];
        PAWN_SHIELD_MASK[BLACK][sq] = WHITE_PASSED_PAWN_MASK[sq] & ADJACENT_FILES_MASK[f];
    }
}

// Checks for draw by insufficient material.
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

// Helper function to evaluate pawn structure for one color
void evaluate_pawns_for_color(const Position& pos, Color current_eval_color,
                                       PhaseScore& pawn_score,
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

        if ((ADJACENT_FILES_MASK[f] & all_friendly_pawns) == 0) {
            pawn_score += ISOLATED_PAWN_PENALTY;
        }

        if (pawn_attacks_bb[1 - current_eval_color][sq] & all_friendly_pawns) {
            pawn_score += SUPPORTED_PAWN_BONUS_PS;
        }

        if (get_bit(east(set_bit(sq)), all_friendly_pawns)) {
            pawn_score += CONNECTED_PAWN_BONUS_PS;
        }
        
        uint64_t forward_file_squares = (current_eval_color == WHITE) ? north(set_bit(sq)) : south(set_bit(sq));
        if ((FILE_BB_MASK[f] & forward_file_squares & all_friendly_pawns) != 0) {
            pawn_score += DOUBLED_PAWN_PENALTY;
        }
        
        uint64_t front_span = (current_eval_color == WHITE) ? WHITE_PASSED_PAWN_MASK[sq] : BLACK_PASSED_PAWN_MASK[sq];
        uint64_t adjacent_pawns = ADJACENT_FILES_MASK[f] & all_friendly_pawns;
        if ((front_span & adjacent_pawns) == 0) { // No pawns on adjacent files ahead of us
            int push_sq = (current_eval_color == WHITE) ? sq + 8 : sq - 8;
            if (get_bit(enemy_pawn_attacks, push_sq)) {
                pawn_score += BACKWARD_PAWN_PENALTY;
            }
        }

        bool is_passed = false;
        if (current_eval_color == WHITE) {
            if ((WHITE_PASSED_PAWN_MASK[sq] & all_enemy_pawns) == 0) is_passed = true;
        } else {
            if ((BLACK_PASSED_PAWN_MASK[sq] & all_enemy_pawns) == 0) is_passed = true;
        }
        if (is_passed) {
            passed_pawns |= set_bit(sq);
            int rank_from_own_side = (current_eval_color == WHITE) ? (sq / 8) : (7 - (sq / 8));
            pawn_score += PASSED_PAWN_BONUS_PS[rank_from_own_side];
        }
    }
}

// Scales the evaluation in the endgame to account for drawish tendencies.
int get_endgame_drawishness_scale_factor(const Position& pos, const PhaseScore& score) {
    if ((pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) != 0) {
         int white_pieces = pop_count(pos.color_bb[WHITE]);
         int black_pieces = pop_count(pos.color_bb[BLACK]);
         if (white_pieces > 3 && black_pieces > 3) return 256;
    }
    
    int white_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]);
    int black_bishops = pop_count(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]);
    
    if (white_bishops == 1 && black_bishops == 1) {
        uint64_t w_b_bb = pos.piece_bb[BISHOP] & pos.color_bb[WHITE];
        uint64_t b_b_bb = pos.piece_bb[BISHOP] & pos.color_bb[BLACK];
        bool w_b_is_light = get_bit(LIGHT_SQUARES, lsb_index(w_b_bb));
        bool b_b_is_light = get_bit(LIGHT_SQUARES, lsb_index(b_b_bb));

        if (w_b_is_light != b_b_is_light) {
            // Significant draw factor for OCB endgames
            return 140;
        }
    }

    int stronger_side = (score.mg + score.eg > 0) ? WHITE : BLACK;
    int weaker_side = 1 - stronger_side;
    
    uint64_t stronger_side_pawns = pos.piece_bb[PAWN] & pos.color_bb[stronger_side];
    int pawn_advantage = pop_count(stronger_side_pawns) - pop_count(pos.piece_bb[PAWN] & pos.color_bb[weaker_side]);
    
    if ((pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) == 0 && pawn_advantage <= 1) {
        return 160;
    }

    int num_stronger_pawns = pop_count(stronger_side_pawns);
    return std::min(256, 192 + num_stronger_pawns * 20);
}

// Calculates bonuses for various types of threats a side can create.
PhaseScore evaluate_threats_for_color(const Position& pos, Color us, const uint64_t attackedBy[2][7], const uint64_t attackedBy2[2]) {
    PhaseScore score = {};
    const Color them = (Color)(1 - us);
    uint64_t non_pawn_enemies = pos.color_bb[them] & ~pos.piece_bb[PAWN];
    uint64_t strongly_protected_by_them = attackedBy[them][PAWN] | (attackedBy2[them] & ~attackedBy2[us]);
    uint64_t weak_enemy_pieces = pos.color_bb[them] & ~strongly_protected_by_them & attackedBy[us][6]; // 6 is ALL_PIECES
    uint64_t defended_enemy_pieces = non_pawn_enemies & strongly_protected_by_them;
    uint64_t all_targets = weak_enemy_pieces | defended_enemy_pieces;

    if (!all_targets) return score; // Early exit if no pieces are targeted

    uint64_t b = all_targets & (attackedBy[us][KNIGHT] | attackedBy[us][BISHOP]);
    while(b) {
        int sq = lsb_index(b);
        b &= b - 1;
        score += THREAT_BY_MINOR_PS[pos.piece_on_sq(sq)];
    }

    b = weak_enemy_pieces & attackedBy[us][ROOK];
    while(b) {
        int sq = lsb_index(b);
        b &= b - 1;
        score += THREAT_BY_ROOK_PS[pos.piece_on_sq(sq)];
    }

    if (weak_enemy_pieces & attackedBy[us][KING]) {
        score += THREAT_BY_KING_PS;
    }

    uint64_t hanging_targets = ~attackedBy[them][6] | (non_pawn_enemies & attackedBy2[us]);
    score += HANGING_PIECE_BONUS_PS * pop_count(weak_enemy_pieces & hanging_targets);
    
    score += ATTACK_ON_QUEEN_DEFENDED_PIECE_PS * pop_count(weak_enemy_pieces & attackedBy[them][QUEEN]);

    uint64_t restricted_squares = attackedBy[them][6] & ~strongly_protected_by_them & attackedBy[us][6]; // 6 is ALL_PIECES
    score += SQUARE_CONTROL_BONUS_PS * pop_count(restricted_squares);

    uint64_t safe_squares_for_us = ~attackedBy[them][6] | attackedBy[us][6];
    uint64_t safe_pawns      = pos.piece_bb[PAWN] & pos.color_bb[us] & safe_squares_for_us;
    uint64_t pawn_threats    = (us == WHITE) ? (ne(safe_pawns) | nw(safe_pawns)) : (se(safe_pawns) | sw(safe_pawns));
    score                   += SAFE_PAWN_ATTACK_BONUS_PS * pop_count(pawn_threats & non_pawn_enemies);

    uint64_t push_targets;
    if (us == WHITE)
    {
        push_targets = north(pos.piece_bb[PAWN] & pos.color_bb[WHITE]) & ~pos.get_occupied_bb();
        push_targets |= north(push_targets & RANK_BB_MASK[2]) & ~pos.get_occupied_bb(); // Rank 3
    }
    else
    {
        push_targets = south(pos.piece_bb[PAWN] & pos.color_bb[BLACK]) & ~pos.get_occupied_bb();
        push_targets |= south(push_targets & RANK_BB_MASK[5]) & ~pos.get_occupied_bb(); // Rank 6
    }
    push_targets          &= ~attackedBy[them][PAWN] & safe_squares_for_us;
    uint64_t push_threats = (us == WHITE) ? (ne(push_targets) | nw(push_targets)) : (se(push_targets) | sw(push_targets));
    score                 += PAWN_PUSH_THREAT_BONUS_PS * pop_count(push_threats & non_pawn_enemies);

    return score;
}

int evaluate(Position& pos) {
    if (is_insufficient_material(pos)) return 0; 

    PhaseScore final_score;
    int game_phase = 0;
    
    // --- Pawn Evaluation (with Caching) ---
    PhaseScore pawn_score;
    uint64_t white_passed_pawns = 0;
    uint64_t black_passed_pawns = 0;

    PawnCacheEntry& pawn_entry = pawn_evaluation_cache[pos.pawn_zobrist_key & pawn_cache_mask];
    if (pawn_entry.key == pos.pawn_zobrist_key) {
        pawn_score = pawn_entry.score;
        white_passed_pawns = pawn_entry.white_passed_pawns;
        black_passed_pawns = pawn_entry.black_passed_pawns;
    } else {
        PhaseScore white_pawn_score, black_pawn_score;
        evaluate_pawns_for_color(pos, WHITE, white_pawn_score, white_passed_pawns);
        evaluate_pawns_for_color(pos, BLACK, black_pawn_score, black_passed_pawns);
        
        pawn_score = white_pawn_score - black_pawn_score;
        pawn_entry.key = pos.pawn_zobrist_key;
        pawn_entry.score = pawn_score;
        pawn_entry.white_passed_pawns = white_passed_pawns;
        pawn_entry.black_passed_pawns = black_passed_pawns;
    }
    final_score += pawn_score;

    uint64_t occupied = pos.get_occupied_bb();
    uint64_t attackedBy[2][7] = {{0}};
    uint64_t attackedBy2[2] = {0};

    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color c = (Color)c_idx;
        uint64_t temp_pawns = pos.piece_bb[PAWN] & pos.color_bb[c];
        while (temp_pawns) {
            int sq = lsb_index(temp_pawns);
            temp_pawns &= temp_pawns - 1;
            attackedBy[c][PAWN] |= pawn_attacks_bb[c][sq];
        }
        int king_sq = lsb_index(pos.piece_bb[KING] & pos.color_bb[c]);
        if (king_sq != -1) attackedBy[c][KING] = king_attacks_bb[king_sq];

        attackedBy[c][6] = attackedBy[c][PAWN] | attackedBy[c][KING];
        attackedBy2[c] = attackedBy[c][PAWN] & attackedBy[c][KING];

        Piece piece_types[] = {KNIGHT, BISHOP, ROOK, QUEEN};
        for (Piece p_type : piece_types) {
            uint64_t pieces = pos.piece_bb[p_type] & pos.color_bb[c];
            while (pieces) {
                int sq = lsb_index(pieces);
                pieces &= pieces - 1;
                uint64_t attacks = 0;
                if (p_type == KNIGHT) attacks = knight_attacks_bb[sq];
                else if (p_type == BISHOP) attacks = get_bishop_attacks(sq, occupied);
                else if (p_type == ROOK) attacks = get_rook_attacks(sq, occupied);
                else if (p_type == QUEEN) attacks = get_queen_attacks(sq, occupied);
                
                attackedBy[c][p_type] |= attacks;
                attackedBy2[c] |= attackedBy[c][6] & attacks;
                attackedBy[c][6] |= attacks;
            }
        }
    }
    
    for (int c_idx = 0; c_idx < 2; ++c_idx) {
        Color current_eval_color = (Color)c_idx;
        Color enemy_color = (Color)(1-c_idx);
        PhaseScore current_color_score;
        
        uint64_t friendly_pieces = pos.color_bb[current_eval_color];
        uint64_t enemy_pieces = pos.color_bb[enemy_color];
        uint64_t enemy_pawn_attacks = attackedBy[enemy_color][PAWN];
        uint64_t safe_mobility_area = ~(friendly_pieces | enemy_pawn_attacks);
        uint64_t all_friendly_pawns = pos.piece_bb[PAWN] & friendly_pieces;
        uint64_t all_enemy_pawns = pos.piece_bb[PAWN] & enemy_pieces;
        
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[current_eval_color];
            game_phase += pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = lsb_index(b);
                b &= b - 1;
                int mirrored_sq = (current_eval_color == WHITE) ? sq : (63 - sq);

                current_color_score += piece_phase_values[p];
                current_color_score += pst_all[p][mirrored_sq];

                if ((Piece)p == PAWN) {
                    uint64_t current_passed_pawns = (current_eval_color == WHITE) ? white_passed_pawns : black_passed_pawns;
                    if (get_bit(current_passed_pawns, sq)) {
                        int enemy_king_sq = lsb_index(pos.piece_bb[KING] & enemy_pieces);
                        if (enemy_king_sq != -1) {
                            int pawn_rank = sq / 8; int pawn_file = sq % 8;
                            int king_rank = enemy_king_sq / 8; int king_file = enemy_king_sq % 8;
                            int dist_to_enemy_king = std::max(std::abs(pawn_rank - king_rank), std::abs(pawn_file - king_file));
                            current_color_score.eg += dist_to_enemy_king * PASSED_PAWN_KING_DISTANCE_BONUS_EG;
                        }
                    }
                } else if ((Piece)p != KING) {
                    uint64_t mobility_attacks = 0;
                    if ((Piece)p == KNIGHT) mobility_attacks = knight_attacks_bb[sq];
                    else if ((Piece)p == BISHOP) mobility_attacks = get_bishop_attacks(sq, occupied);
                    else if ((Piece)p == ROOK) mobility_attacks = get_rook_attacks(sq, occupied);
                    else if ((Piece)p == QUEEN) mobility_attacks = get_queen_attacks(sq, occupied);
                    
                    int mobility_count = pop_count(mobility_attacks & safe_mobility_area);
                    PhaseScore mobility_score = {};
                    if ((Piece)p == KNIGHT) mobility_score = KNIGHT_MOBILITY_BONUS_PS;
                    else if ((Piece)p == BISHOP) mobility_score = BISHOP_MOBILITY_BONUS_PS;
                    else if ((Piece)p == ROOK) mobility_score = ROOK_MOBILITY_BONUS_PS;
                    else if ((Piece)p == QUEEN) mobility_score = QUEEN_MOBILITY_BONUS_PS;
                    
                    mobility_score *= mobility_count;
                    current_color_score += mobility_score;
                }

                if ((Piece)p == BISHOP) {
                    uint64_t blocked_friendly_pawns = (current_eval_color == WHITE) ? south(all_enemy_pawns) & all_friendly_pawns : north(all_enemy_pawns) & all_friendly_pawns;
                    uint64_t bishop_color_squares = get_bit(LIGHT_SQUARES, sq) ? LIGHT_SQUARES : ~LIGHT_SQUARES;
                    int rammed_pawn_count = pop_count(blocked_friendly_pawns & bishop_color_squares);
                    current_color_score += BAD_BISHOP_PENALTY_PS * rammed_pawn_count;
                }
                else if ((Piece)p == ROOK) {
                    int f = sq % 8;
                    bool friendly_pawn_on_file = (FILE_BB_MASK[f] & all_friendly_pawns) != 0;
                    bool enemy_pawn_on_file = (FILE_BB_MASK[f] & all_enemy_pawns) != 0;
                    if (!friendly_pawn_on_file) {
                        if (!enemy_pawn_on_file) current_color_score += ROOK_ON_OPEN_FILE_PS; 
                        else current_color_score += ROOK_ON_SEMI_OPEN_FILE_PS;
                    }
                    int relative_rank = (current_eval_color == WHITE) ? (sq / 8) : (7 - (sq / 8));
                    if (relative_rank == 6) current_color_score += ROOK_ON_7TH_RANK_BONUS_PS;
                }
                else if ((Piece)p == KNIGHT || (Piece)p == BISHOP) {
                    int rank = sq / 8;
                    int relative_rank_idx = (current_eval_color == WHITE) ? rank : 7 - rank;
                    if (relative_rank_idx >= 3 && relative_rank_idx <= 5) { // Ranks 4, 5, 6
                        bool is_supported_by_pawn = get_bit(pawn_attacks_bb[enemy_color][sq], all_friendly_pawns);
                        bool is_defended_by_enemy_pawns = (PAWN_SHIELD_MASK[current_eval_color][sq] & all_enemy_pawns) != 0;
                        if (is_supported_by_pawn && !is_defended_by_enemy_pawns) {
                            current_color_score += ((Piece)p == KNIGHT ? KNIGHT_OUTPOST_BONUS_PS : BISHOP_OUTPOST_BONUS_PS);
                        }
                    }
                    if ((Piece)p == KNIGHT) {
                        uint64_t potential_outpost_moves = knight_attacks_bb[sq] & ~occupied;
                        while(potential_outpost_moves) {
                            int to_sq = lsb_index(potential_outpost_moves);
                            potential_outpost_moves &= potential_outpost_moves-1;
                            int to_rank = to_sq/8;
                            int to_relative_rank = (current_eval_color == WHITE) ? to_rank : 7-to_rank;
                            if (to_relative_rank >= 3 && to_relative_rank <=5) {
                                if (get_bit(pawn_attacks_bb[enemy_color][to_sq], all_friendly_pawns) &&
                                    (PAWN_SHIELD_MASK[current_eval_color][to_sq] & all_enemy_pawns) == 0) {
                                    current_color_score += IMPENDING_OUTPOST_BONUS_PS;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        if (pop_count(pos.piece_bb[BISHOP] & pos.color_bb[current_eval_color]) >= 2) {
            current_color_score += BISHOP_PAIR_BONUS_PS; 
        }

        current_color_score += evaluate_threats_for_color(pos, current_eval_color, attackedBy, attackedBy2);

        int king_sq = lsb_index(pos.piece_bb[KING] & friendly_pieces);
        if (king_sq != -1) {
            int shelter_score = 0;
            int king_file = king_sq % 8, king_rank = king_sq / 8;
            int shield_f_start = std::max(0, king_file - 1);
            int shield_f_end = std::min(7, king_file + 1);
            int shield_rank_1 = (current_eval_color == WHITE) ? king_rank + 1 : king_rank - 1;
            int shield_rank_2 = (current_eval_color == WHITE) ? king_rank + 2 : king_rank - 2;

            if (shield_rank_1 >= 0 && shield_rank_1 < 8) {
                for (int f = shield_f_start; f <= shield_f_end; ++f) {
                    uint64_t friendly_pawns_on_file = FILE_BB_MASK[f] & all_friendly_pawns;
                    if (!friendly_pawns_on_file) {
                        shelter_score += MISSING_PAWN_SHIELD_PENALTY;
                        if (!(FILE_BB_MASK[f] & all_enemy_pawns))
                            shelter_score += OPEN_FILE_NEAR_KING_PENALTY;
                    } else {
                        bool pawn_on_shield_rank_1 = get_bit(friendly_pawns_on_file, shield_rank_1 * 8 + f);
                        bool pawn_on_shield_rank_2 = (shield_rank_2 >= 0 && shield_rank_2 < 8) ? get_bit(friendly_pawns_on_file, shield_rank_2 * 8 + f) : false;
                        if (pawn_on_shield_rank_1) shelter_score += PAWN_SHIELD_BONUS;
                        else if (pawn_on_shield_rank_2) shelter_score += ADVANCED_PAWN_SHIELD_PENALTY;
                        else shelter_score += MISSING_PAWN_SHIELD_PENALTY;
                    }
                }
            }

            int home_rank = (current_eval_color == WHITE) ? 0 : 7;
            if (king_rank == home_rank || (king_rank == 0 && (king_file == 6 || king_file == 2)) || (king_rank == 7 && (king_file == 62 || king_file == 58)))
                current_color_score.mg += shelter_score;
            
            uint64_t storming_pawns = all_enemy_pawns & (ADJACENT_FILES_MASK[king_file] | FILE_BB_MASK[king_file]);
            while (storming_pawns) {
                int pawn_sq = lsb_index(storming_pawns);
                storming_pawns &= storming_pawns - 1;
                int relative_rank = (enemy_color == WHITE) ? (pawn_sq / 8) : (7 - (pawn_sq / 8));
                if (relative_rank >= 4) { // Pawns on 5th rank or further are a threat
                    current_color_score += PAWN_STORM_THREAT_PENALTY_PS;
                }
            }

            int king_danger_value = 0;
            auto calculate_tropism = [&](int p_sq) {
                return KING_PROXIMITY_BONUS[7 - std::max(std::abs(king_rank - p_sq/8), std::abs(king_file - p_sq%8))];
            };

            uint64_t enemy_knights = pos.piece_bb[KNIGHT] & enemy_pieces;
            while (enemy_knights) { int sq = lsb_index(enemy_knights); enemy_knights &= enemy_knights - 1; king_danger_value += calculate_tropism(sq) * KING_ATTACK_WEIGHT_KNIGHT; }
            uint64_t enemy_bishops = pos.piece_bb[BISHOP] & enemy_pieces;
            while (enemy_bishops) { int sq = lsb_index(enemy_bishops); enemy_bishops &= enemy_bishops - 1; king_danger_value += calculate_tropism(sq) * KING_ATTACK_WEIGHT_BISHOP; }
            uint64_t enemy_rooks = pos.piece_bb[ROOK] & enemy_pieces;
            while (enemy_rooks) { int sq = lsb_index(enemy_rooks); enemy_rooks &= enemy_rooks - 1; king_danger_value += calculate_tropism(sq) * KING_ATTACK_WEIGHT_ROOK; }
            uint64_t enemy_queens = pos.piece_bb[QUEEN] & enemy_pieces;
            while (enemy_queens) { int sq = lsb_index(enemy_queens); enemy_queens &= enemy_queens - 1; king_danger_value += calculate_tropism(sq) * KING_ATTACK_WEIGHT_QUEEN; }

            int danger_idx = std::min(king_danger_value / 25, 30);
            current_color_score.mg -= KING_DANGER_PENALTY_MG[danger_idx];
            current_color_score.eg -= KING_DANGER_PENALTY_EG[danger_idx];
        }

        if (current_eval_color == WHITE)
            final_score += current_color_score;
        else
            final_score -= current_color_score;
    }

    // --- Positional Theme Adjustments based on pawn structure ---
    int total_pawns = pop_count(pos.piece_bb[PAWN]);
    int white_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[WHITE]);
    int black_knights = pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[BLACK]);
    int white_rooks = pop_count(pos.piece_bb[ROOK] & pos.color_bb[WHITE]);
    int black_rooks = pop_count(pos.piece_bb[ROOK] & pos.color_bb[BLACK]);

    // Knights prefer closed positions (more pawns), rooks prefer open ones (fewer pawns)
    int pawn_density_score = total_pawns - 16; // ranges from 0 (open) to -16 (closed)
    final_score += KNIGHT_IN_CLOSED_POSITION_BONUS_PS * (white_knights - black_knights) * -pawn_density_score;
    final_score += ROOK_IN_CLOSED_POSITION_PENALTY_PS * (white_rooks - black_rooks) * pawn_density_score;


    final_score += (pos.side_to_move == WHITE ? TEMPO_BONUS_PS : -TEMPO_BONUS_PS);

    game_phase = std::min(game_phase, 24);
    game_phase = std::max(game_phase, 0);

    // Apply endgame material scaling factor
    if (game_phase < 16) { // Only apply in clear endgames
        int scale_factor = get_endgame_drawishness_scale_factor(pos, final_score);
        final_score.eg = (final_score.eg * scale_factor) / 256;
    }

    int final_eval = (final_score.mg * game_phase + final_score.eg * (24 - game_phase)) / 24;
    pos.static_eval = (pos.side_to_move == WHITE) ? final_eval : -final_eval;
    return pos.static_eval;
}

// --- Transposition Table ---
enum TTBound { TT_EXACT, TT_LOWER, TT_UPPER, TT_NONE };
struct TTEntry {
    uint64_t hash = 0;
    Move best_move = NULL_MOVE;
    int score = 0;
    int depth = 0;
    int static_eval = 0;
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
    while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries)
        power_of_2_entries *= 2;

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
    if (!transposition_table.empty())
        std::memset(transposition_table.data(), 0, transposition_table.size() * sizeof(TTEntry));
}

bool probe_tt(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move_from_tt, int& score_from_tt, int& eval_from_tt) {
    if (tt_mask == 0 || !g_tt_is_initialized) return false;
    TTEntry& entry = transposition_table[hash & tt_mask];

    if (entry.hash == hash && entry.bound != TT_NONE) {
        move_from_tt = entry.best_move;
        eval_from_tt = entry.static_eval;
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

void store_tt(uint64_t hash, int depth, int ply, int score, TTBound bound, const Move& best_move, int static_eval) {
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
        entry.static_eval = static_eval;
        if (!best_move.is_null() || entry.hash != hash || bound == TT_EXACT || bound == TT_LOWER)
             entry.best_move = best_move;
    }
}

// --- Search ---
std::chrono::steady_clock::time_point search_start_timepoint;
bool stop_search_flag = false;
uint64_t nodes_searched = 0;

std::chrono::steady_clock::time_point soft_limit_timepoint;
std::chrono::steady_clock::time_point hard_limit_timepoint;
bool use_time_limits = false;

// Search Heuristics
Move killer_moves[MAX_PLY][2];
int history_heuristic_score[2][64][64];
Move countermove_heuristic_table[64][64];
int lmr_reductions_table[MAX_PLY][256];
constexpr int MAX_HISTORY_SCORE = 24000;
constexpr int TACTICAL_LOOKAHEAD_MIN_DEPTH = 5;
constexpr int TACTICAL_LOOKAHEAD_MARGIN = 110;
constexpr int TACTICAL_LOOKAHEAD_REDUCTION = 4;

// Repetition Detection Data Structures
uint64_t game_history_hashes[256];
int game_history_length = 0;
uint64_t search_path_hashes[MAX_PLY];
int static_eval_history[MAX_PLY];

void reset_search_state() {
    nodes_searched = 0;
    stop_search_flag = false;
    use_time_limits = false;
}

void reset_search_heuristics() {
    std::memset(killer_moves, 0, sizeof(killer_moves));
    std::memset(history_heuristic_score, 0, sizeof(history_heuristic_score));
    std::memset(countermove_heuristic_table, 0, sizeof(countermove_heuristic_table));

    // Initialize LMR table
    for (int d = 1; d < MAX_PLY; ++d) {
        for (int m = 1; m < 256; ++m) {
            double reduction = (log(d) * log(m)) / 2.3;
            
            int r = static_cast<int>(reduction);

            if (d < 3 || m < 2) r = 0;
            if (d > 8 && m > 4) r++;
            
            r = std::max(0, r);
            r = std::min(r, d - 2);
            
            lmr_reductions_table[d][m] = r;
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

// --- Static Exchange Evaluation (SEE) ---
int see(const Position& pos, const Move& move) {
    int gain[32];
    int d = 0;
    uint64_t from_bb = set_bit(move.from);
    uint64_t occupied = pos.get_occupied_bb();
    Color stm = pos.color_on_sq(move.from);
    Piece captured_piece_type = pos.piece_on_sq(move.to);
    if(move.promotion != NO_PIECE)
        gain[d] = see_piece_values[move.promotion];
    else
        gain[d] = see_piece_values[captured_piece_type];
    
    Piece attacking_piece_type = pos.piece_on_sq(move.from);

    uint64_t attackers = get_attackers_to_sq(pos, move.to, occupied);
    occupied ^= from_bb;
    stm = (Color)(1-stm);
    d++;

    while (true) {
        gain[d] = see_piece_values[attacking_piece_type] - gain[d - 1];
        if (std::max(-gain[d-1], gain[d]) < 0) break; // if both sides are losing, stop
        
        uint64_t side_attackers = attackers & pos.color_bb[stm];
        if (!side_attackers) break;
        
        from_bb = 0;
        attacking_piece_type = NO_PIECE;

        for (int p_type = PAWN; p_type <= KING; ++p_type) {
            uint64_t p_attackers = side_attackers & pos.piece_bb[p_type];
            if (p_attackers) {
                from_bb = set_bit(lsb_index(p_attackers));
                attacking_piece_type = (Piece)p_type;
                break;
            }
        }

        if(from_bb == 0) break;

        attackers ^= from_bb;
        occupied ^= from_bb;
        stm = (Color)(1 - stm);
        d++;
    }

    while (--d)
        gain[d-1] = -std::max(-gain[d-1], gain[d]);
    return gain[0];
}

// --- Move Picker (Phased Move Generation) ---
class MovePicker {
public:
    MovePicker(const Position& pos, int ply, const Move& tt_move, const Move& prev_move, bool quiescence = false)
        : m_pos(pos), m_ply(ply), m_tt_move(tt_move), m_prev_move(prev_move), m_quiescence_mode(quiescence) {
        
        m_current_idx = 0;
        if(m_quiescence_mode) {
             m_move_count = generate_moves(m_pos, m_moves, true);
             for(int i=0; i<m_move_count; ++i) {
                Piece captured = m_pos.piece_on_sq(m_moves[i].to);
                if (captured == NO_PIECE) captured = PAWN;
                m_moves[i].score = (see_piece_values[captured] * 100) - m_pos.piece_on_sq(m_moves[i].from);
             }
        } else {
            m_move_count = generate_moves(m_pos, m_moves, false);
            score_all_moves();
        }
    }

    Move next_move();

private:
    void score_all_moves();
    const Position& m_pos;
    int m_ply;
    Move m_tt_move;
    Move m_prev_move;
    bool m_quiescence_mode;
    Move m_moves[256];
    int m_move_count;
    int m_current_idx;
};

void MovePicker::score_all_moves() {
    Move refutation = (m_ply > 0 && !m_prev_move.is_null()) ? countermove_heuristic_table[m_prev_move.from][m_prev_move.to] : NULL_MOVE;
    uint64_t opp_pieces = m_pos.color_bb[1-m_pos.side_to_move];

    for (int i = 0; i < m_move_count; ++i) {
        Move& m = m_moves[i];
        
        if (m == m_tt_move) {
            m.score = 3000000;
            continue;
        }

        bool is_capture = get_bit(opp_pieces, m.to) || (m_pos.piece_on_sq(m.from) == PAWN && m.to == m_pos.ep_square);
        
        if (is_capture || m.promotion != NO_PIECE) {
             if (see(m_pos, m) >= 0) {
                Piece moved = m_pos.piece_on_sq(m.from);
                Piece captured = m_pos.piece_on_sq(m.to);
                if (captured == NO_PIECE) captured = PAWN; // En-passant case
                m.score = 2000000 + (see_piece_values[captured] * 100) - see_piece_values[moved];
             } else
                 m.score = -1000000; // Bad capture
        } else { // Quiet moves
            if (!refutation.is_null() && m == refutation)
                m.score = 1000000;
            else if (m_ply < MAX_PLY && !killer_moves[m_ply][0].is_null() && m == killer_moves[m_ply][0])
                m.score = 900000;
            else if (m_ply < MAX_PLY && !killer_moves[m_ply][1].is_null() && m == killer_moves[m_ply][1])
                m.score = 800000;
            else
                m.score = history_heuristic_score[m_pos.side_to_move][m.from][m.to];
        }
    }
}

Move MovePicker::next_move() {
    if (m_current_idx >= m_move_count)
        return NULL_MOVE;

    int best_idx = m_current_idx;
    for (int i = m_current_idx + 1; i < m_move_count; ++i)
        if (m_moves[i].score > m_moves[best_idx].score)
            best_idx = i;

    std::swap(m_moves[m_current_idx], m_moves[best_idx]);
    return m_moves[m_current_idx++];
}

// --- Quiescence Search ---
int quiescence_search(Position& pos, int alpha, int beta, int ply) {
    nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return evaluate(pos);

    bool in_check = is_square_attacked(pos, lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    int stand_pat_score;

    if (in_check)
        stand_pat_score = -INF_SCORE + ply;
    else {
        stand_pat_score = evaluate(pos);
        if (stand_pat_score >= beta) return beta;
        if (alpha < stand_pat_score) alpha = stand_pat_score;
    }
    
    MovePicker picker(pos, ply, NULL_MOVE, NULL_MOVE, true);
    Move current_move;
    int legal_moves_in_qsearch = 0;

    while (!(current_move = picker.next_move()).is_null()) {
        if (!in_check) {
            if (see(pos, current_move) < 0) continue;
        }

        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;
        legal_moves_in_qsearch++;

        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);

        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }

    if (in_check && legal_moves_in_qsearch == 0)
        return -MATE_SCORE + ply;

    return alpha;
}

// --- Main Search Function ---
int search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv_node, bool can_null_move, const Move& prev_move) {
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
    int tt_eval = NO_EVAL_STORED;
    if (probe_tt(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score, tt_eval))
        return tt_score;

    int static_eval = (tt_eval != NO_EVAL_STORED) ? tt_eval : evaluate(pos);
    static_eval_history[ply] = static_eval;

    if (!is_pv_node && !in_check) {
        if (depth < 8) {
            int futility_margin = 90 + 60 * depth;
            if (static_eval - futility_margin >= beta)
                return beta; 
        }

        if (depth < 4) {
            int razoring_margin = 250 + 80 * (depth-1);
            if (static_eval + razoring_margin < alpha) {
                int q_score = quiescence_search(pos, alpha, beta, ply);
                if (q_score < alpha)
                    return alpha;
            }
        }
    }

    if (!is_pv_node && !in_check && can_null_move && depth >= 3 && ply > 0 &&
        (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) != 0 &&
        static_eval >= beta) {
            Position null_next_pos = pos;
            null_next_pos.side_to_move = 1 - pos.side_to_move;
            null_next_pos.zobrist_hash = pos.zobrist_hash;
            if (pos.ep_square != -1) null_next_pos.zobrist_hash ^= zobrist_ep[pos.ep_square];
            null_next_pos.ep_square = -1;
            null_next_pos.zobrist_hash ^= zobrist_ep[64];
            null_next_pos.zobrist_hash ^= zobrist_side_to_move;
            null_next_pos.ply = pos.ply + 1;

            int R_nmp = (depth > 6) ? 3 : 2;
            int null_score = -search(null_next_pos, depth - 1 - R_nmp, -beta, -beta + 1, ply + 1, false, false, NULL_MOVE);
            
            if (stop_search_flag) return 0;
            if (null_score >= beta) {
                 if (null_score >= MATE_THRESHOLD) null_score = beta;
                 store_tt(pos.zobrist_hash, depth, ply, null_score, TT_LOWER, NULL_MOVE, static_eval);
                 return beta;
            }
    }

    if (!is_pv_node && !in_check && depth >= TACTICAL_LOOKAHEAD_MIN_DEPTH && std::abs(beta) < MATE_THRESHOLD) {
        int raised_beta = beta + TACTICAL_LOOKAHEAD_MARGIN;

        MovePicker tactical_picker(pos, ply, NULL_MOVE, NULL_MOVE, true);
        Move tactical_move;
        while (!(tactical_move = tactical_picker.next_move()).is_null()) {
            if (static_eval + see(pos, tactical_move) + 200 < raised_beta)
                continue;

            bool is_legal;
            Position next_pos = make_move(pos, tactical_move, is_legal);
            if (!is_legal) continue;

            int tactical_score = -search(next_pos, depth - 1 - TACTICAL_LOOKAHEAD_REDUCTION,
                                          -raised_beta, -raised_beta + 1, ply + 1, false, true, tactical_move);

            if (stop_search_flag) return 0;

            if (tactical_score >= raised_beta) {
                store_tt(pos.zobrist_hash, depth - TACTICAL_LOOKAHEAD_REDUCTION, ply, beta, TT_LOWER, tactical_move, static_eval);
                return beta;
            }
        }
    }

    MovePicker picker(pos, ply, tt_move, prev_move);
    Move current_move;
    int legal_moves_played = 0;
    Move best_move_found = NULL_MOVE;
    int best_score = -INF_SCORE;
    uint64_t opp_pieces = pos.color_bb[1 - pos.side_to_move];
    uint64_t friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[pos.side_to_move];

    std::vector<Move> quiet_moves_for_history;

    while(!(current_move = picker.next_move()).is_null()) {
        bool legal;
        Position next_pos = make_move(pos, current_move, legal);
        if (!legal) continue;

        bool is_quiet = !(get_bit(opp_pieces, current_move.to) || (current_move.to == pos.ep_square && get_bit(friendly_pawns, current_move.from))) && current_move.promotion == NO_PIECE;

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

        if (is_repetition)
            score = 0;
        else {
            if (is_quiet)
                quiet_moves_for_history.push_back(current_move);

            if (legal_moves_played == 1)
                score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, true, true, current_move);
            else {
                int R_lmr = 0;
                if (depth >= 3 && depth < MAX_PLY && legal_moves_played > 1 && is_quiet) {
                    bool improving = false;
                    if (ply >= 2 && !in_check)
                        improving = static_eval > static_eval_history[ply-2];
                    R_lmr = lmr_reductions_table[depth][legal_moves_played];
                    if (!improving) R_lmr++;
                    if (is_pv_node) R_lmr--;
                }
                R_lmr = std::max(0, R_lmr);

                score = -search(next_pos, depth - 1 - R_lmr, -alpha - 1, -alpha, ply + 1, false, true, current_move);
                
                if (score > alpha && R_lmr > 0)
                     score = -search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, current_move);
                if (score > alpha && score < beta)
                     score = -search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, current_move);
            }
        }

        if (stop_search_flag) return 0;

        if (score > best_score) {
            best_score = score;
            best_move_found = current_move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) {
                    if (is_quiet) {
                        if (ply < MAX_PLY) {
                            if (!(current_move == killer_moves[ply][0])) {
                                killer_moves[ply][1] = killer_moves[ply][0];
                                killer_moves[ply][0] = current_move;
                            }
                        }

                        if (ply > 0 && !prev_move.is_null())
                            countermove_heuristic_table[prev_move.from][prev_move.to] = current_move;

                        int bonus = depth * depth;
                        int& good_hist = history_heuristic_score[pos.side_to_move][current_move.from][current_move.to];
                        good_hist += bonus - (good_hist * std::abs(bonus) / MAX_HISTORY_SCORE);

                        for (const Move& bad_move : quiet_moves_for_history) {
                             if(bad_move == current_move) continue;
                             int& bad_hist = history_heuristic_score[pos.side_to_move][bad_move.from][bad_move.to];
                             bad_hist -= bonus - (bad_hist * std::abs(bonus) / MAX_HISTORY_SCORE);
                        }
                    }
                    store_tt(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move_found, static_eval);
                    return beta;
                }
            }
        }
    }

    if (legal_moves_played == 0)
        return in_check ? (-MATE_SCORE + ply) : 0;

    TTBound final_bound_type = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    store_tt(pos.zobrist_hash, depth, ply, best_score, final_bound_type, best_move_found, static_eval);
    return best_score;
}

// --- UCI ---
Position uci_root_pos;
Move uci_best_move_overall;
Move uci_last_root_move = NULL_MOVE;

uint64_t calculate_pawn_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c) {
        uint64_t b = pos.piece_bb[PAWN] & pos.color_bb[c];
        while (b) {
            int sq = lsb_index(b);
            b &= b - 1;
            h ^= zobrist_pieces[c][PAWN][sq];
        }
    }
    return h;
}

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
    pos.pawn_zobrist_key = calculate_pawn_zobrist_hash(pos);
    game_history_length = 0;
    uci_last_root_move = NULL_MOVE;
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
            std::cout << "id name Amira 1.55\n";
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
                        g_configured_tt_size_mb = std::max(0, std::min(parsed_size, 1024));
                    } catch (...) {}
                    init_tt(g_configured_tt_size_mb);
                    g_tt_is_initialized = true;
                }
            }
        } else if (token == "ucinewgame") {
            parse_fen(uci_root_pos, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            if (!g_tt_is_initialized) {
                 init_tt(g_configured_tt_size_mb);
                 g_tt_is_initialized = true;
            } else
                 clear_tt();
            clear_pawn_cache();
            reset_search_heuristics();
            game_history_length = 0;
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

            if (token == "moves") {
                std::string move_str_uci;
                while (ss >> move_str_uci) {
                    Move m = parse_uci_move_from_string(uci_root_pos, move_str_uci);
                    if (m.is_null() && move_str_uci != "0000") break;
                    if (m.is_null() && move_str_uci == "0000") break;

                    if (game_history_length < 256)
                        game_history_hashes[game_history_length++] = uci_root_pos.zobrist_hash;
                    Piece moved_piece = uci_root_pos.piece_on_sq(m.from);
                    Piece captured_piece = uci_root_pos.piece_on_sq(m.to);
                    if (moved_piece == PAWN || captured_piece != NO_PIECE)
                        game_history_length = 0;

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
                const Move& m = root_pseudo_moves[i];
                bool is_legal_flag;
                make_move(uci_root_pos, m, is_legal_flag);
                if (is_legal_flag)
                    root_legal_moves.push_back(m);
            }

            if (root_legal_moves.size() == 1) {
                std::cout << "bestmove " << move_to_uci(root_legal_moves[0]) << std::endl;
                continue;
            }
            if (root_legal_moves.empty()) {
                std::cout << "bestmove 0000" << std::endl;
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

                if (my_inc > 0) {
                    double soft_limit_s = my_time * 0.000052;
                    double hard_limit_s = my_time * 0.00041;
                    soft_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(soft_limit_s * 1000000.0));
                    hard_limit_timepoint = search_start_timepoint + std::chrono::microseconds(static_cast<long long>(hard_limit_s * 1000000.0));
                } else {
                    int divisor = (movestogo > 0) ? movestogo : 25;

                    long long allocated_time_ms = my_time / divisor;
                    
                    allocated_time_ms = std::min(allocated_time_ms, my_time * 8 / 10);
                    
                    if (allocated_time_ms >= my_time)
                        allocated_time_ms = my_time - 100;
                    if (allocated_time_ms < 0) allocated_time_ms = 0;
                    
                    soft_limit_timepoint = search_start_timepoint + std::chrono::milliseconds(allocated_time_ms);
                    hard_limit_timepoint = search_start_timepoint + std::chrono::milliseconds(allocated_time_ms);
                }
            } else
                use_time_limits = false;

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

                if (std::abs(current_score) < MATE_THRESHOLD && current_score > -INF_SCORE && current_score < INF_SCORE) {
                    aspiration_alpha = current_score - aspiration_window_delta;
                    aspiration_beta = current_score + aspiration_window_delta;
                    aspiration_window_delta += aspiration_window_delta / 3 + 5;
                    if (aspiration_window_delta > 300) aspiration_window_delta = 300;
                } else {
                    aspiration_alpha = -INF_SCORE;
                    aspiration_beta = INF_SCORE;
                    aspiration_window_delta = 50;
                }

                Move tt_root_move = NULL_MOVE; int tt_root_score, tt_root_eval;
                int dummy_alpha = -INF_SCORE, dummy_beta = INF_SCORE;
                if (probe_tt(uci_root_pos.zobrist_hash, depth, 0, dummy_alpha, dummy_beta, tt_root_move, tt_root_score, tt_root_eval)) {
                     if (!tt_root_move.is_null()) uci_best_move_overall = tt_root_move;
                     best_score_overall = current_score;
                } else {
                     best_score_overall = current_score;
                     TTEntry root_entry_check = transposition_table[uci_root_pos.zobrist_hash & tt_mask];
                     if (root_entry_check.hash == uci_root_pos.zobrist_hash && !root_entry_check.best_move.is_null())
                         uci_best_move_overall = root_entry_check.best_move;
                }

                auto now_tp = std::chrono::steady_clock::now();
                auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(now_tp - search_start_timepoint).count();
                if (elapsed_ms <= 0) elapsed_ms = 1;

                std::cout << "info depth " << depth << " score cp " << best_score_overall;
                if (best_score_overall > MATE_THRESHOLD) std::cout << " mate " << (MATE_SCORE - best_score_overall + 1)/2 ;
                else if (best_score_overall < -MATE_THRESHOLD) std::cout << " mate " << -(MATE_SCORE + best_score_overall +1)/2;
                std::cout << " nodes " << nodes_searched << " time " << elapsed_ms;
                if (elapsed_ms > 0 && nodes_searched > 0) std::cout << " nps " << (nodes_searched * 1000 / elapsed_ms);

                if (!uci_best_move_overall.is_null()) {
                    std::cout << " pv";
                    Position temp_pos = uci_root_pos;
                    for (int pv_idx = 0; pv_idx < depth; ++pv_idx) {
                        Move pv_m; int pv_s, pv_e; int pv_a = -INF_SCORE, pv_b = INF_SCORE;
                        if (probe_tt(temp_pos.zobrist_hash, 1, 0, pv_a, pv_b, pv_m, pv_s, pv_e) && !pv_m.is_null()) {
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

                if (use_time_limits && std::chrono::steady_clock::now() > soft_limit_timepoint)
                    break;
                if (std::abs(best_score_overall) > MATE_THRESHOLD && depth > 1) break;
                if (depth >= max_depth_to_search) break;
            }

            if (!uci_best_move_overall.is_null())
                 std::cout << "bestmove " << move_to_uci(uci_best_move_overall) << std::endl;
            else {
                Move legal_moves_fallback[256];
                int num_fallback_moves = generate_moves(uci_root_pos, legal_moves_fallback, false);
                bool found_one_legal_fallback = false;
                for(int i = 0; i < num_fallback_moves; ++i) {
                    bool is_legal_flag;
                    make_move(uci_root_pos, legal_moves_fallback[i], is_legal_flag);
                    if (is_legal_flag) {
                        std::cout << "bestmove " << move_to_uci(legal_moves_fallback[i]) << std::endl;
                        found_one_legal_fallback = true;
                        break;
                    }
                }
                if (!found_one_legal_fallback)
                     std::cout << "bestmove 0000\n" << std::flush;
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
    init_magic_bitboards();
    init_eval_masks();
    init_pawn_cache();
    reset_search_heuristics();
    uci_loop();
    return 0;
}
