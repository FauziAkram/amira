magic_rook_indices[0] = magic_rook_table + 86016; magic_rook_indices[1] = magic_rook_table + 73728;
        // ... (rest of indices setup is tedious but necessary)
        uint64_t* rook_offsets[] = {
          magic_rook_table + 86016, magic_rook_table + 73728, magic_rook_table + 36864, magic_rook_table + 43008, magic_rook_table + 47104, magic_rook_table + 51200, magic_rook_table + 77824, magic_rook_table + 94208,
          magic_rook_table + 69632, magic_rook_table + 32768, magic_rook_table + 38912, magic_rook_table + 10240, magic_rook_table + 14336, magic_rook_table + 53248, magic_rook_table + 57344, magic_rook_table + 81920,
          magic_rook_table + 24576, magic_rook_table + 33792, magic_rook_table + 6144,  magic_rook_table + 11264, magic_rook_table + 15360, magic_rook_table + 18432, magic_rook_table + 58368, magic_rook_table + 61440,
          magic_rook_table + 26624, magic_rook_table + 4096,  magic_rook_table + 7168,  magic_rook_table + 0,     magic_rook_table + 2048,  magic_rook_table + 19456, magic_rook_table + 22528, magic_rook_table + 63488,
          magic_rook_table + 28672, magic_rook_table + 5120,  magic_rook_table + 8192,  magic_rook_table + 1024,  magic_rook_table + 3072,  magic_rook_table + 20480, magic_rook_table + 23552, magic_rook_table + 65536,
          magic_rook_table + 30720, magic_rook_table + 34816, magic_rook_table + 9216,  magic_rook_table + 12288, magic_rook_table + 16384, magic_rook_table + 21504, magic_rook_table + 59392, magic_rook_table + 67584,
          magic_rook_table + 71680, magic_rook_table + 35840, magic_rook_table + 39936, magic_rook_table + 13312, magic_rook_table + 17408, magic_rook_table + 54272, magic_rook_table + 60416, magic_rook_table + 83968,
          magic_rook_table + 90112, magic_rook_table + 75776, magic_rook_table + 40960, magic_rook_table + 45056, magic_rook_table + 49152, magic_rook_table + 55296, magic_rook_table + 79872, magic_rook_table + 98304
        };
        uint64_t* bishop_offsets[] = {
          magic_bishop_table + 4992, magic_bishop_table + 2624, magic_bishop_table + 256,  magic_bishop_table + 896,  magic_bishop_table + 1280, magic_bishop_table + 1664, magic_bishop_table + 4800, magic_bishop_table + 5120,
          magic_bishop_table + 2560, magic_bishop_table + 2656, magic_bishop_table + 288,  magic_bishop_table + 928,  magic_bishop_table + 1312, magic_bishop_table + 1696, magic_bishop_table + 4832, magic_bishop_table + 4928,
          magic_bishop_table + 0,    magic_bishop_table + 128,  magic_bishop_table + 320,  magic_bishop_table + 960,  magic_bishop_table + 1344, magic_bishop_table + 1728, magic_bishop_table + 2304, magic_bishop_table + 2432,
          magic_bishop_table + 32,   magic_bishop_table + 160,  magic_bishop_table + 448,  magic_bishop_table + 2752, magic_bishop_table + 3776, magic_bishop_table + 1856, magic_bishop_table + 2336, magic_bishop_table + 2464,
          magic_bishop_table + 64,   magic_bishop_table + 192,  magic_bishop_table + 576,  magic_bishop_table + 3264, magic_bishop_table + 4288, magic_bishop_table + 1984, magic_bishop_table + 2368, magic_bishop_table + 2496,
          magic_bishop_table + 96,   magic_bishop_table + 224,  magic_bishop_table + 704,  magic_bishop_table + 1088, magic_bishop_table + 1472, magic_bishop_table + 2112, magic_bishop_table + 2400, magic_bishop_table + 2528,
          magic_bishop_table + 2592, magic_bishop_table + 2688, magic_bishop_table + 832,  magic_bishop_table + 1216, magic_bishop_table + 1600, magic_bishop_table + 2240, magic_bishop_table + 4864, magic_bishop_table + 4960,
          magic_bishop_table + 5056, magic_bishop_table + 2720, magic_bishop_table + 864,  magic_bishop_table + 1248, magic_bishop_table + 1632, magic_bishop_table + 2272, magic_bishop_table + 4896, magic_bishop_table + 5184
        };
        for(int i=0; i<64; ++i) {
            magic_rook_indices[i] = rook_offsets[i];
            magic_bishop_indices[i] = bishop_offsets[i];
        }

        for (int sq = 0; sq < 64; ++sq) {
            uint64_t mask = magic_rook_mask[sq];
            int num_mask_bits = Bitboard::pop_count(mask);
            for (int i = 0; i < (1 << num_mask_bits); ++i) {
                uint64_t blockers = 0;
                uint64_t temp_mask = mask;
                for (int j = 0; j < num_mask_bits; ++j) {
                    int bit_pos = Bitboard::lsb_index(temp_mask);
                    temp_mask &= temp_mask - 1;
                    if ((i >> j) & 1) blockers |= Bitboard::set_bit(bit_pos);
                }
                uint64_t index = (blockers * magic_rook[sq]) >> magic_rook_shift[sq];
                magic_rook_indices[sq][index] = reference_rook_attacks(sq, blockers);
            }
            mask = magic_bishop_mask[sq];
            num_mask_bits = Bitboard::pop_count(mask);
            for (int i = 0; i < (1 << num_mask_bits); ++i) {
                uint64_t blockers = 0;
                uint64_t temp_mask = mask;
                for (int j = 0; j < num_mask_bits; ++j) {
                    int bit_pos = Bitboard::lsb_index(temp_mask);
                    temp_mask &= temp_mask - 1;
                    if ((i >> j) & 1) blockers |= Bitboard::set_bit(bit_pos);
                }
                uint64_t index = (blockers * magic_bishop[sq]) >> magic_bishop_shift[sq];
                magic_bishop_indices[sq][index] = reference_bishop_attacks(sq, blockers);
            }
        }
    }
};

// --- Magic Bitboard Static Data Initialization ---
const uint64_t AttackTables::magic_rook_mask[64] = {
  0x000101010101017Eull, 0x000202020202027Cull, 0x000404040404047Aull, 0x0008080808080876ull, 0x001010101010106Eull, 0x002020202020205Eull, 0x004040404040403Eull, 0x008080808080807Eull,
  0x0001010101017E00ull, 0x0002020202027C00ull, 0x0004040404047A00ull, 0x0008080808087600ull, 0x0010101010106E00ull, 0x0020202020205E00ull, 0x0040404040403E00ull, 0x0080808080807E00ull,
  0x00010101017E0100ull, 0x00020202027C0200ull, 0x00040404047A0400ull, 0x0008080808760800ull, 0x00101010106E1000ull, 0x00202020205E2000ull, 0x00404040403E4000ull, 0x00808080807E8000ull,
  0x000101017E010100ull, 0x000202027C020200ull, 0x000404047A040400ull, 0x0008080876080800ull, 0x001010106E101000ull, 0x002020205E202000ull, 0x004040403E404000ull, 0x008080807E808000ull,
  0x0001017E01010100ull, 0x0002027C02020200ull, 0x0004047A04040400ull, 0x0008087608080800ull, 0x0010106E10101000ull, 0x0020205E20202000ull, 0x0040403E40404000ull, 0x0080807E80808000ull,
  0x00017E0101010100ull, 0x00027C0202020200ull, 0x00047A0404040400ull, 0x0008760808080800ull, 0x00106E1010101000ull, 0x00205E2020202000ull, 0x00403E4040404000ull, 0x00807E8080808000ull,
  0x007E010101010100ull, 0x007C020202020200ull, 0x007A040404040400ull, 0x0076080808080800ull, 0x006E101010101000ull, 0x005E202020202000ull, 0x003E404040404000ull, 0x007E808080808000ull,
  0x7E01010101010100ull, 0x7C02020202020200ull, 0x7A04040404040400ull, 0x7608080808080800ull, 0x6E10101010101000ull, 0x5E20202020202000ull, 0x3E40404040404000ull, 0x7E80808080808000ull
};
const uint64_t AttackTables::magic_rook[64] = {
  0x0080001020400080ull, 0x0040001000200040ull, 0x0080081000200080ull, 0x0080040800100080ull, 0x0080020400080080ull, 0x0080010200040080ull, 0x0080008001000200ull, 0x0080002040800100ull,
  0x0000800020400080ull, 0x0000400020005000ull, 0x0000801000200080ull, 0x0000800800100080ull, 0x0000800400080080ull, 0x0000800200040080ull, 0x0000800100020080ull, 0x0000800040800100ull,
  0x0000208000400080ull, 0x0000404000201000ull, 0x0000808010002000ull, 0x0000808008001000ull, 0x0000808004000800ull, 0x0000808002000400ull, 0x0000010100020004ull, 0x0000020000408104ull,
  0x0000208080004000ull, 0x0000200040005000ull, 0x0000100080200080ull, 0x0000080080100080ull, 0x0000040080080080ull, 0x0000020080040080ull, 0x0000010080800200ull, 0x0000800080004100ull,
  0x0000204000800080ull, 0x0000200040401000ull, 0x0000100080802000ull, 0x0000080080801000ull, 0x0000040080800800ull, 0x0000020080800400ull, 0x0000020001010004ull, 0x0000800040800100ull,
  0x0000204000808000ull, 0x0000200040008080ull, 0x0000100020008080ull, 0x0000080010008080ull, 0x0000040008008080ull, 0x0000020004008080ull, 0x0000010002008080ull, 0x0000004081020004ull,
  0x0000204000800080ull, 0x0000200040008080ull, 0x0000100020008080ull, 0x0000080010008080ull, 0x0000040008008080ull, 0x0000020004008080ull, 0x0000800100020080ull, 0x0000800041000080ull,
  0x00FFFCDDFCED714Aull, 0x007FFCDDFCED714Aull, 0x003FFFCDFFD88096ull, 0x0000040810002101ull, 0x0001000204080011ull, 0x0001000204000801ull, 0x0001000082000401ull, 0x0001FFFAABFAD1A2ull
};
const uint32_t AttackTables::magic_rook_shift[64] = { 52, 53, 53, 53, 53, 53, 53, 52, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 53, 53, 53, 53, 53 };
const uint64_t AttackTables::magic_bishop_mask[64] = {
  0x0040201008040200ull, 0x0000402010080400ull, 0x0000004020100A00ull, 0x0000000040221400ull, 0x0000000002442800ull, 0x0000000204085000ull, 0x0000020408102000ull, 0x0002040810204000ull,
  0x0020100804020000ull, 0x0040201008040000ull, 0x00004020100A0000ull, 0x0000004022140000ull, 0x0000000244280000ull, 0x0000020408500000ull, 0x0002040810200000ull, 0x0004081020400000ull,
  0x0010080402000200ull, 0x0020100804000400ull, 0x004020100A000A00ull, 0x0000402214001400ull, 0x0000024428002800ull, 0x0002040850005000ull, 0x0004081020002000ull, 0x0008102040004000ull,
  0x0008040200020400ull, 0x0010080400040800ull, 0x0020100A000A1000ull, 0x0040221400142200ull, 0x0002442800284400ull, 0x0004085000500800ull, 0x0008102000201000ull, 0x0010204000402000ull,
  0x0004020002040800ull, 0x0008040004081000ull, 0x00100A000A102000ull, 0x0022140014224000ull, 0x0044280028440200ull, 0x0008500050080400ull, 0x0010200020100800ull, 0x0020400040201000ull,
  0x0002000204081000ull, 0x0004000408102000ull, 0x000A000A10204000ull, 0x0014001422400000ull, 0x0028002844020000ull, 0x0050005008040200ull, 0x0020002010080400ull, 0x0040004020100800ull,
  0x0000020408102000ull, 0x0000040810204000ull, 0x00000A1020400000ull, 0x0000142240000000ull, 0x0000284402000000ull, 0x0000500804020000ull, 0x0000201008040200ull, 0x0000402010080400ull,
  0x0002040810204000ull, 0x0004081020400000ull, 0x000A102040000000ull, 0x0014224000000000ull, 0x0028440200000000ull, 0x0050080402000000ull, 0x0020100804020000ull, 0x0040201008040200ull
};
const uint64_t AttackTables::magic_bishop[64] = {
  0x0002020202020200ull, 0x0002020202020000ull, 0x0004010202000000ull, 0x0004040080000000ull, 0x0001104000000000ull, 0x0000821040000000ull, 0x0000410410400000ull, 0x0000104104104000ull,
  0x0000040404040400ull, 0x0000020202020200ull, 0x0000040102020000ull, 0x0000040400800000ull, 0x0000011040000000ull, 0x0000008210400000ull, 0x0000004104104000ull, 0x0000002082082000ull,
  0x0004000808080800ull, 0x0002000404040400ull, 0x0001000202020200ull, 0x0000800802004000ull, 0x0000800400A00000ull, 0x0000200100884000ull, 0x0000400082082000ull, 0x0000200041041000ull,
  0x0002080010101000ull, 0x0001040008080800ull, 0x0000208004010400ull, 0x0000404004010200ull, 0x0000840000802000ull, 0x0000404002011000ull, 0x0000808001041000ull, 0x0000404000820800ull,
  0x0001041000202000ull, 0x0000820800101000ull, 0x0000104400080800ull, 0x0000020080080080ull, 0x0000404040040100ull, 0x0000808100020100ull, 0x0001010100020800ull, 0x0000808080010400ull,
  0x0000820820004000ull, 0x0000410410002000ull, 0x0000082088001000ull, 0x0000002011000800ull, 0x0000080100400400ull, 0x0001010101000200ull, 0x0002020202000400ull, 0x0001010101000200ull,
  0x0000410410400000ull, 0x0000208208200000ull, 0x0000002084100000ull, 0x0000000020880000ull, 0x0000001002020000ull, 0x0000040408020000ull, 0x0000040404040400ull, 0x0000020202020200ull,
  0x0000104104104000ull, 0x0000002082082000ull, 0x0000000020841000ull, 0x0000000000208800ull, 0x0000000010020200ull, 0x0000000404080200ull, 0x0000040404040400ull, 0x0002020202020200ull
};
const uint32_t AttackTables::magic_bishop_shift[64] = { 58, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 58 };

// Global instance of attack tables, initialized once in main.
AttackTables g_attack_tables;

// ============================================================================
// 5. POSITION REPRESENTATION
// ============================================================================

class Position {
public:
    uint8_t squares[64];
    uint64_t piece_bb[6];
    uint64_t color_bb[2];
    int side_to_move;
    int ep_square;
    uint8_t castling_rights;
    uint64_t zobrist_hash;
    uint64_t pawn_zobrist_key;
    int halfmove_clock;
    int fullmove_number;
    int ply;
    int static_eval;

    Position() {
        std::memset(this, 0, sizeof(Position));
        side_to_move = WHITE;
        ep_square = -1;
        fullmove_number = 1;
        std::memset(squares, EMPTY_SQUARE, sizeof(squares));
    }

    uint64_t get_occupied_bb() const { return color_bb[WHITE] | color_bb[BLACK]; }

    Piece piece_on_sq(int sq) const {
        if (sq < 0 || sq >= 64 || squares[sq] == EMPTY_SQUARE) return NO_PIECE;
        return get_piece_type(squares[sq]);
    }

    Color color_on_sq(int sq) const {
        if (sq < 0 || sq >= 64 || squares[sq] == EMPTY_SQUARE) return NO_COLOR;
        return get_piece_color(squares[sq]);
    }
};

// --- Helper functions that depend on Position and AttackTables ---
bool is_square_attacked(const Position& pos, int sq, int attacker_color) {
    if (g_attack_tables.pawn_attacks[1 - attacker_color][sq] & pos.piece_bb[PAWN] & pos.color_bb[attacker_color]) return true;
    if (g_attack_tables.knight_attacks[sq] & pos.piece_bb[KNIGHT] & pos.color_bb[attacker_color]) return true;
    if (g_attack_tables.king_attacks[sq] & pos.piece_bb[KING] & pos.color_bb[attacker_color]) return true;

    uint64_t occupied = pos.get_occupied_bb();
    if (g_attack_tables.get_rook_attacks(sq, occupied) & (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_color]) return true;
    if (g_attack_tables.get_bishop_attacks(sq, occupied) & (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]) & pos.color_bb[attacker_color]) return true;
    
    return false;
}

int generate_moves(const Position& pos, Move* moves_list, bool captures_only);
Position make_move(const Position& pos, const Move& move, bool& legal);

// ============================================================================
// 6. PAWN HASH TABLE
// ============================================================================

class PawnCache {
private:
    struct Entry {
        uint64_t key = 0;
        PhaseScore score = {};
        uint64_t white_passed_pawns = 0;
        uint64_t black_passed_pawns = 0;
    };
    std::vector<Entry> m_cache;
    uint64_t m_mask = 0;

public:
    PawnCache() {
        m_cache.assign(PAWN_CACHE_SIZE_ENTRIES, Entry());
        m_mask = PAWN_CACHE_SIZE_ENTRIES - 1;
    }

    void clear() {
        if (!m_cache.empty())
            std::memset(m_cache.data(), 0, m_cache.size() * sizeof(Entry));
    }
    
    // Returns true on a cache hit
    bool probe(uint64_t key, PhaseScore& score, uint64_t& w_passers, uint64_t& b_passers) {
        Entry& entry = m_cache[key & m_mask];
        if (entry.key == key) {
            score = entry.score;
            w_passers = entry.white_passed_pawns;
            b_passers = entry.black_passed_pawns;
            return true;
        }
        return false;
    }

    void store(uint64_t key, const PhaseScore& score, uint64_t w_passers, uint64_t b_passers) {
        Entry& entry = m_cache[key & m_mask];
        entry.key = key;
        entry.score = score;
        entry.white_passed_pawns = w_passers;
        entry.black_passed_pawns = b_passers;
    }
};

// ============================================================================
// 7. TRANSPOSITION TABLE
// ============================================================================

class TranspositionTable {
private:
    struct TTEntry {
        uint64_t hash = 0;
        Move best_move = NULL_MOVE;
        int score = 0;
        int depth = 0;
        int static_eval = 0;
        TTBound bound = TT_NONE;
    };

    std::vector<TTEntry> m_table;
    uint64_t m_mask = 0;
    bool m_is_initialized = false;
    int m_configured_size_mb = TT_SIZE_MB_DEFAULT;

public:
    void set_size(size_t mb_size) {
        m_configured_size_mb = mb_size;
        m_is_initialized = false;
    }

    void init() {
        if (m_is_initialized) return;

        size_t num_entries = (m_configured_size_mb * 1024ULL * 1024ULL) / sizeof(TTEntry);
        if (num_entries == 0) { m_table.clear(); m_mask = 0; return; }

        size_t power_of_2_entries = 1;
        while (power_of_2_entries * 2 <= num_entries && power_of_2_entries * 2 > power_of_2_entries)
            power_of_2_entries *= 2;
        
        if (power_of_2_entries == 0) { m_table.clear(); m_mask = 0; return; }

        try {
            m_table.assign(power_of_2_entries, TTEntry());
            m_mask = power_of_2_entries - 1;
        } catch (const std::bad_alloc&) {
            m_table.clear(); m_mask = 0;
        }
        m_is_initialized = true;
    }

    void clear() {
        if (!m_table.empty())
            std::memset(m_table.data(), 0, m_table.size() * sizeof(TTEntry));
    }

    bool probe(uint64_t hash, int depth, int ply, int& alpha, int& beta, Move& move, int& score, int& eval) {
        if (m_mask == 0) return false;
        TTEntry& entry = m_table[hash & m_mask];

        if (entry.hash == hash && entry.bound != TT_NONE) {
            move = entry.best_move;
            eval = entry.static_eval;
            if (entry.depth >= depth) {
                int stored_score = entry.score;
                if (stored_score > MATE_THRESHOLD) stored_score -= ply;
                else if (stored_score < -MATE_THRESHOLD) stored_score += ply;

                score = stored_score;
                if (entry.bound == TT_EXACT) return true;
                if (entry.bound == TT_LOWER && stored_score >= beta) return true;
                if (entry.bound == TT_UPPER && stored_score <= alpha) return true;
            }
        }
        return false;
    }

    void store(uint64_t hash, int depth, int ply, int score, TTBound bound, const Move& best_move, int static_eval) {
        if (m_mask == 0) return;
        TTEntry& entry = m_table[hash & m_mask];

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
};

// ============================================================================
// 8. EVALUATION
// ============================================================================

class Evaluation {
public:
    int evaluate(Position& pos, PawnCache& pawn_cache);
    void init_eval_masks();

private:
    // --- Constants & PSTs (kept inside the class for encapsulation) ---
    static const PhaseScore piece_phase_values[6];
    static const int game_phase_inc[6];
    static const PhaseScore* pst_all[6];
    static const PhaseScore pawn_pst[64], knight_pst[64], bishop_pst[64], rook_pst[64], queen_pst[64], king_pst[64];
    
    // Evaluation term constants
    static const PhaseScore TEMPO_BONUS;
    static const PhaseScore BISHOP_PAIR_BONUS;
    static const PhaseScore PROTECTED_PAWN_BONUS;
    static const PhaseScore ISOLATED_PAWN_PENALTY;
    static const PhaseScore DOUBLED_PAWN_PENALTY;
    static const PhaseScore BACKWARD_PAWN_PENALTY;
    static const PhaseScore KNIGHT_MOBILITY_BONUS;
    static const PhaseScore BISHOP_MOBILITY_BONUS;
    static const PhaseScore ROOK_MOBILITY_BONUS;
    static const PhaseScore QUEEN_MOBILITY_BONUS;
    static const PhaseScore KNIGHT_OUTPOST_BONUS;
    static const PhaseScore BISHOP_OUTPOST_BONUS;
    static const PhaseScore ROOK_ON_OPEN_FILE;
    static const PhaseScore ROOK_ON_SEMI_OPEN_FILE;
    static const PhaseScore RookOnEnemyTerritoryBonus;
    static const PhaseScore ConnectedRooksOnTerritoryBonus;
    static const PhaseScore passed_pawn_bonus[8];
    static const PhaseScore THREAT_BY_MINOR[7];
    static const PhaseScore THREAT_BY_ROOK[7];
    static const PhaseScore THREAT_BY_KING;
    static const PhaseScore HANGING_PIECE_BONUS;
    static const PhaseScore PhalanxPawnBonus[8][8];
    static const int SHIELD_PAWN_PRESENT_BONUS;
    static const int SHIELD_PAWN_MISSING_PENALTY;
    static const int SHIELD_PAWN_ADVANCED_PENALTY;
    static const int SHIELD_OPEN_FILE_PENALTY;
    static const int SafetyKnightWeight, SafetyBishopWeight, SafetyRookWeight, SafetyQueenWeight, SafetyAttackValue, SafetyWeakSquares, SafetySafeQueenCheck, SafetySafeRookCheck, SafetySafeBishopCheck, SafetySafeKnightCheck, SafetyAdjustment;


    // Helper masks
    uint64_t file_bb_mask[8];
    uint64_t rank_bb_mask[8];
    uint64_t white_passed_pawn_block_mask[64];
    uint64_t black_passed_pawn_block_mask[64];
    uint64_t adjacent_files_mask[8];
    uint64_t pawn_attack_shield_mask[2][64];

    // --- Helper Functions ---
    bool is_insufficient_material(const Position& pos);
    void evaluate_pawn_structure_for_color(const Position& pos, Color c, PhaseScore& score, uint64_t& passed_pawns);
    PhaseScore evaluate_threats_for_color(const Position& pos, Color us, const uint64_t attackedBy[2][7], const uint64_t attackedBy2[2]);
    int get_endgame_material_modifier(const Position& pos, const PhaseScore& score);
};


// --- Initialization of Static Data Members for Evaluation ---
const PhaseScore Evaluation::piece_phase_values[6] = {{85, 135}, {380, 410}, {390, 430}, {570, 680}, {1120, 1350}, {0, 0}};
const int Evaluation::game_phase_inc[6] = {0, 1, 1, 2, 4, 0};
const PhaseScore Evaluation::pawn_pst[64] = {
    {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0},
    {  1,  10}, {  2,  10}, {  0,   9}, { -2,   0}, {  0,  11}, {  1,   8}, {  0,  10}, { -1,   4},
    {  0,   2}, { -5,   3}, {  8,  -2}, { 13,   1}, { 20,  -1}, { 13,   0}, {  1,  -3}, { -8,  -1},
    {  3,   9}, { -8,   5}, {  3,   0}, { 12,   3}, { 25,  -3}, { 10,  -3}, { -3,  -1}, {  0,   2},
    {  9,  18}, {  5,  13}, {  0,  11}, {  5,   2}, { 10,   3}, {  5,   4}, { -2,  19}, {  7,  16},
    { 25,  55}, { 25,  58}, { 25,  59}, { 40,  69}, { 30,  70}, { 25,  54}, { 20,  53}, { 20,  57},
    { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160}, { 90, 160},
    {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}, {  0,   0}
};
const PhaseScore Evaluation::knight_pst[64] = {
    {-85, -70}, {-50, -45}, {-40, -35}, {-40, -25}, {-40, -25}, {-40, -35}, {-50, -45}, {-85, -70},
    {-45, -45}, {-20, -30}, { -5, -10}, {  0,  10}, {  0,  10}, { -5, -10}, {-20, -30}, {-45, -45},
    {-40, -30}, { -5, -10}, { 10,   5}, { 15,  25}, { 15,  25}, { 10,   5}, { -5, -10}, {-40, -30},
    {-35, -30}, {  5,   5}, { 20,  20}, { 25,  35}, { 25,  35}, { 20,  20}, {  5,   5}, {-35, -30},
    {-35, -35}, { 10,   0}, { 22,  20}, { 25,  35}, { 25,  35}, { 22,  20}, { 10,   0}, {-35, -35},
    {-40, -40}, { 10, -15}, { 30,   0}, { 25,  20}, { 25,  20}, { 30,   0}, { 10, -15}, {-40, -40},
    {-50, -50}, {-30, -30}, {  0, -20}, {  10,  0}, {  10,  0}, {  0, -20}, {-30, -30}, {-50, -50},
    {-90, -75}, {-50, -50}, {-40, -40}, {-30, -20}, {-30, -20}, {-40, -40}, {-50, -50}, {-90, -75}
};
const PhaseScore Evaluation::bishop_pst[64] = {
    {-28, -25}, {-10, -15}, {-10, -15}, {-10, -10}, {-10, -10}, {-10, -15}, {-10, -15}, {-28, -25},
    {-10, -15}, {  5,  -5}, {  0,  -5}, {  3,   0}, {  3,   0}, {  0,  -5}, {  5,  -5}, {-10, -15},
    {-10, -10}, {  5,   0}, {  0,   5}, { 10,  10}, { 10,  10}, {  0,   5}, {  5,   0}, {-10, -10},
    {-10, -10}, {  5,   0}, {  8,   5}, { 15,  15}, { 15,  15}, {  8,   5}, {  5,   0}, {-10, -10},
    {-10, -10}, { 10,   0}, { 10,   0}, { 15,  15}, { 15,  15}, { 10,   0}, { 10,   0}, {-10, -10},
    {-10, -15}, {  5,   5}, {  5,   5}, { 10,  10}, { 10,  10}, {  5,   5}, {  5,   5}, {-10, -15},
    {-10, -15}, {  0, -10}, {  5,  -5}, {  0,   0}, {  0,   0}, {  5,  -5}, {  0, -10}, {-10, -15},
    {-28, -25}, {-10, -20}, {-10, -15}, {-10, -10}, {-10, -10}, {-10, -15}, {-10, -20}, {-28, -25}
};
const PhaseScore Evaluation::rook_pst[64] = {
    {-15,   0}, {-10,   0}, { -5,   0}, { -2,   0}, { -2,   0}, { -5,   0}, {-10,   0}, {-15,   0},
    {-12,  -5}, { -5,  -5}, { -2,  -2}, {  5,  -2}, {  5,  -2}, { -2,  -2}, { -5,  -5}, {-12,  -5},
    {-12,  -5}, { -5,  -5}, { -2,  -2}, {  5,  -2}, {  5,  -2}, { -2,  -2}, { -5,  -5}, {-12,  -5},
    {-10,  -5}, { -5,   0}, { -2,   0}, {  0,   0}, {  0,   0}, { -2,   0}, { -5,   0}, {-10,  -5},
    {-15,  -5}, { -5,   2}, { -2,   2}, {  0,   0}, {  0,   0}, { -2,   2}, { -5,   2}, {-15,  -5},
    {-12,   0}, { -2,   0}, {  5,   0}, { 10,   5}, { 10,   5}, {  5,   0}, { -2,   0}, {-12,   0},
    { -1,  10}, {  5,  10}, {  8,  15}, { 12,  10}, { 12,  10}, {  8,  15}, {  5,  10}, { -1,  10},
    {-10,  10}, {-10,   5}, {  0,  10}, {  5,  10}, {  5,  10}, {  0,  10}, {-10,   5}, {-10,  10}
};
const PhaseScore Evaluation::queen_pst[64] = {
    { -10, -40}, {  -5, -30}, {  -5, -25}, {   0, -15}, {   0, -15}, {  -5, -25}, {  -5, -30}, { -10, -40},
    {  -5, -30}, {   0, -15}, {   2, -10}, {   5,  -2}, {   5,  -2}, {   2, -10}, {   0, -15}, {  -5, -30},
    {  -5, -20}, {   0, -10}, {   5,  -5}, {   5,   0}, {   5,   0}, {   5,  -5}, {   0, -10}, {  -5, -20},
    {   0, -12}, {   2,  -2}, {   5,   5}, {   5,  10}, {   5,  10}, {   5,   5}, {   2,  -2}, {   0, -12},
    {  -2, -15}, {   5,  -5}, {   5,   5}, {   5,  10}, {   5,  10}, {   5,   5}, {   5,  -5}, {  -2, -15},
    {  -5, -20}, {   5, -10}, {   2,  -5}, {   5,   0}, {   5,   0}, {   2,  -5}, {   5, -10}, {  -5, -20},
    {  -5, -30}, {   0, -15}, {   5, -12}, {   5, -10}, {   5, -10}, {   5, -12}, {   0, -15}, {  -5, -30},
    { -10, -45}, { -10, -30}, {   0, -25}, {  -2, -20}, {  -2, -20}, {   0, -25}, { -10, -30}, { -10, -45}
};
const PhaseScore Evaluation::king_pst[64] = {
    { 150,  -1}, { 180,  20}, { 150,  50}, { 100,  45}, { 100,  45}, { 150,  50}, { 180,  20}, { 150,  -1},
    { 150,  25}, { 165,  60}, { 130,  80}, { 100,  80}, { 100,  80}, { 130,  80}, { 165,  60}, { 150,  25},
    { 100,  50}, { 140,  80}, {  90, 110}, {  70, 120}, {  70, 120}, {  90, 110}, { 140,  80}, { 100,  50},
    {  90,  60}, { 110, 100}, {  80, 120}, {  50, 120}, {  50, 120}, {  80, 120}, { 110, 100}, {  90,  60},
    {  80,  55}, { 100, 110}, {  60, 140}, {  40, 140}, {  40, 140}, {  60, 140}, { 100, 110}, {  80,  55},
    {  70,  50}, {  80, 120}, {  40, 130}, {  10, 140}, {  10, 140}, {  40, 130}, {  80, 120}, {  70,  50},
    {  50,  20}, {  70,  70}, {  40,  70}, {  15,  80}, {  15,  80}, {  40,  70}, {  70,  70}, {  50,  20},
    {  30,   0}, {  50,  30}, {  20,  40}, {   0,  45}, {   0,  45}, {  20,  40}, {  50,  30}, {  30,   0}
};
const PhaseScore* Evaluation::pst_all[6] = {pawn_pst, knight_pst, bishop_pst, rook_pst, queen_pst, king_pst};

const PhaseScore Evaluation::TEMPO_BONUS                   = {15, 15};
const PhaseScore Evaluation::BISHOP_PAIR_BONUS             = {27, 75};
const PhaseScore Evaluation::PROTECTED_PAWN_BONUS          = {8, 13};
const PhaseScore Evaluation::ISOLATED_PAWN_PENALTY         = {-13, -21};
const PhaseScore Evaluation::DOUBLED_PAWN_PENALTY          = {-11, -18};
const PhaseScore Evaluation::BACKWARD_PAWN_PENALTY         = {-9, -14};
const PhaseScore Evaluation::KNIGHT_MOBILITY_BONUS         = {2, 3};
const PhaseScore Evaluation::BISHOP_MOBILITY_BONUS         = {3, 4};
const PhaseScore Evaluation::ROOK_MOBILITY_BONUS           = {3, 5};
const PhaseScore Evaluation::QUEEN_MOBILITY_BONUS          = {2, 3};
const PhaseScore Evaluation::KNIGHT_OUTPOST_BONUS          = {30, 20};
const PhaseScore Evaluation::BISHOP_OUTPOST_BONUS          = {25, 18};
const PhaseScore Evaluation::ROOK_ON_OPEN_FILE             = {28, 12};
const PhaseScore Evaluation::ROOK_ON_SEMI_OPEN_FILE        = {11, 8};
const PhaseScore Evaluation::RookOnEnemyTerritoryBonus     = {20, 28};
const PhaseScore Evaluation::ConnectedRooksOnTerritoryBonus  = {5, 8};
const PhaseScore Evaluation::passed_pawn_bonus[8] = {{0,0}, {5,12}, {15,28}, {25,42}, {40,65}, {60,95}, {80,125}, {0,0}};
const PhaseScore Evaluation::THREAT_BY_MINOR[7] = {{0,0},{1,9},{16,12},{20,14},{25,32},{20,40},{0,0}};
const PhaseScore Evaluation::THREAT_BY_ROOK[7] = {{0,0},{0,11},{9,17},{11,14},{0,9},{15,9},{0,0}};
const PhaseScore Evaluation::THREAT_BY_KING = {6, 21};
const PhaseScore Evaluation::HANGING_PIECE_BONUS = {18, 10};
const PhaseScore Evaluation::PhalanxPawnBonus[8][8] = {
    {{ 0, 0}, { 1, 4}, { 2, 7}, { 4, 7}, {14, 16}, {32, 38}, {65, 91}, { 0, 0}}, {{ 0, 0}, { 1, 4}, { 2, 7}, { 6, 12}, {17, 20}, {35, 46}, {76, 104}, { 0, 0}},
    {{ 0, 0}, { 1, 4}, { 2, 7}, { 7, 12}, {20, 20}, {37, 46}, {78, 104}, { 0, 0}}, {{ 0, 0}, { 2, 4}, { 4, 7}, {12, 12}, {26, 20}, {42, 46}, {84, 104}, { 0, 0}},
    {{ 0, 0}, { 2, 4}, { 4, 7}, {12, 12}, {26, 20}, {42, 46}, {84, 104}, { 0, 0}}, {{ 0, 0}, { 1, 4}, { 2, 7}, { 7, 12}, {20, 20}, {37, 46}, {78, 104}, { 0, 0}},
    {{ 0, 0}, { 1, 4}, { 2, 7}, { 6, 12}, {17, 20}, {35, 46}, {76, 104}, { 0, 0}}, {{ 0, 0}, { 1, 4}, { 2, 7}, { 4, 7}, {14, 16}, {32, 38}, {65, 91}, { 0, 0}}
};
const int Evaluation::SHIELD_PAWN_PRESENT_BONUS = 10;
const int Evaluation::SHIELD_PAWN_MISSING_PENALTY = -20;
const int Evaluation::SHIELD_PAWN_ADVANCED_PENALTY = -12;
const int Evaluation::SHIELD_OPEN_FILE_PENALTY = -15;
const int Evaluation::SafetyKnightWeight = 32, Evaluation::SafetyBishopWeight = 19, Evaluation::SafetyRookWeight = 27, Evaluation::SafetyQueenWeight = 23, Evaluation::SafetyAttackValue = 32,
          Evaluation::SafetyWeakSquares = 39, Evaluation::SafetySafeQueenCheck = 66, Evaluation::SafetySafeRookCheck = 61, Evaluation::SafetySafeBishopCheck = 50, Evaluation::SafetySafeKnightCheck = 58,
          Evaluation::SafetyAdjustment = -63;

void Evaluation::init_eval_masks() {
    for (int f = 0; f < 8; ++f) {
        file_bb_mask[f] = 0;
        for (int r = 0; r < 8; ++r) file_bb_mask[f] |= Bitboard::set_bit(r * 8 + f);
        adjacent_files_mask[f] = 0;
        if (f > 0) adjacent_files_mask[f] |= file_bb_mask[f-1];
        if (f < 7) adjacent_files_mask[f] |= file_bb_mask[f+1];
    }
    for (int r = 0; r < 8; ++r) {
        rank_bb_mask[r] = 0;
        for (int f = 0; f < 8; ++f) rank_bb_mask[r] |= Bitboard::set_bit(r * 8 + f);
    }
    for (int sq = 0; sq < 64; ++sq) {
        white_passed_pawn_block_mask[sq] = 0;
        black_passed_pawn_block_mask[sq] = 0;
        pawn_attack_shield_mask[WHITE][sq] = 0;
        pawn_attack_shield_mask[BLACK][sq] = 0;
        int r = Bitboard::rank_of(sq), f = Bitboard::file_of(sq);
        for (int cur_r = r + 1; cur_r < 8; ++cur_r) {
            white_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + f);
            if (f > 0) white_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + (f - 1));
            if (f < 7) white_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + (f + 1));
        }
        for (int cur_r = r - 1; cur_r >= 0; --cur_r) {
            black_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + f);
            if (f > 0) black_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + (f - 1));
            if (f < 7) black_passed_pawn_block_mask[sq] |= Bitboard::set_bit(cur_r * 8 + (f + 1));
        }
        pawn_attack_shield_mask[WHITE][sq] = black_passed_pawn_block_mask[sq] & adjacent_files_mask[f];
        pawn_attack_shield_mask[BLACK][sq] = white_passed_pawn_block_mask[sq] & adjacent_files_mask[f];
    }
}

bool Evaluation::is_insufficient_material(const Position& pos) {
    if (pos.piece_bb[PAWN] != 0 || pos.piece_bb[ROOK] != 0 || pos.piece_bb[QUEEN] != 0) return false;
    int white_minors = Bitboard::pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[WHITE]) + Bitboard::pop_count(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]);
    int black_minors = Bitboard::pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[BLACK]) + Bitboard::pop_count(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]);
    if (white_minors + black_minors <= 1) return true;
    if (white_minors == 2 && Bitboard::pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[WHITE]) == 2 && black_minors == 0) return true;
    if (black_minors == 2 && Bitboard::pop_count(pos.piece_bb[KNIGHT] & pos.color_bb[BLACK]) == 2 && white_minors == 0) return true;
    return false;
}

void Evaluation::evaluate_pawn_structure_for_color(const Position& pos, Color c, PhaseScore& score, uint64_t& passed_pawns) {
    uint64_t friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[c];
    uint64_t enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1 - c];
    uint64_t temp_pawns = friendly_pawns;
    uint64_t enemy_pawn_attacks = 0;
    uint64_t temp_enemy_pawns = enemy_pawns;
    while(temp_enemy_pawns) {
        int sq = Bitboard::lsb_index(temp_enemy_pawns);
        enemy_pawn_attacks |= g_attack_tables.pawn_attacks[1 - c][sq];
        temp_enemy_pawns &= temp_enemy_pawns - 1;
    }

    while (temp_pawns) {
        int sq = Bitboard::lsb_index(temp_pawns);
        temp_pawns &= temp_pawns - 1;
        int f = Bitboard::file_of(sq);

        if ((adjacent_files_mask[f] & friendly_pawns) == 0) score += ISOLATED_PAWN_PENALTY;
        if (g_attack_tables.pawn_attacks[1-c][sq] & friendly_pawns) score += PROTECTED_PAWN_BONUS;
        if ((file_bb_mask[f] & ( (c == WHITE) ? Bitboard::north(Bitboard::set_bit(sq)) : Bitboard::south(Bitboard::set_bit(sq)) ) & friendly_pawns) != 0) score += DOUBLED_PAWN_PENALTY;
        if (Bitboard::get_bit(Bitboard::east(Bitboard::set_bit(sq)), friendly_pawns)) {
            int rank_idx = (c == WHITE) ? Bitboard::rank_of(sq) : (7 - Bitboard::rank_of(sq));
            score += PhalanxPawnBonus[f][rank_idx];
        }
        uint64_t front_span = (c == WHITE) ? white_passed_pawn_block_mask[sq] : black_passed_pawn_block_mask[sq];
        if ((front_span & (adjacent_files_mask[f] & friendly_pawns)) == 0) {
            int push_sq = (c == WHITE) ? sq + 8 : sq - 8;
            if (Bitboard::get_bit(enemy_pawn_attacks, push_sq)) score += BACKWARD_PAWN_PENALTY;
        }

        bool is_passed = (c == WHITE) ? ((white_passed_pawn_block_mask[sq] & enemy_pawns) == 0) : ((black_passed_pawn_block_mask[sq] & enemy_pawns) == 0);
        if (is_passed) {
            passed_pawns |= Bitboard::set_bit(sq);
            score += passed_pawn_bonus[(c == WHITE) ? Bitboard::rank_of(sq) : (7-Bitboard::rank_of(sq))];
        }
    }
}

PhaseScore Evaluation::evaluate_threats_for_color(const Position& pos, Color us, const uint64_t attackedBy[2][7], const uint64_t attackedBy2[2]) {
    PhaseScore score = {};
    const Color them = (Color)(1 - us);
    uint64_t non_pawn_enemies = pos.color_bb[them] & ~pos.piece_bb[PAWN];
    uint64_t strongly_protected_by_them = attackedBy[them][PAWN] | (attackedBy2[them] & ~attackedBy2[us]);
    uint64_t weak_enemy_pieces = pos.color_bb[them] & ~strongly_protected_by_them & attackedBy[us][6];
    uint64_t all_targets = weak_enemy_pieces | (non_pawn_enemies & strongly_protected_by_them);

    if (!all_targets) return score;

    uint64_t b = all_targets & (attackedBy[us][KNIGHT] | attackedBy[us][BISHOP]);
    while(b) {
        int sq = Bitboard::lsb_index(b); b &= b-1;
        score += THREAT_BY_MINOR[pos.piece_on_sq(sq)];
    }
    b = weak_enemy_pieces & attackedBy[us][ROOK];
    while(b) {
        int sq = Bitboard::lsb_index(b); b &= b - 1;
        score += THREAT_BY_ROOK[pos.piece_on_sq(sq)];
    }
    if (weak_enemy_pieces & attackedBy[us][KING]) score += THREAT_BY_KING;
    
    score += HANGING_PIECE_BONUS * Bitboard::pop_count(weak_enemy_pieces & (~attackedBy[them][6] | (non_pawn_enemies & attackedBy2[us])));
    return score;
}

int Evaluation::get_endgame_material_modifier(const Position& pos, const PhaseScore& score) {
    constexpr uint64_t LIGHT_SQUARES = 0x55AA55AA55AA55AAULL;
    if ((pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) != 0) {
         if (Bitboard::pop_count(pos.color_bb[WHITE]) > 3 && Bitboard::pop_count(pos.color_bb[BLACK]) > 3) return 256;
    }
    if (Bitboard::pop_count(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]) == 1 && Bitboard::pop_count(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]) == 1) {
        bool w_light = Bitboard::get_bit(LIGHT_SQUARES, Bitboard::lsb_index(pos.piece_bb[BISHOP] & pos.color_bb[WHITE]));
        bool b_light = Bitboard::get_bit(LIGHT_SQUARES, Bitboard::lsb_index(pos.piece_bb[BISHOP] & pos.color_bb[BLACK]));
        if (w_light != b_light) return 140;
    }
    int stronger_side = (score.mg + score.eg > 0) ? WHITE : BLACK;
    if ((pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) == 0 && (Bitboard::pop_count(pos.piece_bb[PAWN] & pos.color_bb[stronger_side]) - Bitboard::pop_count(pos.piece_bb[PAWN] & pos.color_bb[1-stronger_side])) <= 1) {
        return 160;
    }
    return std::min(256, 192 + Bitboard::pop_count(pos.piece_bb[PAWN] & pos.color_bb[stronger_side]) * 20);
}

int Evaluation::evaluate(Position& pos, PawnCache& pawn_cache) {
    if (is_insufficient_material(pos)) return 0;
    PhaseScore final_score;
    int game_phase = 0;

    uint64_t white_passed_pawns = 0, black_passed_pawns = 0;
    PhaseScore pawn_score;
    if (!pawn_cache.probe(pos.pawn_zobrist_key, pawn_score, white_passed_pawns, black_passed_pawns)) {
        PhaseScore w_pawn_score, b_pawn_score;
        evaluate_pawn_structure_for_color(pos, WHITE, w_pawn_score, white_passed_pawns);
        evaluate_pawn_structure_for_color(pos, BLACK, b_pawn_score, black_passed_pawns);
        pawn_score = w_pawn_score - b_pawn_score;
        pawn_cache.store(pos.pawn_zobrist_key, pawn_score, white_passed_pawns, black_passed_pawns);
    }
    final_score += pawn_score;

    uint64_t attackedBy[2][7] = {{0}}, attackedBy2[2] = {0};
    uint64_t king_areas[2];
    int king_attackers_count[2] = {0}, king_attackers_weight[2] = {0};
    uint64_t occupied = pos.get_occupied_bb();

    for (Color c : {WHITE, BLACK}) {
        uint64_t temp_pawns = pos.piece_bb[PAWN] & pos.color_bb[c];
        while (temp_pawns) { int sq = Bitboard::lsb_index(temp_pawns); temp_pawns &= temp_pawns - 1; attackedBy[c][PAWN] |= g_attack_tables.pawn_attacks[c][sq]; }
        int king_sq = Bitboard::lsb_index(pos.piece_bb[KING] & pos.color_bb[c]);
        if (king_sq != -1) { attackedBy[c][KING] = g_attack_tables.king_attacks[king_sq]; king_areas[c] = g_attack_tables.king_attacks[king_sq]; }
        attackedBy[c][6] = attackedBy[c][PAWN] | attackedBy[c][KING];
        attackedBy2[c] = attackedBy[c][PAWN] & attackedBy[c][KING];
        for (Piece p_type : {KNIGHT, BISHOP, ROOK, QUEEN}) {
            uint64_t pieces = pos.piece_bb[p_type] & pos.color_bb[c];
            while (pieces) {
                int sq = Bitboard::lsb_index(pieces); pieces &= pieces - 1;
                uint64_t attacks = 0;
                if (p_type == KNIGHT) attacks = g_attack_tables.knight_attacks[sq];
                else if (p_type == BISHOP) attacks = g_attack_tables.get_bishop_attacks(sq, occupied);
                else if (p_type == ROOK) attacks = g_attack_tables.get_rook_attacks(sq, occupied);
                else if (p_type == QUEEN) attacks = g_attack_tables.get_queen_attacks(sq, occupied);
                attackedBy[c][p_type] |= attacks;
                attackedBy2[c] |= attackedBy[c][6] & attacks;
                attackedBy[c][6] |= attacks;
            }
        }
    }
    
    for (Color c : {WHITE, BLACK}) {
        PhaseScore current_color_score;
        uint64_t friendly_pawns = pos.piece_bb[PAWN] & pos.color_bb[c];
        uint64_t enemy_pawns = pos.piece_bb[PAWN] & pos.color_bb[1-c];
        
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[c];
            game_phase += Bitboard::pop_count(b) * game_phase_inc[p];
            while (b) {
                int sq = Bitboard::lsb_index(b); b &= b - 1;
                int mirrored_sq = (c == WHITE) ? sq : (63 - sq);
                current_color_score += piece_phase_values[p];
                current_color_score += pst_all[p][mirrored_sq];

                if ((Piece)p != PAWN && (Piece)p != KING) {
                    uint64_t mobility = 0;
                    if ((Piece)p == KNIGHT) mobility = g_attack_tables.knight_attacks[sq];
                    else if ((Piece)p == BISHOP) mobility = g_attack_tables.get_bishop_attacks(sq, occupied);
                    else if ((Piece)p == ROOK) mobility = g_attack_tables.get_rook_attacks(sq, occupied);
                    else if ((Piece)p == QUEEN) mobility = g_attack_tables.get_queen_attacks(sq, occupied);
                    int mobility_count = Bitboard::pop_count(mobility & ~(pos.color_bb[c] | attackedBy[1-c][PAWN]));
                    if ((Piece)p == KNIGHT) current_color_score += KNIGHT_MOBILITY_BONUS * mobility_count;
                    else if ((Piece)p == BISHOP) current_color_score += BISHOP_MOBILITY_BONUS * mobility_count;
                    else if ((Piece)p == ROOK) current_color_score += ROOK_MOBILITY_BONUS * mobility_count;
                    else if ((Piece)p == QUEEN) current_color_score += QUEEN_MOBILITY_BONUS * mobility_count;
                }
                if ((Piece)p == ROOK) {
                    int f = Bitboard::file_of(sq);
                    if ((file_bb_mask[f] & friendly_pawns) == 0) {
                        if ((file_bb_mask[f] & enemy_pawns) == 0) current_color_score += ROOK_ON_OPEN_FILE;
                        else current_color_score += ROOK_ON_SEMI_OPEN_FILE;
                    }
                     if (Bitboard::rank_of(sq) == ((c == WHITE) ? 6 : 1) && (pos.piece_bb[KING] & pos.color_bb[1-c] | pos.piece_bb[PAWN] & pos.color_bb[1-c]) & rank_bb_mask[((c == WHITE) ? 6 : 1)]){
                         current_color_score += RookOnEnemyTerritoryBonus;
                         if(g_attack_tables.get_rook_attacks(sq, occupied) & (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]) & pos.color_bb[c])
                            current_color_score += ConnectedRooksOnTerritoryBonus;
                     }
                } else if ((Piece)p == KNIGHT || (Piece)p == BISHOP) {
                     int rank_idx = (c == WHITE) ? Bitboard::rank_of(sq) : 7 - Bitboard::rank_of(sq);
                     if (rank_idx >= 3 && Bitboard::get_bit(g_attack_tables.pawn_attacks[1-c][sq], friendly_pawns) && !(pawn_attack_shield_mask[c][sq] & enemy_pawns)) {
                        current_color_score += ((Piece)p == KNIGHT ? KNIGHT_OUTPOST_BONUS : BISHOP_OUTPOST_BONUS);
                     }
                }
                uint64_t attacks_on_king_zone = 0;
                if ((Piece)p == KNIGHT) attacks_on_king_zone = g_attack_tables.knight_attacks[sq];
                else if ((Piece)p == BISHOP) attacks_on_king_zone = g_attack_tables.get_bishop_attacks(sq, occupied);
                else if ((Piece)p == ROOK) attacks_on_king_zone = g_attack_tables.get_rook_attacks(sq, occupied);
                else if ((Piece)p == QUEEN) attacks_on_king_zone = g_attack_tables.get_queen_attacks(sq, occupied);
                if (attacks_on_king_zone & king_areas[1-c]) {
                    king_attackers_count[1-c]++;
                    if ((Piece)p == KNIGHT) king_attackers_weight[1-c] += SafetyKnightWeight; else if ((Piece)p == BISHOP) king_attackers_weight[1-c] += SafetyBishopWeight;
                    else if ((Piece)p == ROOK) king_attackers_weight[1-c] += SafetyRookWeight; else if ((Piece)p == QUEEN) king_attackers_weight[1-c] += SafetyQueenWeight;
                }
            }
        }
        if (Bitboard::pop_count(pos.piece_bb[BISHOP] & pos.color_bb[c]) >= 2) current_color_score += BISHOP_PAIR_BONUS;
        current_color_score += evaluate_threats_for_color(pos, c, attackedBy, attackedBy2);

        int king_sq = Bitboard::lsb_index(pos.piece_bb[KING] & pos.color_bb[c]);
        if (king_sq != -1) {
            int shelter_score = 0, king_file = Bitboard::file_of(king_sq);
            for (int f = std::max(0, king_file - 1); f <= std::min(7, king_file + 1); ++f) {
                if ((file_bb_mask[f] & friendly_pawns) == 0) shelter_score += SHIELD_PAWN_MISSING_PENALTY;
            }
            if (Bitboard::rank_of(king_sq) == ((c == WHITE) ? 0 : 7)) current_color_score.mg += shelter_score;
            if (king_attackers_count[c] > 1) {
                uint64_t weak_sqs = attackedBy[1-c][6] & ~attackedBy2[c] & (~attackedBy[c][6] | attackedBy[c][QUEEN] | attackedBy[c][KING]);
                uint64_t safe_sqs = ~pos.color_bb[c] & (~attackedBy[c][6] | (weak_sqs & attackedBy2[1-c]));
                int safety_score = king_attackers_weight[c] + SafetyAttackValue * Bitboard::pop_count(attackedBy[1-c][6] & king_areas[c])
                    + SafetyWeakSquares * Bitboard::pop_count(weak_sqs & king_areas[c])
                    + SafetySafeQueenCheck * Bitboard::pop_count((g_attack_tables.get_bishop_attacks(king_sq, occupied) | g_attack_tables.get_rook_attacks(king_sq, occupied)) & safe_sqs & attackedBy[1-c][QUEEN])
                    + SafetyAdjustment;
                current_color_score.mg -= safety_score * safety_score / 716;
            }
        }
        if (c == WHITE) final_score += current_color_score;
        else final_score -= current_color_score;
    }
    final_score += (pos.side_to_move == WHITE ? TEMPO_BONUS : -TEMPO_BONUS);
    game_phase = std::min(game_phase, 24);
    if (game_phase < 16) final_score.eg = (final_score.eg * get_endgame_material_modifier(pos, final_score)) / 256;
    int final_eval = (final_score.mg * game_phase + final_score.eg * (24 - game_phase)) / 24;
    pos.static_eval = (pos.side_to_move == WHITE) ? final_eval : -final_eval;
    return pos.static_eval;
}

// Global instance of evaluation class
Evaluation g_evaluator;

// ============================================================================
// 9. SEARCH
// ============================================================================

class Search {
    // Inner class for move ordering
    class MovePicker {
    public:
        MovePicker(const Search& searcher, const Position& pos, int ply, const Move& tt_move, const Move& prev_move, uint64_t threats, bool quiescence);
        Move next_move();
    private:
        void score_moves(uint64_t threats);
        const Search& m_searcher;
        const Position& m_pos;
        int m_ply;
        bool m_quiescence_mode;
        Move m_moves[256];
        int m_move_count;
        int m_current_idx;
    };

public:
    Search() {
        reset_search_heuristics();
    }
    
    void new_game() {
        m_tt.init();
        m_tt.clear();
        m_pawn_cache.clear();
        reset_search_heuristics();
    }
    
    void set_tt_size(size_t mb) { m_tt.set_size(mb); }

    Move run_iterative_deepening(Position& root_pos, long long soft_limit_ms, long long hard_limit_ms, int max_depth);

private:
    TranspositionTable m_tt;
    PawnCache m_pawn_cache;
    Evaluation m_evaluator;

    // Search state
    std::chrono::steady_clock::time_point m_start_timepoint, m_soft_limit_timepoint, m_hard_limit_timepoint;
    bool m_stop_search = false;
    bool m_use_time_limits = false;
    uint64_t m_nodes_searched = 0;
    
    // Heuristics
    Move m_killer_moves[MAX_PLY][2];
    int16_t m_history_score[2][2][2][64][64]; // [color][from_threat][to_threat][from][to]
    Move m_refutation_moves[64][64];
    int m_search_reductions[MAX_PLY][256];
    int m_late_move_pruning_counts[2][MAX_PLY];
    static const int HISTORY_DIVISOR = 24000;
    
    // Path-dependent data
    uint64_t m_search_path_hashes[MAX_PLY];
    int m_search_path_evals[MAX_PLY];
    std::vector<uint64_t> m_game_history;

    // Core search functions
    int alpha_beta_search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv, bool can_null, const Move& prev_move);
    int quiescence_search(Position& pos, int alpha, int beta, int ply);

    // Helper functions
    void reset_search_heuristics();
    bool check_time();
    int see(const Position& pos, const Move& move) const;
};

// --- Move Picker Implementation ---
Search::MovePicker::MovePicker(const Search& searcher, const Position& pos, int ply, const Move& tt_move, const Move& prev_move, uint64_t threats, bool quiescence)
    : m_searcher(searcher), m_pos(pos), m_ply(ply), m_quiescence_mode(quiescence), m_current_idx(0)
{
    m_move_count = generate_moves(m_pos, m_moves, quiescence);
    if (quiescence) {
        for (int i = 0; i < m_move_count; ++i) m_moves[i].score = m_searcher.see(pos, m_moves[i]);
    } else {
        Move killer1 = (ply < MAX_PLY) ? m_searcher.m_killer_moves[ply][0] : NULL_MOVE;
        Move killer2 = (ply < MAX_PLY) ? m_searcher.m_killer_moves[ply][1] : NULL_MOVE;
        Move counter = (ply > 0 && !prev_move.is_null()) ? m_searcher.m_refutation_moves[prev_move.from][prev_move.to] : NULL_MOVE;
        for (int i = 0; i < m_move_count; ++i) {
            Move& m = m_moves[i];
            if (m == tt_move) { m.score = 3000000; continue; }
            if (pos.piece_on_sq(m.to) != NO_PIECE || (pos.piece_on_sq(m.from) == PAWN && m.to == pos.ep_square)) {
                int see_val = m_searcher.see(pos, m);
                m.score = (see_val >= 0) ? (2000000 + see_val) : (-1000000 + see_val);
            } else {
                if (m == killer1) m.score = 1000000;
                else if (m == killer2) m.score = 900000;
                else if (m == counter) m.score = 800000;
                else {
                    bool t_from = Bitboard::get_bit(threats, m.from), t_to = Bitboard::get_bit(threats, m.to);
                    m.score = m_searcher.m_history_score[pos.side_to_move][t_from][t_to][m.from][m.to];
                }
            }
        }
    }
}

Move Search::MovePicker::next_move() {
    if (m_current_idx >= m_move_count) return NULL_MOVE;
    int best_idx = m_current_idx;
    for (int i = m_current_idx + 1; i < m_move_count; ++i)
        if (m_moves[i].score > m_moves[best_idx].score) best_idx = i;
    std::swap(m_moves[m_current_idx], m_moves[best_idx]);
    return m_moves[m_current_idx++];
}

// --- Search Implementation ---
void Search::reset_search_heuristics() {
    std::memset(m_killer_moves, 0, sizeof(m_killer_moves));
    std::memset(m_history_score, 0, sizeof(m_history_score));
    std::memset(m_refutation_moves, 0, sizeof(m_refutation_moves));
    for (int d = 1; d < MAX_PLY; ++d) {
        for (int m = 1; m < 256; ++m) {
            int r = static_cast<int>((log(d) * log(m)) / 2.3);
            if (d < 3 || m < 2) r = 0;
            if (d > 8 && m > 4) r++;
            m_search_reductions[d][m] = std::min(r, d - 2);
        }
        m_late_move_pruning_counts[0][d] = 2 + (d * d) / 2;
        m_late_move_pruning_counts[1][d] = 4 + d * d;
    }
}

bool Search::check_time() {
    if (m_stop_search) return true;
    if ((m_nodes_searched & 2047) == 0 && m_use_time_limits) {
        if (std::chrono::steady_clock::now() > m_hard_limit_timepoint) {
            m_stop_search = true;
            return true;
        }
    }
    return false;
}

int Search::see(const Position& pos, const Move& move) const {
    const int see_piece_values[7] = {100, 325, 335, 510, 925, 10000, 0};
    int gain[32] = {0};
    uint64_t from_bb = Bitboard::set_bit(move.from);
    uint64_t occupied = pos.get_occupied_bb();
    Color stm = pos.color_on_sq(move.from);
    gain[0] = move.promotion != NO_PIECE ? see_piece_values[move.promotion] : see_piece_values[pos.piece_on_sq(move.to)];
    
    auto get_attackers = [&](int sq, uint64_t occ) {
        return (g_attack_tables.pawn_attacks[WHITE][sq] & pos.piece_bb[PAWN] & pos.color_bb[BLACK])
             | (g_attack_tables.pawn_attacks[BLACK][sq] & pos.piece_bb[PAWN] & pos.color_bb[WHITE])
             | (g_attack_tables.knight_attacks[sq] & pos.piece_bb[KNIGHT]) | (g_attack_tables.king_attacks[sq] & pos.piece_bb[KING])
             | (g_attack_tables.get_bishop_attacks(sq, occ) & (pos.piece_bb[BISHOP] | pos.piece_bb[QUEEN]))
             | (g_attack_tables.get_rook_attacks(sq, occ) & (pos.piece_bb[ROOK] | pos.piece_bb[QUEEN]));
    };
    
    uint64_t attackers = get_attackers(move.to, occupied);
    Piece attacking_piece_type = pos.piece_on_sq(move.from);
    int d = 1;
    occupied ^= from_bb;

    while (true) {
        stm = (Color)(1-stm);
        uint64_t side_attackers = attackers & pos.color_bb[stm];
        if (!side_attackers) break;
        
        Piece next_attacker = NO_PIECE;
        for (int p_type = PAWN; p_type <= KING; ++p_type) {
            if (side_attackers & pos.piece_bb[p_type]) {
                next_attacker = (Piece)p_type;
                break;
            }
        }
        gain[d] = see_piece_values[attacking_piece_type] - gain[d - 1];
        if (std::max(-gain[d-1], gain[d]) < 0) break;

        from_bb = Bitboard::set_bit(Bitboard::lsb_index(side_attackers & pos.piece_bb[next_attacker]));
        occupied ^= from_bb;
        attackers ^= from_bb;
        attackers |= get_attackers(move.to, occupied); // Recalculate revealed attackers
        attacking_piece_type = next_attacker;
        d++;
    }
    while (--d) gain[d-1] = -std::max(-gain[d-1], gain[d]);
    return gain[0];
}

int Search::quiescence_search(Position& pos, int alpha, int beta, int ply) {
    m_nodes_searched++;
    if (check_time() || ply >= MAX_PLY - 1) return m_evaluator.evaluate(pos, m_pawn_cache);

    bool in_check = is_square_attacked(pos, Bitboard::lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    int stand_pat = in_check ? -INF_SCORE + ply : m_evaluator.evaluate(pos, m_pawn_cache);
    if (stand_pat >= beta) return beta;
    if (alpha < stand_pat) alpha = stand_pat;
    
    MovePicker picker(*this, pos, ply, NULL_MOVE, NULL_MOVE, 0, true);
    Move move;
    int legal_moves = 0;
    while (!(move = picker.next_move()).is_null()) {
        if (!in_check && move.score < 0) continue;
        bool legal;
        Position next_pos = make_move(pos, move, legal);
        if (!legal) continue;
        legal_moves++;
        int score = -quiescence_search(next_pos, -beta, -alpha, ply + 1);
        if (score >= beta) return beta;
        if (score > alpha) alpha = score;
    }
    if (in_check && legal_moves == 0) return -MATE_SCORE + ply;
    return alpha;
}

int Search::alpha_beta_search(Position& pos, int depth, int alpha, int beta, int ply, bool is_pv, bool can_null, const Move& prev_move) {
    m_search_path_hashes[ply] = pos.zobrist_hash;
    m_nodes_searched++;
    if (check_time()) return 0;
    if (ply >= MAX_PLY - 1) return m_evaluator.evaluate(pos, m_pawn_cache);
    if (ply > 0 && pos.halfmove_clock >= 100) return 0;

    for (int i = ply - 2; i >= 0; i -= 2) if (m_search_path_hashes[i] == pos.zobrist_hash) return 0;
    for (const auto& hash : m_game_history) if (hash == pos.zobrist_hash) return 0;
    if (ply > 0 && m_evaluator.is_insufficient_material(pos)) return 0;
    
    bool in_check = is_square_attacked(pos, Bitboard::lsb_index(pos.piece_bb[KING] & pos.color_bb[pos.side_to_move]), 1 - pos.side_to_move);
    if (in_check) depth++;
    if (depth <= 0) return quiescence_search(pos, alpha, beta, ply);

    int original_alpha = alpha;
    Move tt_move = NULL_MOVE;
    int tt_score, tt_eval = NO_EVAL_STORED;
    if (m_tt.probe(pos.zobrist_hash, depth, ply, alpha, beta, tt_move, tt_score, tt_eval)) return tt_score;

    int static_eval = (tt_eval != NO_EVAL_STORED) ? tt_eval : m_evaluator.evaluate(pos, m_pawn_cache);
    m_search_path_evals[ply] = static_eval;

    if (!is_pv && !in_check) {
        if (depth < 8 && static_eval - (70 + 50 * depth) >= beta) return beta;
        if (depth < 4 && static_eval + (182 + 78 * (depth - 1)) < alpha) {
            int q_score = quiescence_search(pos, alpha, beta, ply);
            if (q_score < alpha) return alpha;
        }
    }

    if (!is_pv && !in_check && can_null && depth >= 3 && ply > 0 && (pos.color_bb[pos.side_to_move] & ~(pos.piece_bb[PAWN] | pos.piece_bb[KING])) && static_eval >= beta) {
        Position null_next_pos = pos;
        null_next_pos.side_to_move = 1 - pos.side_to_move;
        null_next_pos.zobrist_hash ^= Zobrist::side_to_move;
        if (pos.ep_square != -1) { null_next_pos.zobrist_hash ^= Zobrist::ep[pos.ep_square]; null_next_pos.ep_square = -1; null_next_pos.zobrist_hash ^= Zobrist::ep[64]; }
        null_next_pos.ply++;
        int null_score = -alpha_beta_search(null_next_pos, depth - 1 - (3 + (depth > 6)), -beta, -beta + 1, ply + 1, false, false, NULL_MOVE);
        if (m_stop_search) return 0;
        if (null_score >= beta) { m_tt.store(pos.zobrist_hash, depth, ply, beta, TT_LOWER, NULL_MOVE, static_eval); return beta; }
    }
    
    uint64_t threats = 0; // Not used in this simplified picker logic. Pass 0.
    MovePicker picker(*this, pos, ply, tt_move, prev_move, threats, false);
    Move move, best_move = NULL_MOVE;
    int legal_moves = 0, best_score = -INF_SCORE;

    while (!(move = picker.next_move()).is_null()) {
        bool legal;
        Position next_pos = make_move(pos, move, legal);
        if (!legal) continue;
        legal_moves++;

        bool is_quiet = pos.piece_on_sq(move.to) == NO_PIECE && move.promotion == NO_PIECE;
        if (is_quiet && !in_check && best_score > -MATE_THRESHOLD && depth <= 3 && static_eval + (125*depth) < alpha) continue;

        int score;
        if (legal_moves == 1) {
            score = -alpha_beta_search(next_pos, depth - 1, -beta, -alpha, ply + 1, is_pv, true, move);
        } else {
            int lmr = 0;
            if (depth >= 3 && is_quiet) {
                bool improving = (ply >= 2 && !in_check) ? (static_eval > m_search_path_evals[ply-2]) : false;
                lmr = m_search_reductions[depth][legal_moves];
                if (!improving) lmr++;
                if (is_pv) lmr--;
                lmr = std::max(0, lmr);
            }
            score = -alpha_beta_search(next_pos, depth - 1 - lmr, -alpha - 1, -alpha, ply + 1, false, true, move);
            if (score > alpha && lmr > 0) score = -alpha_beta_search(next_pos, depth - 1, -alpha - 1, -alpha, ply + 1, false, true, move);
            if (score > alpha && score < beta) score = -alpha_beta_search(next_pos, depth - 1, -beta, -alpha, ply + 1, false, true, move);
        }
        if (m_stop_search) return 0;
        if (score > best_score) {
            best_score = score;
            best_move = move;
            if (score > alpha) {
                alpha = score;
                if (score >= beta) {
                    if (is_quiet) {
                        if (!(move == m_killer_moves[ply][0])) { m_killer_moves[ply][1] = m_killer_moves[ply][0]; m_killer_moves[ply][0] = move; }
                        if (ply > 0 && !prev_move.is_null()) m_refutation_moves[prev_move.from][prev_move.to] = move;
                        int bonus = depth * depth;
                        m_history_score[pos.side_to_move][0][0][move.from][move.to] += bonus - (m_history_score[pos.side_to_move][0][0][move.from][move.to] * std::abs(bonus) / HISTORY_DIVISOR);
                    }
                    m_tt.store(pos.zobrist_hash, depth, ply, beta, TT_LOWER, best_move, static_eval);
                    return beta;
                }
            }
        }
    }
    if (legal_moves == 0) return in_check ? (-MATE_SCORE + ply) : 0;
    TTBound bound = (best_score > original_alpha) ? TT_EXACT : TT_UPPER;
    m_tt.store(pos.zobrist_hash, depth, ply, best_score, bound, best_move, static_eval);
    return best_score;
}

Move Search::run_iterative_deepening(Position& root_pos, long long soft_limit_ms, long long hard_limit_ms, int max_depth) {
    m_start_timepoint = std::chrono::steady_clock::now();
    m_soft_limit_timepoint = m_start_timepoint + std::chrono::milliseconds(soft_limit_ms);
    m_hard_limit_timepoint = m_start_timepoint + std::chrono::milliseconds(hard_limit_ms);
    m_use_time_limits = (soft_limit_ms > 0 || hard_limit_ms > 0);
    m_stop_search = false;
    m_nodes_searched = 0;
    
    Move best_move_overall = NULL_MOVE;
    int last_score = 0;
    int aspiration_delta = 15;

    for (int d = 1; d <= max_depth; ++d) {
        int alpha = (d > 1) ? last_score - aspiration_delta : -INF_SCORE;
        int beta = (d > 1) ? last_score + aspiration_delta : INF_SCORE;
        
        int score = alpha_beta_search(root_pos, d, alpha, beta, 0, true, false, NULL_MOVE);

        if (!m_stop_search && (score <= alpha || score >= beta)) {
             score = alpha_beta_search(root_pos, d, -INF_SCORE, INF_SCORE, 0, true, false, NULL_MOVE);
        }

        if (m_stop_search && d > 1) break;
        
        Move tt_move; int dummy_s, dummy_e, dummy_a=-INF_SCORE, dummy_b=INF_SCORE;
        if (m_tt.probe(root_pos.zobrist_hash, d, 0, dummy_a, dummy_b, tt_move, dummy_s, dummy_e) && !tt_move.is_null()) {
            best_move_overall = tt_move;
        }

        auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - m_start_timepoint).count();
        std::cout << "info depth " << d << " score cp " << score;
        if (std::abs(score) > MATE_THRESHOLD) std::cout << " mate " << ((score > 0 ? MATE_SCORE - score : -MATE_SCORE - score)+1)/2;
        std::cout << " nodes " << m_nodes_searched << " time " << elapsed_ms << " nps " << (elapsed_ms > 0 ? m_nodes_searched * 1000 / elapsed_ms : 0) << " pv " << move_to_uci(best_move_overall) << std::endl;

        last_score = score;
        if (m_use_time_limits && std::chrono::steady_clock::now() > m_soft_limit_timepoint) break;
        if (std::abs(score) > MATE_THRESHOLD) break;
    }
    return best_move_overall;
}


// ============================================================================
// 10. MOVE GENERATION AND APPLICATION
// ============================================================================

// Definitions for functions that need full Position and AttackTables knowledge
// These were forward-declared earlier

uint64_t calculate_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c) {
        for (int p = PAWN; p <= KING; ++p) {
            uint64_t b = pos.piece_bb[p] & pos.color_bb[c];
            while (b) {
                int sq = Bitboard::lsb_index(b);
                b &= b - 1;
                h ^= Zobrist::pieces[c][p][sq];
            }
        }
    }
    h ^= Zobrist::castling[pos.castling_rights];
    h ^= Zobrist::ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    if (pos.side_to_move == BLACK) h ^= Zobrist::side_to_move;
    return h;
}

uint64_t calculate_pawn_zobrist_hash(const Position& pos) {
    uint64_t h = 0;
    for (int c = 0; c < 2; ++c) {
        uint64_t b = pos.piece_bb[PAWN] & pos.color_bb[c];
        while (b) {
            int sq = Bitboard::lsb_index(b);
            b &= b - 1;
            h ^= Zobrist::pieces[c][PAWN][sq];
        }
    }
    return h;
}

int generate_moves(const Position& pos, Move* moves_list, bool captures_only) {
    // This function's logic is large and remains unchanged.
    // It's placed here to have access to all necessary definitions.
    int move_count = 0;
    int stm = pos.side_to_move;
    uint64_t my_pieces = pos.color_bb[stm];
    uint64_t opp_pieces = pos.color_bb[1-stm];
    uint64_t occupied = my_pieces | opp_pieces;

    // Pawns
    uint64_t pawns = pos.piece_bb[PAWN] & my_pieces;
    while(pawns) {
        int from = Bitboard::lsb_index(pawns); pawns &= pawns-1;
        int rank = Bitboard::rank_of(from);
        
        // Pushes
        int one_step = from + (stm == WHITE ? 8 : -8);
        if (Bitboard::get_bit(~occupied, one_step)) {
            if (rank == (stm == WHITE ? 6 : 1)) {
                 moves_list[move_count++] = {from, one_step, QUEEN}; moves_list[move_count++] = {from, one_step, ROOK}; moves_list[move_count++] = {from, one_step, BISHOP}; moves_list[move_count++] = {from, one_step, KNIGHT};
            } else if (!captures_only) {
                moves_list[move_count++] = {from, one_step};
                if (rank == (stm == WHITE ? 1 : 6)) {
                    int two_steps = from + (stm == WHITE ? 16 : -16);
                    if (Bitboard::get_bit(~occupied, two_steps)) moves_list[move_count++] = {from, two_steps};
                }
            }
        }
        // Captures
        uint64_t attacks = g_attack_tables.pawn_attacks[stm][from] & (opp_pieces | (pos.ep_square != -1 ? Bitboard::set_bit(pos.ep_square) : 0));
        while(attacks) {
            int to = Bitboard::lsb_index(attacks); attacks &= attacks-1;
            if (rank == (stm == WHITE ? 6 : 1)) {
                moves_list[move_count++] = {from, to, QUEEN}; moves_list[move_count++] = {from, to, ROOK}; moves_list[move_count++] = {from, to, BISHOP}; moves_list[move_count++] = {from, to, KNIGHT};
            } else {
                moves_list[move_count++] = {from, to};
            }
        }
    }

    // Other pieces
    for(Piece p_type : {KNIGHT, BISHOP, ROOK, QUEEN, KING}) {
        uint64_t pieces = pos.piece_bb[p_type] & my_pieces;
        while(pieces) {
            int from = Bitboard::lsb_index(pieces); pieces &= pieces - 1;
            uint64_t attacks = 0;
            if (p_type == KNIGHT) attacks = g_attack_tables.knight_attacks[from];
            else if (p_type == BISHOP) attacks = g_attack_tables.get_bishop_attacks(from, occupied);
            else if (p_type == ROOK) attacks = g_attack_tables.get_rook_attacks(from, occupied);
            else if (p_type == QUEEN) attacks = g_attack_tables.get_queen_attacks(from, occupied);
            else if (p_type == KING) attacks = g_attack_tables.king_attacks[from];
            
            attacks &= (captures_only ? opp_pieces : ~my_pieces);
            while(attacks) {
                int to = Bitboard::lsb_index(attacks); attacks &= attacks-1;
                moves_list[move_count++] = {from, to};
            }
        }
    }

    // Castling
    if (!captures_only) {
        if (stm == WHITE) {
            if ((pos.castling_rights & WK_CASTLE_MASK) && (occupied & WK_CASTLE_PATH) == 0 && !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ+1, BLACK)) moves_list[move_count++] = {E1_SQ, G1_SQ};
            if ((pos.castling_rights & WQ_CASTLE_MASK) && (occupied & WQ_CASTLE_PATH) == 0 && !is_square_attacked(pos, E1_SQ, BLACK) && !is_square_attacked(pos, E1_SQ-1, BLACK)) moves_list[move_count++] = {E1_SQ, C1_SQ};
        } else {
            if ((pos.castling_rights & BK_CASTLE_MASK) && (occupied & BK_CASTLE_PATH) == 0 && !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ+1, WHITE)) moves_list[move_count++] = {E8_SQ, G8_SQ};
            if ((pos.castling_rights & BQ_CASTLE_MASK) && (occupied & BQ_CASTLE_PATH) == 0 && !is_square_attacked(pos, E8_SQ, WHITE) && !is_square_attacked(pos, E8_SQ-1, WHITE)) moves_list[move_count++] = {E8_SQ, C8_SQ};
        }
    }

    return move_count;
}

Position make_move(const Position& pos, const Move& move, bool& legal) {
    // This function's logic is large and remains unchanged.
    legal = false;
    Position next_pos = pos;
    int stm = pos.side_to_move;
    int opp = 1 - stm;
    uint64_t from_bb = Bitboard::set_bit(move.from);
    uint64_t to_bb = Bitboard::set_bit(move.to);
    Piece piece_moved = pos.piece_on_sq(move.from);
    Piece piece_captured = pos.piece_on_sq(move.to);

    if (piece_moved == NO_PIECE || pos.color_on_sq(move.from) != stm) return pos;

    next_pos.zobrist_hash = pos.zobrist_hash;
    next_pos.pawn_zobrist_key = pos.pawn_zobrist_key;
    
    // Update board state for the moving piece
    next_pos.piece_bb[piece_moved] &= ~from_bb;
    next_pos.color_bb[stm] &= ~from_bb;
    next_pos.squares[move.from] = EMPTY_SQUARE;
    next_pos.zobrist_hash ^= Zobrist::pieces[stm][piece_moved][move.from];
    if (piece_moved == PAWN) next_pos.pawn_zobrist_key ^= Zobrist::pieces[stm][PAWN][move.from];

    // Handle captures
    if (piece_captured != NO_PIECE) {
        next_pos.piece_bb[piece_captured] &= ~to_bb;
        next_pos.color_bb[opp] &= ~to_bb;
        next_pos.zobrist_hash ^= Zobrist::pieces[opp][piece_captured][move.to];
        if (piece_captured == PAWN) next_pos.pawn_zobrist_key ^= Zobrist::pieces[opp][PAWN][move.to];
        next_pos.halfmove_clock = 0;
    } else {
        next_pos.halfmove_clock++;
    }
    if (piece_moved == PAWN) next_pos.halfmove_clock = 0;

    // Handle en-passant capture
    if (piece_moved == PAWN && move.to == pos.ep_square && pos.ep_square != -1) {
        int captured_pawn_sq = (stm == WHITE) ? move.to - 8 : move.to + 8;
        next_pos.squares[captured_pawn_sq] = EMPTY_SQUARE;
        next_pos.piece_bb[PAWN] &= ~Bitboard::set_bit(captured_pawn_sq);
        next_pos.color_bb[opp] &= ~Bitboard::set_bit(captured_pawn_sq);
        next_pos.zobrist_hash ^= Zobrist::pieces[opp][PAWN][captured_pawn_sq];
        next_pos.pawn_zobrist_key ^= Zobrist::pieces[opp][PAWN][captured_pawn_sq];
    }
    
    // Update en-passant square
    next_pos.zobrist_hash ^= Zobrist::ep[(pos.ep_square == -1) ? 64 : pos.ep_square];
    if (piece_moved == PAWN && std::abs(move.to - move.from) == 16) next_pos.ep_square = (stm == WHITE) ? move.from + 8 : move.from - 8;
    else next_pos.ep_square = -1;
    next_pos.zobrist_hash ^= Zobrist::ep[(next_pos.ep_square == -1) ? 64 : next_pos.ep_square];
    
    // Place the piece on the new square, handle promotion
    Piece place_piece = move.promotion != NO_PIECE ? move.promotion : piece_moved;
    next_pos.piece_bb[place_piece] |= to_bb;
    next_pos.color_bb[stm] |= to_bb;
    next_pos.squares[move.to] = make_piece(place_piece, (Color)stm);
    next_pos.zobrist_hash ^= Zobrist::pieces[stm][place_piece][move.to];
    if (place_piece == PAWN) next_pos.pawn_zobrist_key ^= Zobrist::pieces[stm][PAWN][move.to];
    
    // Handle castling rook movement
    if (piece_moved == KING && std::abs(move.to - move.from) == 2) {
        int rook_from, rook_to;
        if (move.to == G1_SQ) { rook_from = H1_SQ; rook_to = F1_SQ; }
        else if (move.to == C1_SQ) { rook_from = A1_SQ; rook_to = D1_SQ; }
        else if (move.to == G8_SQ) { rook_from = H8_SQ; rook_to = F8_SQ; }
        else { rook_from = A8_SQ; rook_to = D8_SQ; } // C8_SQ
        next_pos.piece_bb[ROOK] ^= (Bitboard::set_bit(rook_from) | Bitboard::set_bit(rook_to));
        next_pos.color_bb[stm] ^= (Bitboard::set_bit(rook_from) | Bitboard::set_bit(rook_to));
        next_pos.squares[rook_from] = EMPTY_SQUARE;
        next_pos.squares[rook_to] = make_piece(ROOK, (Color)stm);
        next_pos.zobrist_hash ^= Zobrist::pieces[stm][ROOK][rook_from] ^ Zobrist::pieces[stm][ROOK][rook_to];
    }

    // Update castling rights
    uint8_t new_castling_rights = pos.castling_rights;
    if (piece_moved == KING) new_castling_rights &= (stm == WHITE) ? ~(WK_CASTLE_MASK | WQ_CASTLE_MASK) : ~(BK_CASTLE_MASK | BQ_CASTLE_MASK);
    if (move.from == H1_SQ || move.to == H1_SQ) new_castling_rights &= ~WK_CASTLE_MASK;
    if (move.from == A1_SQ || move.to == A1_SQ) new_castling_rights &= ~WQ_CASTLE_MASK;
    if (move.from == H8_SQ || move.to == H8_SQ) new_castling_rights &= ~BK_CASTLE_MASK;
    if (move.from == A8_SQ || move.to == A8_SQ) new_castling_rights &= ~BQ_CASTLE_MASK;
    if (new_castling_rights != pos.castling_rights) {
        next_pos.zobrist_hash ^= Zobrist::castling[pos.castling_rights] ^ Zobrist::castling[new_castling_rights];
        next_pos.castling_rights = new_castling_rights;
    }
    
    // Update side to move and clocks
    next_pos.side_to_move = opp;
    next_pos.zobrist_hash ^= Zobrist::side_to_move;
    if (stm == BLACK) next_pos.fullmove_number++;
    next_pos.ply++;

    // Check legality
    int king_sq = Bitboard::lsb_index(next_pos.piece_bb[KING] & next_pos.color_bb[stm]);
    if (king_sq != -1 && is_square_attacked(next_pos, king_sq, opp)) return pos; // Illegal move
    
    legal = true;
    return next_pos;
}


// ============================================================================
// 11. UCI (Universal Chess Interface)
// ============================================================================

class UCI {
public:
    void loop();

private:
    void handle_position(std::istringstream& ss);
    void handle_go(std::istringstream& ss);
    void handle_setoption(std::istringstream& ss);
    void parse_fen(const std::string& fen_str);
    Move parse_uci_move(const std::string& move_str);

    Position m_root_pos;
    Search m_search;
    std::vector<uint64_t> m_game_history;
};

void UCI::loop() {
    std::string line, token;
    parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    while (std::getline(std::cin, line)) {
        std::istringstream ss(line);
        ss >> token;
        if (token == "uci") {
            std::cout << "id name Amira 1.65 (Refactored)\n";
            std::cout << "id author ChessTubeTree\n";
            std::cout << "option name Hash type spin default " << TT_SIZE_MB_DEFAULT << " min 0 max 16384\n";
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            m_search.set_tt_size(TT_SIZE_MB_DEFAULT); // Ensure TT is ready
            m_search.new_game();
            std::cout << "readyok\n" << std::flush;
        } else if (token == "setoption") {
            handle_setoption(ss);
        } else if (token == "ucinewgame") {
            parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
            m_search.new_game();
        } else if (token == "position") {
            handle_position(ss);
        } else if (token == "go") {
            handle_go(ss);
        } else if (token == "quit" || token == "stop") {
            break;
        }
    }
}

void UCI::handle_setoption(std::istringstream& ss) {
    std::string token, name, value;
    ss >> token; // "name"
    ss >> name;
    ss >> token; // "value"
    ss >> value;
    if (name == "Hash") {
        try {
            m_search.set_tt_size(std::stoi(value));
        } catch (...) {}
    }
}

void UCI::handle_position(std::istringstream& ss) {
    std::string token, fen_part, fen_str;
    ss >> token;
    if (token == "startpos") {
        fen_str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        ss >> token; // Check for "moves"
    } else if (token == "fen") {
        while (ss >> fen_part && fen_part != "moves") fen_str += fen_part + " ";
        token = (fen_part == "moves") ? "moves" : "";
    }
    parse_fen(fen_str);

    if (token == "moves") {
        std::string move_str;
        while (ss >> move_str) {
            Move m = parse_uci_move(move_str);
            if (m.is_null()) continue;
            m_game_history.push_back(m_root_pos.zobrist_hash);
            bool legal;
            m_root_pos = make_move(m_root_pos, m, legal);
        }
    }
}

void UCI::handle_go(std::istringstream& ss) {
    long long wtime = -1, btime = -1, winc = 0, binc = 0;
    int max_depth = MAX_PLY;
    std::string token;
    while(ss >> token) {
        if (token == "wtime") ss >> wtime;
        else if (token == "btime") ss >> btime;
        else if (token == "winc") ss >> winc;
        else if (token == "binc") ss >> binc;
        else if (token == "depth") ss >> max_depth;
    }
    
    long long my_time = (m_root_pos.side_to_move == WHITE) ? wtime : btime;
    long long my_inc = (m_root_pos.side_to_move == WHITE) ? winc : binc;
    long long soft_limit_ms = 0, hard_limit_ms = 0;

    if (my_time != -1) {
        soft_limit_ms = (my_time / 25) + (my_inc * 3 / 4);
        hard_limit_ms = std::min(my_time / 2, my_time - 100);
    }

    Move best_move = m_search.run_iterative_deepening(m_root_pos, soft_limit_ms, hard_limit_ms, max_depth);
    std::cout << "bestmove " << move_to_uci(best_move) << std::endl;
}

void UCI::parse_fen(const std::string& fen_str) {
    m_root_pos = Position();
    m_game_history.clear();
    std::stringstream ss(fen_str);
    std::string part;
    ss >> part;
    int rank = 7, file = 0;
    for (char c : part) {
        if (isdigit(c)) { file += (c - '0'); }
        else if (c == '/') { rank--; file = 0; }
        else {
            Piece p_type = NO_PIECE; Color p_color = islower(c) ? BLACK : WHITE;
            c = tolower(c);
            if (c=='p') p_type=PAWN; else if(c=='n') p_type=KNIGHT; else if(c=='b') p_type=BISHOP;
            else if(c=='r') p_type=ROOK; else if(c=='q') p_type=QUEEN; else if(c=='k') p_type=KING;
            int sq = rank * 8 + file;
            m_root_pos.piece_bb[p_type] |= Bitboard::set_bit(sq);
            m_root_pos.color_bb[p_color] |= Bitboard::set_bit(sq);
            m_root_pos.squares[sq] = make_piece(p_type, p_color);
            file++;
        }
    }
    ss >> part; m_root_pos.side_to_move = (part == "w") ? WHITE : BLACK;
    ss >> part; for(char c : part) {
        if(c=='K') m_root_pos.castling_rights |= WK_CASTLE_MASK; else if(c=='Q') m_root_pos.castling_rights |= WQ_CASTLE_MASK;
        else if(c=='k') m_root_pos.castling_rights |= BK_CASTLE_MASK; else if(c=='q') m_root_pos.castling_rights |= BQ_CASTLE_MASK;
    }
    ss >> part; if(part != "-") m_root_pos.ep_square = (part[0]-'a') + (part[1]-'1') * 8;
    if(ss >> part) m_root_pos.halfmove_clock = std::stoi(part);
    if(ss >> part) m_root_pos.fullmove_number = std::stoi(part);

    m_root_pos.zobrist_hash = calculate_zobrist_hash(m_root_pos);
    m_root_pos.pawn_zobrist_key = calculate_pawn_zobrist_hash(m_root_pos);
}

Move UCI::parse_uci_move(const std::string& move_str) {
    Move m = NULL_MOVE;
    if (move_str.length() < 4) return m;
    m.from = (move_str[0] - 'a') + (move_str[1] - '1') * 8;
    m.to = (move_str[2] - 'a') + (move_str[3] - '1') * 8;
    if (move_str.length() == 5) {
        char promo = move_str[4];
        if (promo == 'q') m.promotion = QUEEN; else if (promo == 'r') m.promotion = ROOK;
        else if (promo == 'b') m.promotion = BISHOP; else if (promo == 'n') m.promotion = KNIGHT;
    }
    return m;
}

// ============================================================================
// 12. MAIN FUNCTION
// ============================================================================

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    // Initialize engine components
    Zobrist::init();
    g_attack_tables.init();
    g_evaluator.init_eval_masks();

    // Start the UCI loop
    UCI uci_handler;
    uci_handler.loop();

    return 0;
}
