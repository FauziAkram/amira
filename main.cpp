#include "types.h"
#include "zobrist.h"
#include "position.h"
#include "evaluation.h"
#include "search.h"
#include "uci.h"

#include <iostream>

int main(int argc, char* argv[]) {
    // Ensure fast I/O
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    // Initialize all the engine's subsystems
    init_zobrist();
    init_attack_tables();
    init_eval_masks();
    init_pawn_cache();
    reset_killers_and_history();

    // Start the UCI communication loop
    uci_loop();

    return 0;
}
