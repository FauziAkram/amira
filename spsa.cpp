#include <vector>
#include <iostream>
#include <string>

#include "spsa.h"

void SPSA::trySetParam(std::string varName, std::string value) {
    
    SPSAValue<int>* intEntry = nullptr;
    SPSAValue<float>* floatEntry = nullptr;
    bool foundInt = false;
    bool foundFloat = false;

    for (SPSAValue<int>& other : instance().intValues) {
        if (other.varName == varName) {
            intEntry = &other;
            foundInt = true;
            break;
        }
    }
    for (SPSAValue<float>& other : instance().floatValues) {
        if (other.varName == varName) {
            floatEntry = &other;
            foundFloat = true;
            break;
        }
    }

    // Check if the value string contains a decimal point to distinguish types
    bool isFloat = value.find(".") != std::string::npos;

    if (isFloat && foundFloat) {
        float* floatVariable = (float*)floatEntry->varPointer;
        try {
            *floatVariable = std::stof(value);
        } catch (...) {
            // Silently ignore malformed values during tuning
        }
    }
    else if (foundInt) {
        int* intVariable = (int*)intEntry->varPointer;
        try {
            *intVariable = std::stoi(value);
        } catch (...) {
            // Silently ignore malformed values during tuning
        }
    }
}
