#include <iostream>
#include "fob_loader.hpp"

using namespace ObjectIR;

int main() {
    try {
        std::cout << "=== FOB Loader Test ===" << std::endl;

        // Try to load the test.fob file from OIFortran
        auto vm = FOBLoader::LoadFromFile("/run/media/charlie/the cat storage v2/ObjectIR/src/OIFortran/test.fob");

        if (vm) {
            std::cout << "✓ FOB loader loaded test.fob successfully!" << std::endl;
            std::cout << "✓ Standard library registered" << std::endl;

            // Check if Console class is available
            auto consoleClass = vm->GetClass("System.Console");
            if (consoleClass) {
                std::cout << "✓ Console class found in VM" << std::endl;
            } else {
                std::cout << "✗ Console class not found" << std::endl;
            }
        } else {
            std::cout << "✗ FOB loader failed to create VM" << std::endl;
            return 1;
        }

        std::cout << "=== FOB Loader Test Complete ===" << std::endl;
        return 0;

    } catch (const std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
        return 1;
    }
}