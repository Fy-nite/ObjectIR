#include <iostream>
#include "ir_loader.hpp"

using namespace ObjectIR;

int main() {
    try {
        std::cout << "=== Format Detection Test ===" << std::endl;

        // Test FOB format detection
        bool isFob = IRLoader::IsFOBFormat("test.fob");
        std::cout << "test.fob is FOB format: " << (isFob ? "✓ YES" : "✗ NO") << std::endl;

        // Test JSON format detection
        bool isJsonFob = IRLoader::IsFOBFormat("test.json");
        std::cout << "test.json is FOB format: " << (isJsonFob ? "✓ YES" : "✗ NO") << std::endl;

        // Test auto-loading
        std::cout << std::endl << "Testing auto-loading..." << std::endl;

        auto vmFob = IRLoader::LoadFromFile("test.fob");
        if (vmFob) {
            std::cout << "✓ Successfully loaded FOB file (stub implementation)" << std::endl;
        }

        auto vmJson = IRLoader::LoadFromFile("test.json");
        if (vmJson) {
            std::cout << "✓ Successfully loaded JSON file" << std::endl;
        }

        std::cout << "=== Format Detection Test Complete ===" << std::endl;
        return 0;

    } catch (const std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
        return 1;
    }
}