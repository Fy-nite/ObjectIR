#include "fob_loader.hpp"
#include <iostream>

int main() {
    try {
        auto vm = ObjectIR::FOBLoader::LoadFromFile("/run/media/charlie/the cat storage v2/ObjectIR/src/ObjectIR.CppRuntime/build/test.fob");
        std::cout << "Successfully loaded FOB file!" << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Error loading FOB file: " << e.what() << std::endl;
        return 1;
    }
}