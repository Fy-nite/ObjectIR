#pragma once

#include "objectir_runtime.hpp"
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>

namespace ObjectIR
{

    // ============================================================================
    // FOB (Finite Open Bytecode) Loader - Deserialize IR modules from FOB format
    // ============================================================================

    /// Loads ObjectIR modules from FOB binary format
    class FOBLoader
    {
    public:
        /// Load a module from a FOB file path
        static std::shared_ptr<VirtualMachine> LoadFromFile(const std::string &filePath);

    /// Load a module from FOB binary data
    static std::shared_ptr<VirtualMachine> LoadFromData(const std::vector<uint8_t> &data);

private:
    FOBLoader() = default;

    // FOB format structures
    struct FOBHeader {
        char magic[3];          // "FOB"
        uint8_t forkNameLength;
        std::string forkName;   // Should be "OBJECTIR,FOB"
        uint32_t fileSize;
        uint32_t entryPoint;
    };

    struct SectionHeader {
        std::string name;
        uint32_t startAddr;
        uint32_t size;
    };

    // Forward declarations for nested structs
    struct FOBFieldDefinition;
    struct FOBMethodDefinition;
    struct FOBParameterDefinition;
    struct FOBLocalDefinition;
    struct FOBInstruction;
    struct FOBConstant;

    // ObjectIR FOB section data
    struct FOBFieldDefinition {
        uint32_t nameIndex;
        uint32_t typeIndex;
        uint8_t access;
        uint8_t flags;
    };

    struct FOBParameterDefinition {
        uint32_t nameIndex;
        uint32_t typeIndex;
    };

    struct FOBLocalDefinition {
        uint32_t nameIndex;
        uint32_t typeIndex;
    };

    struct FOBInstruction {
        uint8_t opcode;
        uint8_t operandCount;
        std::vector<uint8_t> operands;
    };

    struct FOBConstant {
        uint8_t type;
        std::vector<uint8_t> value;
    };

    struct FOBMethodDefinition {
        uint32_t nameIndex;
        uint32_t returnTypeIndex;
        uint8_t access;
        uint8_t flags;
        std::vector<FOBParameterDefinition> parameters;
        std::vector<FOBLocalDefinition> locals;
        std::vector<uint32_t> instructionOffsets;
    };

    struct FOBTypeDefinition {
        uint8_t kind;           // 0x01 = Class, 0x02 = Interface
        uint32_t nameIndex;
        uint32_t namespaceIndex;
        uint8_t access;         // 0x01 = Public, etc.
        uint8_t flags;          // Bit flags
        uint32_t baseTypeIndex; // 0xFFFFFFFF for none
        std::vector<uint32_t> interfaceIndices;
        std::vector<FOBFieldDefinition> fields;
        std::vector<FOBMethodDefinition> methods;
    };        // Parsing methods
        static FOBHeader ParseHeader(std::istream &stream);
        static std::vector<SectionHeader> ParseSectionHeaders(std::istream &stream, uint32_t fileSize);
        static std::vector<std::string> ParseStringsSection(std::istream &stream, const SectionHeader &section);
        static std::vector<FOBTypeDefinition> ParseTypesSection(std::istream &stream, const SectionHeader &section);
        static std::vector<FOBInstruction> ParseCodeSection(std::istream &stream, const SectionHeader &section);
        static std::vector<FOBConstant> ParseConstantsSection(std::istream &stream, const SectionHeader &section);

        // Helper methods
        static uint32_t ReadU32(std::istream &stream);
        static uint16_t ReadU16(std::istream &stream);
        static uint8_t ReadU8(std::istream &stream);
        static std::string ReadString(std::istream &stream, uint32_t length);
        static std::vector<uint8_t> ReadBytes(std::istream &stream, uint32_t count);

        // Construction methods
        static std::shared_ptr<VirtualMachine> BuildVirtualMachine(
            const FOBHeader &header,
            const std::vector<std::string> &strings,
            const std::vector<FOBTypeDefinition> &types,
            const std::vector<FOBInstruction> &instructions,
            const std::vector<FOBConstant> &constants);

        static void BuildType(
            std::shared_ptr<VirtualMachine> vm,
            const FOBTypeDefinition &typeDef,
            const std::vector<std::string> &strings);

        static MethodRef BuildMethod(
            ClassRef classRef,
            const FOBMethodDefinition &methodDef,
            const std::vector<std::string> &strings,
            const std::vector<FOBInstruction> &allInstructions);

        static TypeReference ParseTypeReference(
            std::shared_ptr<VirtualMachine> vm,
            uint32_t typeIndex,
            const std::vector<std::string> &strings);
    };

} // namespace ObjectIR