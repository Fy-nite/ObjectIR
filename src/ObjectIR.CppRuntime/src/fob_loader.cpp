#include "fob_loader.hpp"
#include "instruction_executor.hpp"
#include "stdlib.hpp"
#include <algorithm>
#include <cstring>
#include <iostream>
#include <sstream>

namespace ObjectIR {

struct BuildTypeResult {
    std::string className;
    std::vector<std::string> methodNames;
};

FOBLoader::FOBLoadResult FOBLoader::LoadFromFile(const std::string& filePath) {
    std::ifstream file(filePath, std::ios::binary);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open FOB file: " + filePath);
    }

    // Read entire file into memory
    file.seekg(0, std::ios::end);
    size_t fileSize = file.tellg();
    file.seekg(0, std::ios::beg);

    std::vector<uint8_t> data(fileSize);
    file.read(reinterpret_cast<char*>(data.data()), fileSize);
    file.close();

    return LoadFromData(data);
}

FOBLoader::FOBLoadResult FOBLoader::LoadFromData(const std::vector<uint8_t>& data) {
    std::istringstream stream(std::string(data.begin(), data.end()), std::ios::binary);

    // Parse FOB header
    auto header = ParseHeader(stream);

    // Parse section headers
    auto sections = ParseSectionHeaders(stream, header.fileSize);

    // Parse each section
    std::vector<std::string> strings;
    std::vector<FOBTypeDefinition> types;
    std::vector<FOBInstruction> instructions;
    std::vector<FOBConstant> constants;

    for (const auto& section : sections) {
        // Clear any error bits before seeking
        stream.clear();
        stream.seekg(section.startAddr, std::ios::beg);

        if (section.name == ".strings") {
            strings = ParseStringsSection(stream, section);
        } else if (section.name == ".types") {
            types = ParseTypesSection(stream, section);
        } else if (section.name == ".code") {
            instructions = ParseCodeSection(stream, section);
        } else if (section.name == ".constants") {
            constants = ParseConstantsSection(stream, section);
        }
        // Skip unknown sections
    }

    // Build virtual machine from parsed data
    return BuildVirtualMachine(header, strings, types, instructions, constants);
}

// ============================================================================
// Parsing Methods
// ============================================================================

FOBLoader::FOBHeader FOBLoader::ParseHeader(std::istream &stream) {
    FOBHeader header;

    // Read magic bytes "FOB"
    char magic[3];
    stream.read(magic, 3);
    if (magic[0] != 'F' || magic[1] != 'O' || magic[2] != 'B') {
        throw std::runtime_error("Invalid FOB file: wrong magic bytes");
    }
    header.magic[0] = magic[0];
    header.magic[1] = magic[1];
    header.magic[2] = magic[2];

    // Read fork name length and name
    uint8_t forkNameLength = ReadU8(stream);
    header.forkName.resize(forkNameLength);
    stream.read(&header.forkName[0], forkNameLength);

    // Verify fork name
    if (header.forkName != "OBJECTIR,FOB") {
        throw std::runtime_error("Unsupported FOB fork: " + header.forkName);
    }

    // Read file size and entry point
    header.fileSize = ReadU32(stream);
    header.entryPoint = ReadU32(stream);

    return header;
}

std::vector<FOBLoader::SectionHeader> FOBLoader::ParseSectionHeaders(std::istream &stream, uint32_t fileSize) {
    std::vector<SectionHeader> sections;

    // Read sections until we reach the end or find a section with size 0
    while (stream.tellg() < fileSize) {
        SectionHeader section;

        // Read section name (null-terminated)
        std::getline(stream, section.name, '\0');
        if (section.name.empty()) break; // End of sections

        // Read section size
        section.size = ReadU32(stream);
        
        // Record the start address BEFORE skipping
        section.startAddr = static_cast<uint32_t>(stream.tellg());

        sections.push_back(section);

        // Skip to next section header
        stream.seekg(section.size, std::ios::cur);
    }

    return sections;
}

std::vector<std::string> FOBLoader::ParseStringsSection(std::istream &stream, const SectionHeader &section) {
    std::vector<std::string> strings;

    // Read string count
    uint32_t stringCount = ReadU32(stream);
    strings.reserve(stringCount);

    for (uint32_t i = 0; i < stringCount; ++i) {
        // Read string length
        uint32_t length = ReadU32(stream);

        // Read string data
        std::string str(length, '\0');
        stream.read(&str[0], length);
        strings.push_back(str);
    }

    return strings;
}

std::vector<FOBLoader::FOBTypeDefinition> FOBLoader::ParseTypesSection(std::istream &stream, const SectionHeader &section) {
    std::vector<FOBTypeDefinition> types;

    // Read type count
    uint32_t typeCount = ReadU32(stream);
    types.reserve(typeCount);

    for (uint32_t i = 0; i < typeCount; ++i) {
        FOBTypeDefinition typeDef;

        // Read type kind (0x01 = Class, 0x02 = Interface)
        typeDef.kind = ReadU8(stream);

        // Read name and namespace indices
        typeDef.nameIndex = ReadU32(stream);
        typeDef.namespaceIndex = ReadU32(stream);

        // Read access and flags
        typeDef.access = ReadU8(stream);
        typeDef.flags = ReadU8(stream);

        // Read base type index
        typeDef.baseTypeIndex = ReadU32(stream);

        // Read interface count and indices
        uint32_t interfaceCount = ReadU32(stream);
        typeDef.interfaceIndices.resize(interfaceCount);
        for (uint32_t j = 0; j < interfaceCount; ++j) {
            typeDef.interfaceIndices[j] = ReadU32(stream);
        }

        // Read field count and fields
        uint32_t fieldCount = ReadU32(stream);
        typeDef.fields.resize(fieldCount);
        for (uint32_t j = 0; j < fieldCount; ++j) {
            auto& field = typeDef.fields[j];
            field.nameIndex = ReadU32(stream);
            field.typeIndex = ReadU32(stream);
            field.access = ReadU8(stream);
            field.flags = ReadU8(stream);
        }

        // Read method count and methods
        uint32_t methodCount = ReadU32(stream);
        typeDef.methods.resize(methodCount);
        for (uint32_t j = 0; j < methodCount; ++j) {
            auto& method = typeDef.methods[j];
            method.nameIndex = ReadU32(stream);
            method.returnTypeIndex = ReadU32(stream);
            method.access = ReadU8(stream);
            method.flags = ReadU8(stream);

            // Parameters
            uint32_t paramCount = ReadU32(stream);
            method.parameters.resize(paramCount);
            for (uint32_t k = 0; k < paramCount; ++k) {
                auto& param = method.parameters[k];
                param.nameIndex = ReadU32(stream);
                param.typeIndex = ReadU32(stream);
            }

            // Locals
            uint32_t localCount = ReadU32(stream);
            method.locals.resize(localCount);
            for (uint32_t k = 0; k < localCount; ++k) {
                auto& local = method.locals[k];
                local.nameIndex = ReadU32(stream);
                local.typeIndex = ReadU32(stream);
            }

            // Instructions
            uint32_t instructionCount = ReadU32(stream);
            method.instructionOffsets.resize(instructionCount);
            for (uint32_t k = 0; k < instructionCount; ++k) {
                method.instructionOffsets[k] = ReadU32(stream);
            }
        }

        types.push_back(typeDef);
    }

    return types;
}

std::vector<FOBLoader::FOBInstruction> FOBLoader::ParseCodeSection(std::istream &stream, const SectionHeader &section) {
    std::vector<FOBInstruction> instructions;

    // Read instruction count
    uint32_t instructionCount = ReadU32(stream);
    instructions.reserve(instructionCount);

    for (uint32_t i = 0; i < instructionCount; ++i) {
        FOBInstruction instruction;

        // Read opcode
        instruction.opcode = ReadU8(stream);

        // Read operand count
        instruction.operandCount = ReadU8(stream);

        // Read operands
        instruction.operands.resize(instruction.operandCount);
        for (uint8_t j = 0; j < instruction.operandCount; ++j) {
            instruction.operands[j] = ReadU8(stream);
        }

        instructions.push_back(instruction);
    }

    return instructions;
}

std::vector<FOBLoader::FOBConstant> FOBLoader::ParseConstantsSection(std::istream &stream, const SectionHeader &section) {
    std::vector<FOBConstant> constants;

    // Read constant count
    uint32_t constantCount = ReadU32(stream);
    constants.reserve(constantCount);

    for (uint32_t i = 0; i < constantCount; ++i) {
        FOBConstant constant;

        // Read type
        constant.type = ReadU8(stream);

        // Read value based on type
        switch (constant.type) {
            case 0x01: // Int32
                constant.value.resize(4);
                stream.read(reinterpret_cast<char*>(constant.value.data()), 4);
                break;
            case 0x02: // Int64
                constant.value.resize(8);
                stream.read(reinterpret_cast<char*>(constant.value.data()), 8);
                break;
            case 0x03: // Float
                constant.value.resize(4);
                stream.read(reinterpret_cast<char*>(constant.value.data()), 4);
                break;
            case 0x04: // Double
                constant.value.resize(8);
                stream.read(reinterpret_cast<char*>(constant.value.data()), 8);
                break;
            case 0x05: // String
                {
                    uint32_t length = ReadU32(stream);
                    constant.value.resize(length);
                    stream.read(reinterpret_cast<char*>(constant.value.data()), length);
                }
                break;
            case 0x06: // Bool
                constant.value.resize(1);
                stream.read(reinterpret_cast<char*>(constant.value.data()), 1);
                break;
            case 0x07: // Null
                // No value
                break;
            default:
                throw std::runtime_error("Unknown constant type: " + std::to_string(constant.type));
        }

        constants.push_back(constant);
    }

    return constants;
}

// ============================================================================
// Helper Methods
// ============================================================================

uint32_t FOBLoader::ReadU32(std::istream &stream) {
    uint32_t value;
    stream.read(reinterpret_cast<char*>(&value), sizeof(value));
    return value;
}

uint16_t FOBLoader::ReadU16(std::istream &stream) {
    uint16_t value;
    stream.read(reinterpret_cast<char*>(&value), sizeof(value));
    return value;
}

uint8_t FOBLoader::ReadU8(std::istream &stream) {
    uint8_t value;
    stream.read(reinterpret_cast<char*>(&value), sizeof(value));
    return value;
}

std::string FOBLoader::ReadString(std::istream &stream, uint32_t length) {
    std::string str(length, '\0');
    stream.read(&str[0], length);
    return str;
}

std::vector<uint8_t> FOBLoader::ReadBytes(std::istream &stream, uint32_t count) {
    std::vector<uint8_t> bytes(count);
    stream.read(reinterpret_cast<char*>(bytes.data()), count);
    return bytes;
}

// ============================================================================
// Construction Methods
// ============================================================================

FOBLoader::FOBLoadResult FOBLoader::BuildVirtualMachine(
    const FOBHeader &header,
    const std::vector<std::string> &strings,
    const std::vector<FOBTypeDefinition> &types,
    const std::vector<FOBInstruction> &instructions,
    const std::vector<FOBConstant> &constants) {

    auto vm = std::make_shared<VirtualMachine>();

    // Register standard library types and methods
    RegisterStandardLibrary(vm);

    // Build types and track names
    std::vector<std::string> classNames;
    std::vector<std::vector<std::string>> methodNames;

    for (const auto& typeDef : types) {
        auto result = BuildType(vm, typeDef, strings);
        classNames.push_back(result.className);
        methodNames.push_back(result.methodNames);
    }

    // Extract entry point indices from header
    // Entry point is encoded as (type_index << 16) | method_index
    uint32_t entryPoint = header.entryPoint;
    uint16_t entryTypeIndex = (entryPoint >> 16) & 0xFFFF;
    uint16_t entryMethodIndex = entryPoint & 0xFFFF;

    // Validate header-provided entry point. If invalid, attempt automatic Fortran-style fallback.
    // Fortran sources produce exactly one program; pick the first type containing a method named "Main".
    bool headerEntryValid = entryPoint != 0xFFFFFFFFu &&
        entryTypeIndex < classNames.size() &&
        entryTypeIndex < methodNames.size() &&
        entryMethodIndex < methodNames[entryTypeIndex].size();

    if (!headerEntryValid) {
        // Fallback scan: locate a method named "Main" (case-sensitive) on any loaded class.
        for (size_t t = 0; t < methodNames.size(); ++t) {
            const auto &mNames = methodNames[t];
            for (size_t m = 0; m < mNames.size(); ++m) {
                if (mNames[m] == "Main") {
                    entryTypeIndex = static_cast<uint16_t>(t);
                    entryMethodIndex = static_cast<uint16_t>(m);
                    headerEntryValid = true;
                    // Encode synthesized entryPoint for downstream consumers (optional).
                    entryPoint = (static_cast<uint32_t>(entryTypeIndex) << 16) | static_cast<uint32_t>(entryMethodIndex);
                    break;
                }
            }
            if (headerEntryValid) break;
        }
    }

    // NOTE: We intentionally return even if still invalid; runtime layer can report missing entry point.

    return {vm, entryTypeIndex, entryMethodIndex, classNames, methodNames};
}

FOBLoader::BuildTypeResult FOBLoader::BuildType(
    std::shared_ptr<VirtualMachine> vm,
    const FOBTypeDefinition &typeDef,
    const std::vector<std::string> &strings) {
    
    std::string typeName = strings[typeDef.nameIndex];
    std::string namespaceName = strings[typeDef.namespaceIndex];
    
    // Create class
    auto classRef = std::make_shared<Class>(typeName);
    classRef->SetNamespace(namespaceName);
    
    // Register with VM
    vm->RegisterClass(classRef);
    
    // Add fields
    for (const auto& field : typeDef.fields) {
        std::string fieldName = strings[field.nameIndex];
        auto fieldType = ParseTypeReference(vm, field.typeIndex, strings);
        auto fieldRef = std::make_shared<Field>(fieldName, fieldType);
        classRef->AddField(fieldRef);
    }
    
    // Add methods and collect method names
    std::vector<std::string> methodNames;
    for (const auto& method : typeDef.methods) {
        auto methodRef = BuildMethod(classRef, method, strings, std::vector<FOBInstruction>()); // TODO: Pass instructions
        methodNames.push_back(methodRef->GetName());
    }

    // Return qualified class name (namespace.typename)
    std::string qualifiedName = namespaceName.empty() ? typeName : namespaceName + "." + typeName;
    return {qualifiedName, methodNames};
}

MethodRef FOBLoader::BuildMethod(
    ClassRef classRef,
    const FOBMethodDefinition &methodDef,
    const std::vector<std::string> &strings,
    const std::vector<FOBInstruction> &allInstructions) {
    
    std::string methodName = strings[methodDef.nameIndex];
    auto returnType = ParseTypeReference(nullptr, methodDef.returnTypeIndex, strings); // TODO: Pass VM
    
    // Create method
    MethodRef methodRef = std::make_shared<Method>(methodName, returnType);
    
    // Add to class
    classRef->AddMethod(methodRef);
    
    // Add parameters
    for (const auto& param : methodDef.parameters) {
        std::string paramName = strings[param.nameIndex];
        auto paramType = ParseTypeReference(nullptr, param.typeIndex, strings); // TODO: Pass VM
        methodRef->AddParameter(paramName, paramType);
    }
    
    // Add locals
    for (const auto& local : methodDef.locals) {
        std::string localName = strings[local.nameIndex];
        auto localType = ParseTypeReference(nullptr, local.typeIndex, strings); // TODO: Pass VM
        methodRef->AddLocal(localName, localType);
    }
    
    // TODO: Add instructions
    
    return methodRef;
}

TypeReference FOBLoader::ParseTypeReference(
    std::shared_ptr<VirtualMachine> vm,
    uint32_t typeIndex,
    const std::vector<std::string> &strings) {
    
    // For now, return a placeholder type reference
    // TODO: Implement proper type reference parsing
    return TypeReference::Void();
}

} // namespace ObjectIR
