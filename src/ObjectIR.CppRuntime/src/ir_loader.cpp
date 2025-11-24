#include "ir_loader.hpp"
#include "fob_loader.hpp"
#include "instruction_executor.hpp"
#include "stdlib.hpp"
#include <fstream>
#include <iostream>
#include <algorithm>

namespace ObjectIR {

std::shared_ptr<VirtualMachine> IRLoader::LoadFromFile(const std::string& filePath) {
    // Auto-detect format
    if (IsFOBFormat(filePath)) {
        auto result = FOBLoader::LoadFromFile(filePath);
        return result.vm;
    } else {
        // Assume JSON format
        std::ifstream file(filePath);
        if (!file.is_open()) {
            throw std::runtime_error("Cannot open IR file: " + filePath);
        }

        std::stringstream buffer;
        buffer << file.rdbuf();
        return LoadFromString(buffer.str());
    }
}

std::shared_ptr<VirtualMachine> IRLoader::LoadFromString(const std::string& jsonStr) {
    try {
        json j = json::parse(jsonStr);
        return ParseModule(j);
    } catch (const json::parse_error& e) {
        throw std::runtime_error("JSON parse error: " + std::string(e.what()));
    }
}

std::shared_ptr<VirtualMachine> IRLoader::LoadFromFOBData(const std::vector<uint8_t>& data) {
    auto result = FOBLoader::LoadFromData(data);
    return result.vm;
}

bool IRLoader::IsFOBFormat(const std::string& filePath) {
    std::ifstream file(filePath, std::ios::binary);
    if (!file.is_open()) {
        return false;
    }

    char magic[3];
    file.read(magic, 3);
    file.close();

    return (magic[0] == 'F' && magic[1] == 'O' && magic[2] == 'B');
}

std::shared_ptr<VirtualMachine> IRLoader::ParseModule(const json& moduleJson) {
    auto vm = std::make_shared<VirtualMachine>();

    // Register standard library types and methods
    RegisterStandardLibrary(vm);

    // Load types
    if (moduleJson.contains("types")) {
        LoadTypes(vm, moduleJson["types"]);
    }

    return vm;
}

void IRLoader::LoadTypes(std::shared_ptr<VirtualMachine> vm, const json& typesArray) {
    for (const auto& typeJson : typesArray) {
        LoadTypeDefinition(vm, typeJson);
    }
}

void IRLoader::LoadTypeDefinition(std::shared_ptr<VirtualMachine> vm, const json& typeJson) {
    std::string kind = typeJson["kind"];
    if (kind == "class") {
        LoadClass(vm, typeJson);
    } else if (kind == "interface") {
        LoadInterface(vm, typeJson);
    } else if (kind == "struct") {
        LoadStruct(vm, typeJson);
    }
}

ClassRef IRLoader::LoadClass(std::shared_ptr<VirtualMachine> vm, const json& classJson) {
    std::string name = classJson["name"];
    std::string ns = classJson.value("namespace", "");

    std::string fullName = GetFQTypeName(name, ns);
    auto classRef = std::make_shared<Class>(fullName);
    classRef->SetNamespace(ns);

    // Load base class if present
    if (classJson.contains("base")) {
        std::string baseName = classJson["base"];
        // TODO: Resolve base class
    }

    // Load interfaces if present
    if (classJson.contains("interfaces")) {
        // TODO: Load interfaces
    }

    // Load fields
    if (classJson.contains("fields")) {
        LoadFields(classRef, classJson["fields"], vm);
    }

    // Load methods
    if (classJson.contains("methods")) {
        LoadMethods(classRef, classJson["methods"], vm);
    }

    vm->RegisterClass(classRef);
    return classRef;
}

void IRLoader::LoadInterface(std::shared_ptr<VirtualMachine> vm, const json& interfaceJson) {
    // TODO: Implement interface loading
}

void IRLoader::LoadStruct(std::shared_ptr<VirtualMachine> vm, const json& structJson) {
    // TODO: Implement struct loading
}

void IRLoader::LoadFields(ClassRef classRef, const json& fieldsArray, std::shared_ptr<VirtualMachine> vm) {
    for (const auto& fieldJson : fieldsArray) {
        std::string name = fieldJson["name"];
        std::string typeStr = fieldJson["type"];
        TypeReference typeRef = ParseTypeReference(vm, typeStr);
        
        auto field = std::make_shared<Field>(name, typeRef);
        classRef->AddField(field);
    }
}

void IRLoader::LoadMethods(ClassRef classRef, const json& methodsArray, std::shared_ptr<VirtualMachine> vm) {
    for (const auto& methodJson : methodsArray) {
        std::string name = methodJson["name"];
        std::string returnTypeStr = methodJson["returnType"];
        TypeReference returnType = ParseTypeReference(vm, returnTypeStr);

        bool isStatic = methodJson.value("isStatic", false);
        auto method = std::make_shared<Method>(name, returnType, isStatic);

        if (methodJson.contains("parameters")) {
            for (const auto& paramJson : methodJson["parameters"]) {
                std::string paramName = paramJson["name"];
                std::string paramTypeStr = paramJson["type"];
                TypeReference paramType = ParseTypeReference(vm, paramTypeStr);
                method->AddParameter(paramName, paramType);
            }
        }

        // Load method body/instructions
        if (methodJson.contains("body") && methodJson["body"].is_array()) {
            std::vector<Instruction> instructions;
            int instrCount = 0;
            for (const auto& instrJson : methodJson["body"]) {
                try {
                    std::cerr << "  [" << name << "] Parsing instruction " << instrCount << ": " 
                              << instrJson.dump() << std::endl;
                    Instruction instr = InstructionExecutor::ParseJsonInstruction(instrJson);
                    instructions.push_back(instr);
                    std::cerr << "  [" << name << "] ✓ Instruction " << instrCount << " parsed successfully" << std::endl;
                } catch (const std::exception& e) {
                    // If instruction parsing fails, log but continue
                    // The method will have partial instructions rather than failing completely
                    std::cerr << "ERROR [" << name << "] Failed to parse instruction " << instrCount 
                              << ": " << e.what() << std::endl;
                    std::cerr << "  Instruction JSON: " << instrJson.dump() << std::endl;
                    std::cerr << "  Instruction type: " << instrJson.type_name() << std::endl;
                }
                instrCount++;
            }
            if (!instructions.empty()) {
                std::cerr << "[" << name << "] Setting " << instructions.size() << " instructions on method" << std::endl;
                method->SetInstructions(std::move(instructions));
            }
        }

        classRef->AddMethod(method);
    }
}

TypeReference IRLoader::ParseTypeReference(std::shared_ptr<VirtualMachine> vm, const std::string& typeStr) {
    // Handle primitive types
    if (typeStr == "int32") return TypeReference::Int32();
    if (typeStr == "int64") return TypeReference::Int64();
    if (typeStr == "float") return TypeReference::Float32();
    if (typeStr == "double") return TypeReference::Float64();
    if (typeStr == "bool") return TypeReference::Bool();
    if (typeStr == "string") return TypeReference::String();
    if (typeStr == "void") return TypeReference::Void();

    // Handle user-defined types - for now, return Object type as fallback
    // TODO: Look up user-defined types from the VM
    return TypeReference::Object();
}

std::string IRLoader::GetFQTypeName(const std::string& name, const std::string& ns) {
    return ns.empty() ? name : ns + "." + name;
}

} // namespace ObjectIR