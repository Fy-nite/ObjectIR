#include "ir_loader.hpp"
#include "instruction_executor.hpp"
#include "stdlib.hpp"
#include <fstream>
#include <algorithm>

namespace ObjectIR {

namespace {

// Helper to compute stack effect of a single instruction
int GetStackDelta(const Instruction& instr) {
    switch (instr.opCode) {
        // Load operations - push one value
        case OpCode::LdArg:
        case OpCode::LdLoc:
        case OpCode::LdFld:
        case OpCode::LdCon:
        case OpCode::LdStr:
        case OpCode::LdI4:
        case OpCode::LdI8:
        case OpCode::LdR4:
        case OpCode::LdR8:
        case OpCode::LdTrue:
        case OpCode::LdFalse:
        case OpCode::LdNull:
            return 1;

        // Store operations - pop one value
        case OpCode::StLoc:
        case OpCode::StFld:
        case OpCode::StArg:
            return -1;

        // Binary arithmetic/comparison - pop two, push one
        case OpCode::Add:
        case OpCode::Sub:
        case OpCode::Mul:
        case OpCode::Div:
        case OpCode::Rem:
        case OpCode::Ceq:
        case OpCode::Cne:
        case OpCode::Clt:
        case OpCode::Cle:
        case OpCode::Cgt:
        case OpCode::Cge:
            return -1;

        // Unary operations - pop one, push one
        case OpCode::Neg:
            return 0;

        case OpCode::Dup:
            return 1;

        case OpCode::Pop:
            return -1;

        case OpCode::Call:
        case OpCode::CallVirt: {
            int delta = 0;
            if (instr.callTarget.has_value()) {
                delta -= static_cast<int>(instr.callTarget->parameterTypes.size());
                bool isVoidReturn = instr.callTarget->returnType.empty() ||
                    instr.callTarget->returnType == "void" ||
                    instr.callTarget->returnType == "System.Void";
                if (!isVoidReturn) {
                    delta += 1;
                }
            }
            if (instr.opCode == OpCode::CallVirt) {
                delta -= 1; // pop instance
            }
            return delta;
        }

        case OpCode::NewObj:
        case OpCode::NewArr:
            return 1;

        case OpCode::Ret:
            return -1;

        default:
            return 0;
    }
}

} // namespace



std::shared_ptr<VirtualMachine> IRLoader::LoadFromFile(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open IR file: " + filePath);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return LoadFromString(buffer.str());
}

std::shared_ptr<VirtualMachine> IRLoader::LoadFromString(const std::string& jsonStr) {
    try {
        json moduleJson = json::parse(jsonStr);
        return ParseModule(moduleJson);
    } catch (const json::exception& e) {
        throw std::runtime_error("Failed to parse JSON: " + std::string(e.what()));
    }
}

std::shared_ptr<VirtualMachine> IRLoader::ParseModule(const json& moduleJson) {
    auto vm = std::make_shared<VirtualMachine>();
    
    // Register standard library types and methods
    RegisterStandardLibrary(vm);
    
    // Extract module metadata
    std::string moduleName = moduleJson.value("Name", "UnnamedModule");
    std::string version = moduleJson.value("Version", "1.0.0");
    
    // Load all type definitions
    if (moduleJson.contains("Types") && moduleJson["Types"].is_array()) {
        LoadTypes(vm, moduleJson["Types"]);
    }
    
    // Load global functions if present
    if (moduleJson.contains("Functions") && moduleJson["Functions"].is_array()) {
        // TODO: Implement global function loading
    }
    
    return vm;
}

void IRLoader::LoadTypes(std::shared_ptr<VirtualMachine> vm, const json& typesArray) {
    for (const auto& typeJson : typesArray) {
        try {
            LoadTypeDefinition(vm, typeJson);
        } catch (const std::exception& e) {
            std::string typeName = typeJson.value("Name", "Unknown");
            throw std::runtime_error("Failed to load type '" + typeName + "': " + e.what());
        }
    }
}

void IRLoader::LoadTypeDefinition(
    std::shared_ptr<VirtualMachine> vm,
    const json& typeJson
) {
    std::string kind = typeJson.value("Kind", "");
    
    if (kind == "Class") {
        LoadClass(vm, typeJson);
    } else if (kind == "Interface") {
        LoadInterface(vm, typeJson);
    } else if (kind == "Struct") {
        LoadStruct(vm, typeJson);
    } else {
        throw std::runtime_error("Unsupported type kind: " + kind);
    }
}

ClassRef IRLoader::LoadClass(
    std::shared_ptr<VirtualMachine> vm,
    const json& classJson
) {
    std::string className = classJson.value("Name", "");
    std::string nameSpace = classJson.value("Namespace", "");
    std::string access = classJson.value("Access", "Public");
    bool isAbstract = classJson.value("IsAbstract", false);
    bool isSealed = classJson.value("IsSealed", false);
    
    std::string fqName = GetFQTypeName(className, nameSpace);
    
    // Create class in virtual machine
    auto classRef = std::make_shared<Class>(fqName);
    classRef->SetNamespace(nameSpace);
    classRef->SetAbstract(isAbstract);
    classRef->SetSealed(isSealed);
    
    // Handle base class if present
    if (classJson.contains("BaseType") && !classJson["BaseType"].is_null()) {
        std::string baseTypeName = classJson["BaseType"].get<std::string>();
        // TODO: Link to base class once loaded
    }
    
    // Load fields
    if (classJson.contains("Fields") && classJson["Fields"].is_array()) {
        LoadFields(classRef, classJson["Fields"]);
    }
    
    // Load methods
    if (classJson.contains("Methods") && classJson["Methods"].is_array()) {
        LoadMethods(classRef, classJson["Methods"]);
    }
    
    // Register class in VM
    vm->RegisterClass(classRef);
    
    return classRef;
}

void IRLoader::LoadInterface(
    std::shared_ptr<VirtualMachine> vm,
    const json& interfaceJson
) {
    std::string ifaceName = interfaceJson.value("Name", "");
    std::string nameSpace = interfaceJson.value("Namespace", "");
    
    std::string fqName = GetFQTypeName(ifaceName, nameSpace);
    
    // Create interface class (interfaces are represented as abstract classes with virtual methods)
    auto ifaceRef = std::make_shared<Class>(fqName);
    ifaceRef->SetNamespace(nameSpace);
    ifaceRef->SetAbstract(true);
    
    // Load interface methods
    if (interfaceJson.contains("Methods") && interfaceJson["Methods"].is_array()) {
        LoadMethods(ifaceRef, interfaceJson["Methods"]);
    }
    
    vm->RegisterClass(ifaceRef);
}

void IRLoader::LoadStruct(
    std::shared_ptr<VirtualMachine> vm,
    const json& structJson
) {
    std::string structName = structJson.value("Name", "");
    std::string nameSpace = structJson.value("Namespace", "");
    
    std::string fqName = GetFQTypeName(structName, nameSpace);
    
    auto structRef = std::make_shared<Class>(fqName);
    structRef->SetNamespace(nameSpace);
    
    // Load fields
    if (structJson.contains("Fields") && structJson["Fields"].is_array()) {
        LoadFields(structRef, structJson["Fields"]);
    }
    
    // Load methods
    if (structJson.contains("Methods") && structJson["Methods"].is_array()) {
        LoadMethods(structRef, structJson["Methods"]);
    }
    
    vm->RegisterClass(structRef);
}

void IRLoader::LoadFields(
    ClassRef classRef,
    const json& fieldsArray
) {
    for (const auto& fieldJson : fieldsArray) {
        std::string fieldName = fieldJson.value("Name", "");
        std::string fieldType = fieldJson.value("Type", "object");
        std::string access = fieldJson.value("Access", "Private");
        bool isReadOnly = fieldJson.value("IsReadOnly", false);
        
        // Parse field type - for now support basic types
        TypeReference typeRef = ParseTypeReference(nullptr, fieldType);
        
        auto field = std::make_shared<Field>(fieldName, typeRef);
        classRef->AddField(field);
    }
}

void IRLoader::LoadMethods(
    ClassRef classRef,
    const json& methodsArray
) {
    for (const auto& methodJson : methodsArray) {
        std::string methodName = methodJson.value("Name", "");
        std::string returnType = methodJson.value("ReturnType", "void");
        bool isStatic = methodJson.value("IsStatic", false);
        bool isVirtual = methodJson.value("IsVirtual", false);
        bool isAbstract = methodJson.value("IsAbstract", false);
        bool isConstructor = methodJson.value("IsConstructor", false);
        
        TypeReference returnTypeRef = ParseTypeReference(nullptr, returnType);
        
        // Create method object
        auto method = std::make_shared<Method>(
            methodName,
            returnTypeRef,
            isStatic,
            isVirtual
        );
        
        // Load parameters
        if (methodJson.contains("Parameters") && methodJson["Parameters"].is_array()) {
            for (const auto& paramJson : methodJson["Parameters"]) {
                std::string paramName = paramJson.value("Name", "");
                std::string paramType = paramJson.value("Type", "object");
                
                TypeReference paramTypeRef = ParseTypeReference(nullptr, paramType);
                method->AddParameter(paramName, paramTypeRef);
            }
        }

        // Load locals
        if (methodJson.contains("LocalVariables") && methodJson["LocalVariables"].is_array()) {
            for (const auto& localJson : methodJson["LocalVariables"]) {
                std::string localName = localJson.value("Name", "");
                std::string localType = localJson.value("Type", "object");
                TypeReference localTypeRef = ParseTypeReference(nullptr, localType);
                method->AddLocal(localName, localTypeRef);
            }
        }

        // Load instructions if present
        if (methodJson.contains("Instructions") && methodJson["Instructions"].is_array()) {
            std::vector<Instruction> instructions;
            instructions.reserve(methodJson["Instructions"].size());
            for (const auto& instructionJson : methodJson["Instructions"]) {
                Instruction instr = InstructionExecutor::ParseJsonInstruction(instructionJson);
                // While instructions are parsed with their metadata already intact
                instructions.push_back(instr);
            }
            method->SetInstructions(std::move(instructions));
        }

        classRef->AddMethod(method);
    }
}

TypeReference IRLoader::ParseTypeReference(
    std::shared_ptr<VirtualMachine> vm,
    const std::string& typeStr
) {
    // Handle primitive types
    if (typeStr == "int32" || typeStr == "int") {
        return TypeReference::Int32();
    } else if (typeStr == "int64" || typeStr == "long") {
        return TypeReference::Int64();
    } else if (typeStr == "float" || typeStr == "float32") {
        return TypeReference::Float32();
    } else if (typeStr == "double" || typeStr == "float64") {
        return TypeReference::Float64();
    } else if (typeStr == "bool" || typeStr == "boolean") {
        return TypeReference::Bool();
    } else if (typeStr == "string") {
        return TypeReference::String();
    } else if (typeStr == "void") {
        return TypeReference::Void();
    } else {
        // For custom types, we'd need to look them up in the VM
        // For now, return a generic object type
        if (vm) {
            auto classRef = vm->GetClass(typeStr);
            if (classRef) {
                return TypeReference::Object(classRef);
            }
        }
        // Return generic object reference
        return TypeReference::Object(nullptr);
    }
}

std::string IRLoader::GetFQTypeName(const std::string& name, const std::string& ns) {
    if (ns.empty()) {
        return name;
    }
    return ns + "." + name;
}

} // namespace ObjectIR
