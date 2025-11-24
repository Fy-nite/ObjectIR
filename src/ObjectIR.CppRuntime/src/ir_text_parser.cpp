#include "ir_text_parser.hpp"
#include "ir_loader.hpp"
#include <algorithm>
#include <iostream>
#include <cctype>

namespace ObjectIR
{

// ============================================================================
// Lexer Implementation
// ============================================================================

IRTextParser::Lexer::Lexer(const std::string& input)
    : input(input)
{
}

char IRTextParser::Lexer::Current() const
{
    return position < input.length() ? input[position] : '\0';
}

char IRTextParser::Lexer::Peek(size_t offset) const
{
    size_t pos = position + offset;
    return pos < input.length() ? input[pos] : '\0';
}

void IRTextParser::Lexer::Advance()
{
    if (position < input.length())
    {
        if (input[position] == '\n')
        {
            line++;
            column = 1;
        }
        else
        {
            column++;
        }
        position++;
    }
}

void IRTextParser::Lexer::SkipWhitespace()
{
    while (!IsAtEnd() && std::isspace(Current()))
    {
        Advance();
    }
}

void IRTextParser::Lexer::SkipComment()
{
    if (Current() == '/' && Peek() == '/')
    {
        while (!IsAtEnd() && Current() != '\n')
        {
            Advance();
        }
    }
}

bool IRTextParser::Lexer::IsAtEnd() const
{
    return position >= input.length();
}

IRTextParser::Token IRTextParser::Lexer::MakeToken(Token::Type type, const std::string& value)
{
    return Token(type, value, line, column);
}

std::string IRTextParser::Lexer::ReadIdentifier()
{
    std::string result;
    while (!IsAtEnd() && (std::isalnum(Current()) || Current() == '_' || Current() == '.'))
    {
        result += Current();
        Advance();
    }
    return result;
}

std::string IRTextParser::Lexer::ReadString()
{
    std::string result;
    Advance(); // Skip opening quote
    while (!IsAtEnd() && Current() != '"')
    {
        if (Current() == '\\' && Peek() == '"')
        {
            result += '"';
            Advance();
            Advance();
        }
        else
        {
            result += Current();
            Advance();
        }
    }
    if (!IsAtEnd())
    {
        Advance(); // Skip closing quote
    }
    return result;
}

std::string IRTextParser::Lexer::ReadNumber()
{
    std::string result;
    while (!IsAtEnd() && (std::isdigit(Current()) || Current() == '.'))
    {
        result += Current();
        Advance();
    }
    return result;
}

IRTextParser::Token IRTextParser::Lexer::NextToken()
{
    SkipWhitespace();

    if (IsAtEnd())
    {
        return MakeToken(Token::Type::EOF_TOKEN, "");
    }

    // Skip comments
    if (Current() == '/' && Peek() == '/')
    {
        SkipComment();
        return NextToken();
    }

    char ch = Current();

    // Strings
    if (ch == '"')
    {
        std::string value = ReadString();
        return MakeToken(Token::Type::String, value);
    }

    // Numbers
    if (std::isdigit(ch))
    {
        std::string value = ReadNumber();
        return MakeToken(Token::Type::Number, value);
    }

    // Identifiers and keywords
    if (std::isalpha(ch) || ch == '_')
    {
        std::string value = ReadIdentifier();

        if (IsKeyword(value))
        {
            return MakeToken(Token::Type::Keyword, value);
        }

        if (IsInstruction(value))
        {
            return MakeToken(Token::Type::Instruction, value);
        }

        if (IsPrimitiveType(value))
        {
            return MakeToken(Token::Type::Type, value);
        }

        return MakeToken(Token::Type::Identifier, value);
    }

    // Operators and delimiters
    Advance();

    switch (ch)
    {
    case '-':
        if (Current() == '>')
        {
            Advance();
            return MakeToken(Token::Type::Arrow, "->");
        }
        return MakeToken(Token::Type::Identifier, "-");
    case ':':
        return MakeToken(Token::Type::Colon, ":");
    case '{':
        return MakeToken(Token::Type::LBrace, "{");
    case '}':
        return MakeToken(Token::Type::RBrace, "}");
    case '(':
        return MakeToken(Token::Type::LParen, "(");
    case ')':
        return MakeToken(Token::Type::RParen, ")");
    case ',':
        return MakeToken(Token::Type::Comma, ",");
    case '.':
        return MakeToken(Token::Type::Dot, ".");
    case '\n':
        return MakeToken(Token::Type::Newline, "\n");
    default:
        return MakeToken(Token::Type::Identifier, std::string(1, ch));
    }
}

// ============================================================================
// Parser Implementation
// ============================================================================

IRTextParser::Parser::Parser(const std::vector<Token>& tokens)
    : tokens(tokens)
{
}

bool IRTextParser::Parser::Match(Token::Type type)
{
    if (Check(type))
    {
        Advance();
        return true;
    }
    return false;
}

bool IRTextParser::Parser::Check(Token::Type type) const
{
    if (current >= tokens.size())
        return false;
    return tokens[current].type == type;
}

IRTextParser::Token IRTextParser::Parser::Advance()
{
    if (current < tokens.size())
    {
        return tokens[current++];
    }
    return tokens.back();
}

IRTextParser::Token IRTextParser::Parser::Peek() const
{
    if (current < tokens.size())
    {
        return tokens[current];
    }
    return tokens.back();
}

IRTextParser::Token IRTextParser::Parser::Previous() const
{
    if (current > 0)
    {
        return tokens[current - 1];
    }
    return tokens[0];
}

void IRTextParser::Parser::Consume(Token::Type type, const std::string& message)
{
    if (!Check(type))
    {
        throw std::runtime_error(message);
    }
    Advance();
}

json IRTextParser::Parser::ParseModule()
{
    json module;
    module["name"] = "ObjectIRModule";
    module["types"] = json::array();

    // Skip to module keyword if present
    while (Peek().value != "module" && current < tokens.size())
    {
        Advance();
    }

    if (Match(Token::Type::Keyword)) // module
    {
        if (Check(Token::Type::Identifier))
        {
            module["name"] = Advance().value;
        }
    }

    // Parse types
    while (current < tokens.size() && Peek().type != Token::Type::EOF_TOKEN)
    {
        if (Peek().value == "class")
        {
            module["types"].push_back(ParseClass());
        }
        else if (Peek().value == "interface")
        {
            module["types"].push_back(ParseInterface());
        }
        else if (Peek().value == "struct")
        {
            module["types"].push_back(ParseStruct());
        }
        else
        {
            Advance();
        }
    }

    return module;
}

json IRTextParser::Parser::ParseClass()
{
    Advance(); // 'class'
    std::string className = Advance().value;

    json classJson;
    classJson["name"] = className;
    classJson["kind"] = "class";
    classJson["fields"] = json::array();
    classJson["methods"] = json::array();

    if (Match(Token::Type::Colon))
    {
        classJson["base"] = Advance().value;
    }

    Match(Token::Type::LBrace);

    // Parse members
    while (current < tokens.size() && !Check(Token::Type::RBrace))
    {
        if (Peek().value == "field")
        {
            classJson["fields"].push_back(ParseField());
        }
        else if (Peek().value == "method")
        {
            classJson["methods"].push_back(ParseMethod());
        }
        else if (Peek().value == "constructor")
        {
            classJson["methods"].push_back(ParseMethod());
        }
        else
        {
            Advance();
        }
    }

    Match(Token::Type::RBrace);
    return classJson;
}

json IRTextParser::Parser::ParseInterface()
{
    Advance(); // 'interface'
    std::string ifaceName = Advance().value;

    json ifaceJson;
    ifaceJson["name"] = ifaceName;
    ifaceJson["kind"] = "interface";
    ifaceJson["methods"] = json::array();

    Match(Token::Type::LBrace);

    while (current < tokens.size() && !Check(Token::Type::RBrace))
    {
        if (Peek().value == "method")
        {
            ifaceJson["methods"].push_back(ParseMethod());
        }
        else
        {
            Advance();
        }
    }

    Match(Token::Type::RBrace);
    return ifaceJson;
}

json IRTextParser::Parser::ParseStruct()
{
    Advance(); // 'struct'
    std::string structName = Advance().value;

    json structJson;
    structJson["name"] = structName;
    structJson["kind"] = "struct";
    structJson["fields"] = json::array();

    Match(Token::Type::LBrace);

    while (current < tokens.size() && !Check(Token::Type::RBrace))
    {
        if (Peek().value == "field")
        {
            structJson["fields"].push_back(ParseField());
        }
        else
        {
            Advance();
        }
    }

    Match(Token::Type::RBrace);
    return structJson;
}

json IRTextParser::Parser::ParseField()
{
    Advance(); // 'field'

    std::string fieldName = Advance().value;
    Match(Token::Type::Colon);

    std::string fieldType = Advance().value;
    while (Check(Token::Type::Dot) || Check(Token::Type::LParen))
    {
        if (Match(Token::Type::Dot))
        {
            fieldType += "." + Advance().value;
        }
        else if (Check(Token::Type::LParen))
        {
            Advance();
            fieldType += "<";
            while (!Check(Token::Type::RParen))
            {
                fieldType += Advance().value;
            }
            Advance();
            fieldType += ">";
        }
    }

    json field;
    field["name"] = fieldName;
    field["type"] = fieldType;
    return field;
}

json IRTextParser::Parser::ParseMethod()
{
    bool isConstructor = Peek().value == "constructor";
    Advance(); // 'method' or 'constructor'

    std::string methodName = Advance().value;
    Match(Token::Type::LParen);

    json method;
    method["name"] = methodName;
    method["parameters"] = json::array();
    method["body"] = json::array();

    // Parse parameters
    while (!Check(Token::Type::RParen))
    {
        if (Check(Token::Type::Identifier))
        {
            std::string paramName = Advance().value;
            Match(Token::Type::Colon);
            std::string paramType = Advance().value;

            json param;
            param["name"] = paramName;
            param["type"] = paramType;
            method["parameters"].push_back(param);

            if (Check(Token::Type::Comma))
            {
                Advance();
            }
        }
        else
        {
            Advance();
        }
    }

    Match(Token::Type::RParen);

    // Parse return type
    std::string returnType = "void";
    if (Match(Token::Type::Arrow))
    {
        returnType = Advance().value;
    }
    method["returnType"] = returnType;

    if (!isConstructor)
    {
        method["isStatic"] = false;
    }

    // Parse method body - collect all instructions
    if (Match(Token::Type::LBrace))
    {
        json bodyInstructions = json::array();
        int braceCount = 1;
        
        while (braceCount > 0 && current < tokens.size())
        {
            if (Check(Token::Type::LBrace))
            {
                braceCount++;
                Advance();
            }
            else if (Check(Token::Type::RBrace))
            {
                braceCount--;
                if (braceCount > 0)
                {
                    Advance();
                }
            }
            else if (Check(Token::Type::Instruction))
            {
                // Parse instruction
                std::string instructionName = Advance().value;
                json instruction;
                instruction["opCode"] = instructionName;

                // Collect instruction arguments until next instruction or brace
                std::vector<std::string> args;
                while (current < tokens.size() &&
                       !Check(Token::Type::Instruction) &&
                       !Check(Token::Type::LBrace) &&
                       !Check(Token::Type::RBrace) &&
                       !Check(Token::Type::Newline))
                {
                    args.push_back(Peek().value);
                    Advance();
                }

                std::cerr << "  Parsed instruction: " << instructionName << " with " << args.size() << " arguments";
                for (size_t i = 0; i < args.size(); ++i)
                {
                    std::cerr << " [" << i << "]=" << args[i];
                }
                std::cerr << std::endl;

                // Build operand based on instruction type and arguments
                json operand = json::object();
                bool hasOperand = false;
                
                if (instructionName == "ldarg" && !args.empty())
                {
                    operand["argumentName"] = args[0];
                    hasOperand = true;
                }
                else if (instructionName == "starg" && !args.empty())
                {
                    operand["argumentName"] = args[0];
                    hasOperand = true;
                }
                else if (instructionName == "ldloc" && !args.empty())
                {
                    operand["localName"] = args[0];
                    hasOperand = true;
                }
                else if (instructionName == "stloc" && !args.empty())
                {
                    operand["localName"] = args[0];
                    hasOperand = true;
                }
                else if (instructionName == "ldstr" && !args.empty())
                {
                    operand["value"] = args[0];
                    operand["type"] = "string";
                    hasOperand = true;
                    std::cerr << "    ldstr operand: value=" << args[0] << std::endl;
                }
                else if ((instructionName == "ldc" || instructionName == "ldc.i4" || instructionName == "ldc.i8") && !args.empty())
                {
                    operand["value"] = args[0];
                    operand["type"] = "int32";
                    hasOperand = true;
                }
                else if ((instructionName == "ldc.r4" || instructionName == "ldc.r8") && !args.empty())
                {
                    operand["value"] = args[0];
                    operand["type"] = "float64";
                    hasOperand = true;
                }
                else if ((instructionName == "call" || instructionName == "callvirt") && !args.empty())
                {
                    // Parse method reference: "Type.Method ( param1, param2 ) -> returnType"
                    json methodObj = parseMethodReference(args);
                    operand["method"] = methodObj;
                    hasOperand = true;
                    std::cerr << "    call operand: method=" << methodObj.dump() << std::endl;
                }
                else if ((instructionName == "ldfld" || instructionName == "stfld") && !args.empty())
                {
                    operand["field"] = args[0];
                    hasOperand = true;
                }
                else if (!args.empty())
                {
                    // Generic operand for instructions with arguments
                    operand["arguments"] = args;
                    hasOperand = true;
                    std::cerr << "    generic operand with " << args.size() << " arguments" << std::endl;
                }

                if (hasOperand)
                {
                    instruction["operand"] = operand;
                    std::cerr << "    operand JSON: " << operand.dump() << std::endl;
                }

                bodyInstructions.push_back(instruction);
            }
            else if (Check(Token::Type::Newline))
            {
                Advance();
            }
            else
            {
                Advance();
            }
        }

        method["body"] = bodyInstructions;
        Match(Token::Type::RBrace);
    }

    return method;
}

json IRTextParser::Parser::ParseTypeReference(const std::string& typeStr)
{
    return json::object();
}

std::string IRTextParser::Parser::GetTypeJsonRepresentation(const std::string& typeStr)
{
    return typeStr;
}

// Helper function to parse method reference from call instruction arguments
json IRTextParser::Parser::parseMethodReference(const std::vector<std::string>& args)
{
    // Expected format: ["Type.Method", "(", "param1", "param2", ")", "->", "returnType"]
    // or: ["Type.Method", "(", ")", "->", "returnType"]
    
    json methodObj = json::object();
    
    if (args.empty()) return methodObj;
    
    // Parse declaring type and method name from first arg like "System.Console.WriteLine"
    std::string fullMethodName = args[0];
    size_t lastDot = fullMethodName.find_last_of('.');
    if (lastDot != std::string::npos) {
        methodObj["declaringType"] = fullMethodName.substr(0, lastDot);
        methodObj["name"] = fullMethodName.substr(lastDot + 1);
    } else {
        methodObj["declaringType"] = "object"; // fallback
        methodObj["name"] = fullMethodName;
    }
    
    // Parse parameter types
    json paramTypes = json::array();
    bool inParams = false;
    std::string returnType = "void";
    
    for (size_t i = 1; i < args.size(); ++i) {
        const std::string& arg = args[i];
        if (arg == "(") {
            inParams = true;
        } else if (arg == ")") {
            inParams = false;
        } else if (arg == "->") {
            // Next arg should be return type
            if (i + 1 < args.size()) {
                returnType = args[i + 1];
            }
            break;
        } else if (inParams && !arg.empty()) {
            // This is a parameter type
            paramTypes.push_back(arg);
        }
    }
    
    methodObj["parameterTypes"] = paramTypes;
    methodObj["returnType"] = returnType;
    
    return methodObj;
}

// ============================================================================
// IRTextParser Public Implementation
// ============================================================================

std::shared_ptr<VirtualMachine> IRTextParser::ParseToVirtualMachine(const std::string& irText)
{
    try
    {
        json jsonIR = ParseToJson(irText);
        return IRLoader::LoadFromString(jsonIR.dump());
    }
    catch (const std::exception& e)
    {
        throw std::runtime_error(std::string("IR parsing error: ") + e.what());
    }
}

json IRTextParser::ParseToJson(const std::string& irText)
{
    // Tokenize
    Lexer lexer(irText);
    std::vector<Token> tokens;

    Token token = lexer.NextToken();
    while (token.type != Token::Type::EOF_TOKEN)
    {
        tokens.push_back(token);
        token = lexer.NextToken();
    }
    tokens.push_back(token); // Add EOF token

    std::cerr << "[IRTextParser] Generated " << tokens.size() << " tokens" << std::endl;

    // Parse
    Parser parser(tokens);
    json result = parser.ParseModule();

    std::cerr << "[IRTextParser] Generated JSON module:" << std::endl;
    std::cerr << result.dump(2) << std::endl;

    return result;
}

std::vector<uint8_t> IRTextParser::ParseToFOB(const std::string& irText)
{
    // Parse text IR to JSON first
    json jsonIR = ParseToJson(irText);

    // Load JSON into VirtualMachine
    std::string jsonStr = jsonIR.dump();
    auto vm = IRLoader::LoadFromString(jsonStr);

    // TODO: Implement proper FOB serialization
    // For now, return a minimal FOB header + JSON data
    // This is not true FOB format but allows the pipeline to work

    std::vector<uint8_t> fobData;

    // FOB header: "FOB" + fork name length + "OBJECTIR,FOB" + file size + entry point
    fobData.push_back('F');
    fobData.push_back('O');
    fobData.push_back('B');

    std::string forkName = "OBJECTIR,FOB";
    fobData.push_back(static_cast<uint8_t>(forkName.length()));
    for (char c : forkName) {
        fobData.push_back(static_cast<uint8_t>(c));
    }

    // File size (placeholder - we'll fill this in at the end)
    size_t fileSizePos = fobData.size();
    for (int i = 0; i < 4; ++i) fobData.push_back(0);

    // Entry point (0xFFFFFFFF = no specific entry point)
    fobData.push_back(0xFF);
    fobData.push_back(0xFF);
    fobData.push_back(0xFF);
    fobData.push_back(0xFF);

    // For now, just append the JSON as binary data
    // This is not proper FOB format but allows testing the pipeline
    for (char c : jsonStr) {
        fobData.push_back(static_cast<uint8_t>(c));
    }

    // Fill in file size
    uint32_t fileSize = static_cast<uint32_t>(fobData.size());
    fobData[fileSizePos] = fileSize & 0xFF;
    fobData[fileSizePos + 1] = (fileSize >> 8) & 0xFF;
    fobData[fileSizePos + 2] = (fileSize >> 16) & 0xFF;
    fobData[fileSizePos + 3] = (fileSize >> 24) & 0xFF;

    return fobData;
}

// ============================================================================
// Utility Functions
// ============================================================================

std::string IRTextParser::TrimString(const std::string& str)
{
    size_t start = str.find_first_not_of(" \t\r\n");
    if (start == std::string::npos)
        return "";
    size_t end = str.find_last_not_of(" \t\r\n");
    return str.substr(start, end - start + 1);
}

std::vector<std::string> IRTextParser::SplitString(const std::string& str, char delimiter)
{
    std::vector<std::string> result;
    std::stringstream ss(str);
    std::string item;

    while (std::getline(ss, item, delimiter))
    {
        result.push_back(item);
    }

    return result;
}

bool IRTextParser::IsKeyword(const std::string& word)
{
    static const std::vector<std::string> keywords = {
        "module", "class", "interface", "struct", "enum",
        "method", "field", "property", "constructor",
        "static", "virtual", "abstract", "private", "public", "protected",
        "local", "if", "else", "while", "for", "switch", "case",
        "return", "implements", "version"
    };

    return std::find(keywords.begin(), keywords.end(), word) != keywords.end();
}

bool IRTextParser::IsInstruction(const std::string& word)
{
    static const std::vector<std::string> instructions = {
        "ldarg", "ldloc", "ldfld", "stloc", "stfld", "ldc", "ldc.i4", "ldc.r8",
        "ldstr", "ldnull", "add", "sub", "mul", "div", "rem", "neg",
        "ceq", "cgt", "clt", "call", "callvirt", "newobj", "dup", "pop",
        "ret", "br", "beq", "bne", "brfalse", "brtrue", "bgt", "blt", "bge", "ble",
        "if", "while", "for", "switch", "case", "default", "break", "continue"
    };

    return std::find(instructions.begin(), instructions.end(), word) != instructions.end();
}

bool IRTextParser::IsPrimitiveType(const std::string& typeStr)
{
    static const std::vector<std::string> primitives = {
        "void", "bool",
        "int8", "int16", "int32", "int64",
        "uint8", "uint16", "uint32", "uint64",
        "float32", "float64",
        "char", "string"
    };

    return std::find(primitives.begin(), primitives.end(), typeStr) != primitives.end();
}

} // namespace ObjectIR
