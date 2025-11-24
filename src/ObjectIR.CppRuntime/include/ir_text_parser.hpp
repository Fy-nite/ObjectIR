#pragma once

#include "objectir_runtime.hpp"
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
#include <memory>
#include <regex>
#include <sstream>

namespace ObjectIR
{
    using json = nlohmann::json;

    // ============================================================================
    // IR Text Parser - Parse ObjectIR textual syntax and convert to JSON/VirtualMachine
    // ============================================================================

    /// Parses ObjectIR textual syntax (as shown in COMPLETE_EXAMPLE.md) into a VirtualMachine
    /// or JSON representation suitable for use with IRLoader
    class OBJECTIR_API IRTextParser
    {
    public:
        /// Parse ObjectIR textual IR code and return a VirtualMachine
        /// Example:
        ///   module TodoApp
        ///   class Main {
        ///       method Main() -> void {
        ///           ldstr "Hello"
        ///           call System.Console.WriteLine(string) -> void
        ///           ret
        ///       }
        ///   }
        static std::shared_ptr<VirtualMachine> ParseToVirtualMachine(const std::string& irText);

        /// Parse ObjectIR textual IR code and return JSON representation
        static json ParseToJson(const std::string& irText);

        /// Parse ObjectIR textual IR code and return binary FOB data
        static std::vector<uint8_t> ParseToFOB(const std::string& irText);

    private:
        IRTextParser() = default;

        struct Token
        {
            enum class Type
            {
                Keyword,      // module, class, method, field, etc.
                Identifier,   // Names
                Type,         // int32, string, bool, etc.
                Number,       // 123
                String,       // "text"
                Arrow,        // ->
                Colon,        // :
                LBrace,       // {
                RBrace,       // }
                LParen,       // (
                RParen,       // )
                Comma,        // ,
                Dot,          // .
                Instruction,  // ldstr, call, ret, etc.
                Comment,      // // comment
                Newline,
                EOF_TOKEN
            };

            Type type;
            std::string value;
            int line;
            int column;

            Token(Type t, const std::string& v, int l = 0, int c = 0)
                : type(t), value(v), line(l), column(c) {}
        };

        class Lexer
        {
        public:
            explicit Lexer(const std::string& input);
            Token NextToken();
            bool IsAtEnd() const;

        private:
            const std::string& input;
            size_t position = 0;
            int line = 1;
            int column = 1;

            char Current() const;
            char Peek(size_t offset = 1) const;
            void Advance();
            void SkipWhitespace();
            void SkipComment();
            Token MakeToken(Token::Type type, const std::string& value);
            std::string ReadIdentifier();
            std::string ReadString();
            std::string ReadNumber();
        };

        class Parser
        {
        public:
            explicit Parser(const std::vector<Token>& tokens);
            json ParseModule();

        private:
            const std::vector<Token>& tokens;
            size_t current = 0;

            bool Match(Token::Type type);
            bool Check(Token::Type type) const;
            Token Advance();
            Token Peek() const;
            Token Previous() const;
            void Consume(Token::Type type, const std::string& message);

            json ParseModuleHeader();
            json ParseClass();
            json ParseInterface();
            json ParseStruct();
            json ParseField();
            json ParseMethod();
            json ParseTypeReference(const std::string& typeStr);
            std::string GetTypeJsonRepresentation(const std::string& typeStr);
            json parseMethodReference(const std::vector<std::string>& args);
        };

        // Utility functions
        static std::string TrimString(const std::string& str);
        static std::vector<std::string> SplitString(const std::string& str, char delimiter);
        static bool IsKeyword(const std::string& word);
        static bool IsInstruction(const std::string& word);
        static bool IsPrimitiveType(const std::string& typeStr);
    };

} // namespace ObjectIR
