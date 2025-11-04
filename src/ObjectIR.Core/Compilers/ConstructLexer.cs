namespace ObjectIR.Core.Compilers;

using System;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Tokenizes Construct language source code
/// </summary>
public class ConstructLexer
{
    private readonly string _source;
    private int _position = 0;
    private int _line = 1;
    private int _column = 1;
    private readonly List<Token> _tokens = new();

    public ConstructLexer(string source)
    {
        _source = source;
    }

    public List<Token> Tokenize()
    {
        while (_position < _source.Length)
        {
            SkipWhitespaceAndComments();

            if (_position >= _source.Length)
                break;

            char current = _source[_position];

            if (char.IsLetter(current) || current == '_')
            {
                ReadIdentifierOrKeyword();
            }
            else if (char.IsDigit(current))
            {
                ReadNumber();
            }
            else if (current == '"')
            {
                ReadString();
            }
            else if (current == '{')
            {
                AddToken(TokenType.LeftBrace, "{");
                Advance();
            }
            else if (current == '}')
            {
                AddToken(TokenType.RightBrace, "}");
                Advance();
            }
            else if (current == '(')
            {
                AddToken(TokenType.LeftParen, "(");
                Advance();
            }
            else if (current == ')')
            {
                AddToken(TokenType.RightParen, ")");
                Advance();
            }
            else if (current == ';')
            {
                AddToken(TokenType.Semicolon, ";");
                Advance();
            }
            else if (current == ',')
            {
                AddToken(TokenType.Comma, ",");
                Advance();
            }
            else if (current == ':')
            {
                AddToken(TokenType.Colon, ":");
                Advance();
            }
            else if (current == '.')
            {
                AddToken(TokenType.Dot, ".");
                Advance();
            }
            else if (current == '=')
            {
                if (Peek() == '=')
                {
                    AddToken(TokenType.EqualEqual, "==");
                    Advance();
                    Advance();
                }
                else
                {
                    AddToken(TokenType.Equal, "=");
                    Advance();
                }
            }
            else if (current == '<')
            {
                if (Peek() == '=')
                {
                    AddToken(TokenType.LessEqual, "<=");
                    Advance();
                    Advance();
                }
                else
                {
                    AddToken(TokenType.Less, "<");
                    Advance();
                }
            }
            else if (current == '>')
            {
                if (Peek() == '=')
                {
                    AddToken(TokenType.GreaterEqual, ">=");
                    Advance();
                    Advance();
                }
                else
                {
                    AddToken(TokenType.Greater, ">");
                    Advance();
                }
            }
            else if (current == '+')
            {
                AddToken(TokenType.Plus, "+");
                Advance();
            }
            else if (current == '-')
            {
                AddToken(TokenType.Minus, "-");
                Advance();
            }
            else if (current == '*')
            {
                AddToken(TokenType.Star, "*");
                Advance();
            }
            else if (current == '/')
            {
                AddToken(TokenType.Slash, "/");
                Advance();
            }
            else if (current == '!')
            {
                if (Peek() == '=')
                {
                    AddToken(TokenType.BangEqual, "!=");
                    Advance();
                    Advance();
                }
                else
                {
                    AddToken(TokenType.Bang, "!");
                    Advance();
                }
            }
            else
            {
                throw new CompileException($"Unexpected character '{current}' at line {_line}, column {_column}");
            }
        }

        AddToken(TokenType.EOF, "");
        return _tokens;
    }

    private void SkipWhitespaceAndComments()
    {
        while (_position < _source.Length)
        {
            char current = _source[_position];

            if (char.IsWhiteSpace(current))
            {
                if (current == '\n')
                {
                    _line++;
                    _column = 1;
                }
                else
                {
                    _column++;
                }
                _position++;
            }
            else if (current == '/' && Peek() == '/')
            {
                // Line comment
                while (_position < _source.Length && _source[_position] != '\n')
                    _position++;
            }
            else if (current == '/' && Peek() == '*')
            {
                // Block comment
                _position += 2;
                while (_position < _source.Length - 1)
                {
                    if (_source[_position] == '*' && _source[_position + 1] == '/')
                    {
                        _position += 2;
                        break;
                    }
                    if (_source[_position] == '\n')
                    {
                        _line++;
                        _column = 1;
                    }
                    _position++;
                }
            }
            else
            {
                break;
            }
        }
    }

    private void ReadIdentifierOrKeyword()
    {
        int start = _position;
        while (_position < _source.Length && (char.IsLetterOrDigit(_source[_position]) || _source[_position] == '_'))
        {
            _position++;
            _column++;
        }

        string text = _source[start.._position];

        TokenType type = text switch
        {
            "Contract" => TokenType.Contract,
            "fn" => TokenType.Fn,
            "var" => TokenType.Var,
            "if" => TokenType.If,
            "else" => TokenType.Else,
            "while" => TokenType.While,
            "switch" => TokenType.Switch,
            "case" => TokenType.Case,
            "return" => TokenType.Return,
            "Int" => TokenType.TypeInt,
            "String" => TokenType.TypeString,
            "Bool" => TokenType.TypeBool,
            "true" => TokenType.True,
            "false" => TokenType.False,
            "IO" => TokenType.IO,
            _ => TokenType.Identifier
        };

        AddToken(type, text);
    }

    private void ReadNumber()
    {
        int start = _position;
        while (_position < _source.Length && char.IsDigit(_source[_position]))
        {
            _position++;
            _column++;
        }

        string text = _source[start.._position];
        AddToken(TokenType.Number, text);
    }

    private void ReadString()
    {
        _position++; // Skip opening quote
        _column++;
        int start = _position;

        while (_position < _source.Length && _source[_position] != '"')
        {
            if (_source[_position] == '\\' && _position + 1 < _source.Length)
            {
                _position += 2;
                _column += 2;
            }
            else
            {
                _position++;
                _column++;
            }
        }

        if (_position >= _source.Length)
            throw new CompileException($"Unterminated string at line {_line}");

        string text = _source[start.._position];
        _position++; // Skip closing quote
        _column++;

        AddToken(TokenType.String, text);
    }

    private char Peek()
    {
        if (_position + 1 < _source.Length)
            return _source[_position + 1];
        return '\0';
    }

    private void Advance()
    {
        _position++;
        _column++;
    }

    private void AddToken(TokenType type, string text)
    {
        _tokens.Add(new Token(type, text, _line, _column - text.Length));
    }
}

/// <summary>
/// Token types for Construct language
/// </summary>
public enum TokenType
{
    // Literals
    Number,
    String,
    Identifier,

    // Keywords
    Contract,
    Fn,
    Var,
    If,
    Else,
    While,
    Switch,
    Case,
    Return,
    True,
    False,
    IO,

    // Type keywords
    TypeInt,
    TypeString,
    TypeBool,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Bang,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Colon,
    Dot,

    // Special
    EOF
}

/// <summary>
/// Represents a token
/// </summary>
public record Token(TokenType Type, string Text, int Line, int Column);

/// <summary>
/// Compilation error
/// </summary>
public class CompileException : Exception
{
    public CompileException(string message) : base(message) { }
}
