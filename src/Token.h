#ifndef TIGER_LANG_TOKEN_H
#define TIGER_LANG_TOKEN_H

#include <cstdint>
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormatProviders.h"

namespace tig {

enum class TokenKind : int {
  // Literals 
  Identifier,
  IntegerLiteral,
  StringLiteral,

  // Keywoards
  Array, Break, Do,
  Else, End, For, 
  Function, If, In, 
  Let, Nil, Of, Then,
  To, Type, Var, While,

  // Punctuation symbols
  Comma, Colon, Semicolon, Dot,
  Plus, Minus, Star, Slash, 
  And, Or, 

  // Operators 
  Define,
  Equals, NotEquals, 
  Less, Greater,
  LessEquals, GreaterEquals,

  // Braces
  LeftParen, RightParen, 
  LeftBracket, RightBracket,
  LeftBrace, RightBrace,

  Empty, EndOfFile
};

struct Token {
  Token();
  Token(llvm::StringRef lexeme, TokenKind tk);
  Token(TokenKind tk);

  bool isLiteral() const {
    return type == TokenKind::IntegerLiteral || type == TokenKind::StringLiteral;
  }
  bool isIdentifier() const {
    return type == TokenKind::Identifier;
  }
  bool isEndOfFile() const {
    return type == TokenKind::EndOfFile;
  }

  llvm::StringRef lexeme;
  TokenKind type;
};

} // namespace

namespace llvm {

template <>
struct format_provider<tig::TokenKind> {
  static void format(const tig::TokenKind &v, raw_ostream &stream, StringRef style);
};

template <>
struct format_provider<tig::Token> {
  static void format(const tig::Token &v, raw_ostream &stream, StringRef style);
};



} // namespace llvm

#endif
