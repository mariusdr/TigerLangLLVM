#include "Lexer.h"
#include <iostream>

namespace tig {

std::optional<char> Lexer::Cursor::consume() {
  return done() ? std::nullopt : std::optional(*(pos++));
}

bool Lexer::Cursor::match(char c) {
  if (LLVM_UNLIKELY(done())) {
    return false;
  }
  if (peek() == c) {
    pos++;
    return true;
  }
  return false;
}

bool Lexer::Cursor::match(const char *sv, size_t length) {
  if (LLVM_UNLIKELY(done())) {
    return false;
  }
  if (strncmp(sv, pos, length) == 0) {
    pos += length;
    return true;
  }
  return false;
}

Lexer::Cursor &Lexer::Cursor::skipComment() {
  if (LLVM_UNLIKELY(done())) {
    return *this;
  }
  unsigned int nestedCommentLevel = 0;
  if (match("/*", 2)) {
    nestedCommentLevel++;
  }
  while (nestedCommentLevel > 0) {
    if (match("/*", 2)) {
      nestedCommentLevel++;
    } else if (match("*/", 2)) {
      nestedCommentLevel--;
    } else {
      consume();
    }
  }
  return *this;
}

Lexer::Cursor &Lexer::Cursor::skipWhitespace() {
  while (isspace(peek())) {
    pos++;
  }
  return *this;
}

// static member definition
llvm::StringMap<TokenKind> Lexer::kwMap;

void Lexer::initKwMap() {
  if (kwMap.size() == 0) {
    kwMap.insert({llvm::StringRef("array"), TokenKind::Array});
    kwMap.insert({llvm::StringRef("break"), TokenKind::Break});
    kwMap.insert({llvm::StringRef("do"), TokenKind::Do});
    kwMap.insert({llvm::StringRef("else"), TokenKind::Else});
    kwMap.insert({llvm::StringRef("end"), TokenKind::End});
    kwMap.insert({llvm::StringRef("for"), TokenKind::For});
    kwMap.insert({llvm::StringRef("function"), TokenKind::Function});
    kwMap.insert({llvm::StringRef("if"), TokenKind::If});
    kwMap.insert({llvm::StringRef("in"), TokenKind::In});
    kwMap.insert({llvm::StringRef("let"), TokenKind::Let});
    kwMap.insert({llvm::StringRef("nil"), TokenKind::Nil});
    kwMap.insert({llvm::StringRef("of"), TokenKind::Of});
    kwMap.insert({llvm::StringRef("then"), TokenKind::Then});
    kwMap.insert({llvm::StringRef("to"), TokenKind::To});
    kwMap.insert({llvm::StringRef("type"), TokenKind::Type});
    kwMap.insert({llvm::StringRef("var"), TokenKind::Var});
    kwMap.insert({llvm::StringRef("while"), TokenKind::While});
  }
}

Lexer::Lexer(const llvm::MemoryBufferRef fileBuffer):
  cur(fileBuffer.getBufferStart(), fileBuffer.getBufferEnd())
{
  Lexer::initKwMap();
}

Lexer::Lexer(llvm::StringRef str):
  cur(str.begin(), str.end())
{
  Lexer::initKwMap();
}

llvm::Expected<Token> Lexer::emitKeywordOrIdentifier() {
  auto lex = cur.consumeWhile([](char c) {
    return !isspace(c) && isalnum(c);
  });
  if (!lex) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, "input sequence is no valid identifier");
  }
  auto it = kwMap.find(*lex);
  if (it != kwMap.end()) {
    return Token(it->second);
  }
  return Token(*lex, TokenKind::Identifier);
}

llvm::Expected<Token> Lexer::emitIntegerLiteral() {
  int pos = 0;
  bool isHexLiteral = false;
  auto lex = cur.consumeWhile([&pos, &isHexLiteral](char c) {
    if (c == 'x' && pos == 1) {
      isHexLiteral = true;
      return true;
    }
    pos++;
    bool res = '0' <= c && c <= '9';
    if (!res && isHexLiteral) {
      res = 'a' <= c && c <= 'f';
    }
    return res;
  });
  if (!lex) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, "input sequence is no valid integer literal");
  }
  return Token(*lex, TokenKind::IntegerLiteral);
}

llvm::Expected<Token> Lexer::emitStringLiteral() {
  cur.consume(); // eat opening '"' 
  auto lex = cur.consumeWhile([](char c) {
    return c != '"';
  });
  cur.consume(); // eat closing '"' 
  if (!lex) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, "input sequence is no valid string literal");
  }
  return Token(*lex, TokenKind::StringLiteral);
}

llvm::Expected<Token> Lexer::emitSymbol() {
  if (cur.match(":=", 2)) {
    return Token(TokenKind::Define);
  }
  if (cur.match("<>", 2)) {
    return Token(TokenKind::NotEquals);
  }
  if (cur.match("<=", 2)) {
    return Token(TokenKind::LessEquals);
  }
  if (cur.match(">=", 2)) {
    return Token(TokenKind::GreaterEquals);
  }
  if (cur.match('<')) { return Token(TokenKind::Less); }
  if (cur.match('>')) { return Token(TokenKind::Greater); }
  if (cur.match(',')) { return Token(TokenKind::Comma); }
  if (cur.match(':')) { return Token(TokenKind::Colon); }
  if (cur.match(';')) { return Token(TokenKind::Semicolon); }
  if (cur.match('.')) { return Token(TokenKind::Dot); }
  if (cur.match('+')) { return Token(TokenKind::Plus); }
  if (cur.match('-')) { return Token(TokenKind::Minus); }
  if (cur.match('*')) { return Token(TokenKind::Star); }
  if (cur.match('/')) { return Token(TokenKind::Slash); }
  if (cur.match('&')) { return Token(TokenKind::And); }
  if (cur.match('|')) { return Token(TokenKind::Or); }
  if (cur.match('+')) { return Token(TokenKind::Plus); }
  if (cur.match('(')) { return Token(TokenKind::LeftParen); }
  if (cur.match(')')) { return Token(TokenKind::RightParen); }
  if (cur.match('{')) { return Token(TokenKind::LeftBrace); }
  if (cur.match('}')) { return Token(TokenKind::RightBrace); }
  if (cur.match('[')) { return Token(TokenKind::LeftBracket); }
  if (cur.match(']')) { return Token(TokenKind::RightBracket); }
  if (cur.match('=')) { return Token(TokenKind::Equals); }

  auto ec = std::make_error_code(std::errc::invalid_argument);
  return llvm::createStringError(ec, "could not deduce Token kind");
}

llvm::Expected<Token> Lexer::getNext() {
  cur.skipWhitespace().skipComment().skipWhitespace();
  if (cur.done()) {
    return Token(TokenKind::EndOfFile);
  } else if (std::isalpha(cur.peek())) {
    return emitKeywordOrIdentifier();
  } else if (std::isdigit(cur.peek())) {
    return emitIntegerLiteral();
  } else if (cur.peek() == '"') {
    return emitStringLiteral();
  }
  return emitSymbol();
}

} // namespace 
