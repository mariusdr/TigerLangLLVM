#ifndef TIGER_LANG_LEXER_H
#define TIGER_LANG_LEXER_H

#include <optional>
#include <cstring>
#include "llvm/Support/Error.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryBufferRef.h"
#include "llvm/ADT/StringMap.h"

#include "Token.h"

namespace tig {

class Lexer {
public:
  explicit Lexer(const llvm::MemoryBufferRef fileBuffer);
  Lexer(llvm::StringRef str);
  Lexer(const Lexer&) = delete;
  Lexer& operator=(const Lexer&) = delete;

  Lexer(Lexer &&other) noexcept:
    cur(std::move(other.cur)) 
  {}

  Lexer &operator=(Lexer &&other) noexcept {
    cur = std::move(other.cur);
    return *this;
  }

  llvm::Expected<Token> getNext();

  inline bool done() const {
    return cur.done();
  }
private:
  llvm::Expected<Token> emitKeywordOrIdentifier();
  llvm::Expected<Token> emitIntegerLiteral();
  llvm::Expected<Token> emitStringLiteral();
  llvm::Expected<Token> emitSymbol();

  struct Cursor {
    Cursor(const char *pos, const char *end):
      pos(pos), end(end)
    {}

    inline bool done() const {
      return pos >= end;
    }

    inline char peek(uint32_t off = 0) const {
      return done() ? '\0' : *(pos + off);
    }

    std::optional<char> consume();
    Cursor &skipWhitespace();
    Cursor &skipComment();
    bool match(char c);
    bool match(const char *sv, size_t length);

    template <typename Pred> 
    std::optional<char> consumeIf(Pred pred) {
      if (pred(peek())) {
        return consume();
      }
      return std::nullopt;
    }

    template <typename Pred>
    std::optional<llvm::StringRef> consumeWhile(Pred pred) {
      size_t len = 0;
      const char *start = pos;
      while (pred(peek())) {
        if (LLVM_UNLIKELY(!consume())) {
          return std::nullopt;
        }
        len++;
      }
      return std::optional(llvm::StringRef(start, len));
    }

    const char *pos;
    const char *end;
  };

  static void initKwMap();
  static llvm::StringMap<TokenKind> kwMap;

  Cursor cur;
};

} // namespace
#endif
