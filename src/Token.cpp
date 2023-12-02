#include "Token.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormatVariadic.h"

namespace tig {

static const llvm::StringRef ES = "";

Token::Token():
  lexeme(ES), type(TokenKind::Empty)
{}

Token::Token(llvm::StringRef lexeme, TokenKind tk):
  lexeme(lexeme), type(tk)
{}

Token::Token(TokenKind tk):
  lexeme(ES), type(tk)
{}

} // namespace tig

void llvm::format_provider<tig::TokenKind>::format(
  const tig::TokenKind &tk, 
  llvm::raw_ostream &out, 
  llvm::StringRef style)
{
  #define OSTK(tokenKind) \
    case tig::TokenKind::tokenKind: \
      out << #tokenKind; \
      break;

  switch (tk) {
    OSTK(Identifier)
    OSTK(IntegerLiteral)
    OSTK(StringLiteral)
    OSTK(Array)
    OSTK(Break)
    OSTK(Do)
    OSTK(Else)
    OSTK(End)
    OSTK(For)
    OSTK(Function)
    OSTK(If)
    OSTK(In)
    OSTK(Let)
    OSTK(Nil)
    OSTK(Of)
    OSTK(Then)
    OSTK(To)
    OSTK(Type)
    OSTK(Var)
    OSTK(While)
    OSTK(Comma)
    OSTK(Colon)
    OSTK(Semicolon)
    OSTK(Dot)
    OSTK(Plus)
    OSTK(Minus)
    OSTK(Star)
    OSTK(Slash)
    OSTK(And)
    OSTK(Or)
    OSTK(Define)
    OSTK(Equals)
    OSTK(NotEquals)
    OSTK(Less)
    OSTK(Greater)
    OSTK(LessEquals)
    OSTK(GreaterEquals)
    OSTK(LeftParen)
    OSTK(RightParen)
    OSTK(LeftBracket)
    OSTK(RightBracket)
    OSTK(LeftBrace)
    OSTK(RightBrace)
    OSTK(Empty)
    OSTK(EndOfFile)
  default:
    break;
  }
  #undef OSTK
}

void llvm::format_provider<tig::Token>::format(
  const tig::Token &tok, 
  llvm::raw_ostream &out, 
  llvm::StringRef style)
{
  if (tok.lexeme.size() > 0) {
    out << llvm::formatv("{ {0}, lex: '{0}' }", tok.type, tok.lexeme);
  } else {
    out << llvm::formatv("{ {0} }", tok.type);
  }
}