#ifndef TIGER_LANG_PARSER_H
#define TIGER_LANG_PARSER_H

#include "llvm/Support/Allocator.h"
#include "llvm/Support/FormatVariadic.h"

#include "Ast.h"
#include "Lexer.h"
#include "ErrorMessage.h"
#include "SymbolTable.h"

namespace tig {

class ArenaScopeGuard {
public:
  ArenaScopeGuard() = default;

  ArenaScopeGuard(ArenaScopeGuard &&other) noexcept:
    arena(std::move(other.arena))
  {}

  ArenaScopeGuard &operator=(ArenaScopeGuard &&other) noexcept {
    if (this == &other) {
      return *this;
    }
    arena = std::move(other.arena);
    return *this;
  }

  inline ~ArenaScopeGuard() {
    arena.Reset();
  }
  inline llvm::BumpPtrAllocator &getAlloc() {
    return arena;
  }
private:
  llvm::BumpPtrAllocator arena;
};

/// Wraps the Lexer into a peekable stream of tokens.
class TokenStream {
public:
  explicit TokenStream(Lexer &&lexer):
    lexer(std::move(lexer)),
    top(this->lexer.getNext()),
    flags(0)
  {}

  [[nodiscard]] inline bool goOn() const {
    return !(flags.endOfFileReached | flags.errorOccured);
  }
  
  inline bool inErrorState() const {
    return flags.errorOccured;
  }
  
  [[nodiscard]] const Token &peekAhead() const;
  [[nodiscard]] llvm::Expected<Token> next();

  [[nodiscard]] llvm::Error takeError() {
    return top.takeError();
  }

private:
  Lexer lexer;
  llvm::Expected<Token> top;

  struct FlagByte {
    FlagByte(uint8_t b) {
      memset(this, b, sizeof(FlagByte));
    }
    unsigned endOfFileReached : 1;
    unsigned errorOccured : 1;
    unsigned errorOnPeek : 1; 
    unsigned _pad : 5;
  } flags;
};

class Parser {
public:
  Parser(const llvm::MemoryBufferRef fileBuffer, llvm::BumpPtrAllocator &arena);
  Parser(const llvm::StringRef source, llvm::BumpPtrAllocator &arena);

  [[nodiscard]] llvm::Expected<ast::ExprPtr> parse();

private:
  llvm::BumpPtrAllocator &arena;
  TokenStream tokenStream;
  llvm::Expected<Token> current;
  ScopedSymbolTablePtr symbolTable;
  
  /// This one keeps track of function symbols that were used in the current scope 
  /// but not yet declared. When the scope closes the parser checks if there
  /// is still any undeclared declaration left. If so, an error will be raised.
  ///
  /// This mechanisms allows for mutually recursive function declarations, i.e.
  ///
  /// let function foo(x: int) = if x > 0 then bar(x - 1) else 0
  ///     function bar(x: int) = x + foo(x - 1)
  /// in 
  ///     foo(123) 
  /// end
  ///
  /// without the need for forward declarations or anything like that.
  ScopedSymbolTablePtr deferredDeclarations;

  enum class ScopeKind {
    /// Scope of declaration list in let-expr.
    LetScope,
    /// Scope of function parameters inside function body.
    FuncScope,
    Other
  };

  void pushScope(ScopeKind sk) {
    ScopedSymbolTablePtr newScope = makeScopedSymbolTable(std::move(symbolTable));
    symbolTable = std::move(newScope);
    if (sk == ScopeKind::LetScope) {
      ScopedSymbolTablePtr newDeferredDecls = makeScopedSymbolTable(std::move(deferredDeclarations));
      deferredDeclarations = std::move(newDeferredDecls);
    }
  }

  void popScope(ScopeKind sk) {
    ScopedSymbolTablePtr oldScope = symbolTable->takeParent();
    symbolTable = std::move(oldScope);
    if (sk == ScopeKind::LetScope) {
      ScopedSymbolTablePtr oldDeferredDecls = deferredDeclarations->takeParent();
      deferredDeclarations = std::move(oldDeferredDecls);
    }
  }

  [[nodiscard]] std::optional<ast::OperatorKind> comparisonOperator();
  [[nodiscard]] std::optional<ast::OperatorKind> additionOperator();
  [[nodiscard]] std::optional<ast::OperatorKind> multiplicationOperator();
  
  /// @name Main Parser Logic
  /// @{

  [[nodiscard]] llvm::Expected<ast::ExprPtr> orExpression();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> andExpression();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> comparison();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> term();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> factor();
  
  [[nodiscard]] llvm::Expected<ast::ExprPtr> expression();
  
  [[nodiscard]] llvm::Expected<ast::ExprPtr> afterIdentifier();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> afterTypeIdentifier();

  [[nodiscard]] llvm::Expected<ast::ExprPtr> strLit();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> intLit();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> nil();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> negation();

  [[nodiscard]] llvm::Expected<ast::ExprPtr> variable(ast::String &&id);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> simpleVariable(ast::String &&id);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> fieldVariable(ast::ExprPtr variable);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> subscriptVariable(ast::ExprPtr variable);
  
  [[nodiscard]] llvm::Expected<ast::ExprPtr> assignment(ast::ExprPtr &&lvalue);

  [[nodiscard]] llvm::Expected<ast::ExprPtr> functionCall(ast::String &&id);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> arrayConstructor(ast::String &&typeId);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> recordConstructor(ast::String &&typeId);

  [[nodiscard]] llvm::Expected<ast::ExprPtr> expressionSequence(TokenKind closer);
  [[nodiscard]] llvm::Expected<ast::ExprPtr> ifExpression();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> whileLoop();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> forLoopVariable();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> forLoop();
  [[nodiscard]] llvm::Expected<ast::ExprPtr> loopBreak();

  [[nodiscard]] llvm::Expected<ast::Decl> typeDeclaration();
  [[nodiscard]] llvm::Expected<ast::Decl> aliasTypeDeclaration(ast::String &&identifer);
  [[nodiscard]] llvm::Expected<ast::Decl> arrayTypeDeclaration(ast::String &&identifer);
  [[nodiscard]] llvm::Expected<ast::Decl> recordTypeDeclaration(ast::String &&identifer);

  /// @brief Parse let-expression. 
  /// Syntax:
  ///
  ///     let-expr ::= 'let' declaration-list? 'in' expr-seq? 'end'
  ///
  /// @return pointer to expression or error
  [[nodiscard]] llvm::Expected<ast::ExprPtr> letExpression();
  
  /// @brief Parse declaration list. 
  /// Syntax:
  ///
  ///     declaration-list ::= declaration-list? declaration
  ///     declaration ::= type-declaration 
  ///                   | variable-declaration
  ///                   | function-declaration
  ///
  /// @return pointer to expression or error
  [[nodiscard]] llvm::Expected<ast::DeclList> declarations();

  /// @brief Parse variable declaration. 
  /// Syntax:
  ///
  ///     variable-declaration ::= 'var' id := expr
  ///                            | 'var' id : type-id := expr
  ///
  /// @return pointer to expression or error
  [[nodiscard]] llvm::Expected<ast::Decl> variableDeclaration();

  [[nodiscard]] llvm::Expected<ast::Decl> functionDeclaration();
  [[nodiscard]] llvm::Expected<ast::ParamList> functionParams();

  /// @}

  llvm::Error checkDeferredDeclarations();

  [[nodiscard]] llvm::BumpPtrAllocator &getAlloc() {
    return arena;
  }

  [[nodiscard]] bool match(TokenKind ttype);
  void gotoNextToken();
  llvm::Error propagateLexerError();

  template <typename... Ts> 
  llvm::Error makeParserError(const char *err, Ts &&... vals) const {
    auto fmt = llvm::formatv(err, std::forward<Ts>(vals)...);
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, fmt.str());
  }

  template <typename... Ts> 
  llvm::Error makeParserError(MsgNo msgNum, Ts &&... vals) const {
    return makeParserError(message(msgNum), std::forward<Ts>(vals)...);
  }
};

} // namespace

#endif
