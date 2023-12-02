#include "Parser.h"
#include "Support.h"
#include "ErrorMessage.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/ADT/StringSet.h"

#include <iostream>

namespace tig {

llvm::Expected<Token> TokenStream::next() {
  if (LLVM_UNLIKELY(flags.errorOccured)) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    return llvm::createStringError(ec, "TokenStream already in error state");
  }
  auto curr = std::move(top);
  if (LLVM_UNLIKELY(!curr)) {
    flags.errorOccured = 1;
  } else if (curr->type == TokenKind::EndOfFile) {
    flags.endOfFileReached = 1;
  }
  top = lexer.getNext(); 
  if (LLVM_UNLIKELY(!top)) {
    flags.errorOnPeek = 1;
  }
  return curr;
}

const Token &TokenStream::peekAhead() const {
  if (LLVM_UNLIKELY(flags.errorOnPeek)) {
    static const Token EMPTY(TokenKind::Empty);
    return EMPTY;
  }
  return *top;
}

Parser::Parser(const llvm::MemoryBufferRef fileBuffer, llvm::BumpPtrAllocator &arena):
  arena(arena),
  tokenStream(Lexer(fileBuffer)),
  current(Token(TokenKind::Empty)),
  symbolTable(makeTopLevelScope()),
  deferredDeclarations(makeScopedSymbolTable(nullptr))
{}

Parser::Parser(const llvm::StringRef source, llvm::BumpPtrAllocator &arena):
  arena(arena),
  tokenStream(Lexer(source)),
  current(Token(TokenKind::Empty)),
  symbolTable(makeTopLevelScope()),
  deferredDeclarations(makeScopedSymbolTable(nullptr))
{}

bool Parser::match(TokenKind ttype) {
  if (tokenStream.peekAhead().type == ttype) {
    current = tokenStream.next();
    return true;
  }
  return false;
}

void Parser::gotoNextToken() {
  current = tokenStream.next();
}

llvm::Error Parser::propagateLexerError() {
  if (auto e = current.takeError()) {
    auto ec = std::make_error_code(std::errc::invalid_argument);
    auto e0 = llvm::createStringError(ec, "Lexer Error");
    return llvm::joinErrors(std::move(e0), std::move(e));
  }
  return llvm::Error::success();
}

llvm::Expected<ast::ExprPtr> Parser::parse() {
  if (!match(TokenKind::Let)) {
    return makeParserError(MsgNo::MissTopLvlLetExpr);
  }
  return letExpression();
}

llvm::Expected<ast::ExprPtr> Parser::nil() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  return ast::Expr::create(ast::NilExpr(), getAlloc());
}

llvm::Expected<ast::ExprPtr> Parser::strLit() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  ast::StrLitExpr lit(current->lexeme, getAlloc());
  return ast::Expr::create(std::move(lit), getAlloc());
}

llvm::Expected<ast::ExprPtr> Parser::intLit() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  uint64_t val = 0;
  if (LLVM_UNLIKELY(current->lexeme.getAsInteger(0, val))) {
    return makeParserError(MsgNo::NotAnIntLiteral, current->lexeme);
  }
  return ast::Expr::create(ast::IntLitExpr(val), getAlloc());
}

llvm::Expected<ast::ExprPtr> Parser::negation() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto expr = orExpression();
  if (LLVM_UNLIKELY(!expr)) {
    return expr.takeError();
  }
  return ast::Expr::create(
    ast::OperatorExpr(
      ast::Expr::create(ast::IntLitExpr(0), arena),
      std::move(expr.get()),
      ast::OperatorKind::Minus), arena);
}

llvm::Expected<ast::ExprPtr> Parser::functionCall(ast::String &&id) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  ast::ExprList exprs(arena);
  while (!match(TokenKind::RightParen)) {
    if (!tokenStream.goOn()) {
      return makeParserError(MsgNo::MissingCloser, TokenKind::RightParen, "function parameter list");
    }
    if (exprs.size() > 0 && !match(TokenKind::Comma)) {
      return makeParserError(MsgNo::MissingSeperator, TokenKind::Comma);
    }
    auto expr = orExpression();
    if (LLVM_UNLIKELY(!expr)) {
      return expr.takeError();
    }
    exprs.push_back(std::move(expr.get()));
  }
  ast::CallExpr ce(std::move(id), std::move(exprs));
  return ast::Expr::create(std::move(ce), arena);
}

llvm::Expected<ast::ExprPtr> Parser::arrayConstructor(ast::String &&typeId) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto sizeExpr = orExpression();
  if (LLVM_UNLIKELY(!sizeExpr)) {
    return sizeExpr.takeError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::RightBracket))) {
    return makeParserError(MsgNo::MissingCloser, TokenKind::RightBracket, "array length");
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Of))) {
    return makeParserError(MsgNo::MissingKeywordBefore, "of", "initializer", "array constructor");
  }
  auto initExpr = orExpression();
  if (LLVM_UNLIKELY(!initExpr)) {
    return initExpr.takeError();
  }
  ast::ArrayExpr ae(std::move(typeId), std::move(sizeExpr.get()), std::move(initExpr.get()));
  return ast::Expr::create(std::move(ae), arena);
}

llvm::Expected<ast::ExprPtr> Parser::recordConstructor(ast::String &&typeId) {
  ast::FieldList fields(arena);
  while (!match(TokenKind::RightBrace)) {
    if (LLVM_UNLIKELY(!current)) {
      return propagateLexerError();
    }
    if (LLVM_UNLIKELY(fields.size() > 0 && !match(TokenKind::Comma))) {
      return makeParserError(MsgNo::MissingSeperator, TokenKind::Comma);
    }
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingRecFieldId, typeId.data());
    }
    ast::String fieldId(current->lexeme.data(), current->lexeme.size(), arena);
    if (LLVM_UNLIKELY(!match(TokenKind::Equals))) {
      return makeParserError(MsgNo::MissingTokenBetween, TokenKind::Equals, "identifier", "value");
    }
    auto expr = orExpression();
    if (LLVM_UNLIKELY(!expr)) {
      return expr.takeError();
    }
    fields.push_back(ast::Field(std::move(fieldId), std::move(expr.get())));
  }
  ast::RecordExpr re(std::move(fields));
  return ast::Expr::create(std::move(re), arena);
}

llvm::Expected<ast::ExprPtr> Parser::simpleVariable(ast::String &&id) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto var = ast::VariableExpr::create(std::move(id), arena);
  return ast::Expr::create(std::move(*var), arena); 
}

llvm::Expected<ast::ExprPtr> Parser::fieldVariable(ast::ExprPtr lvalue) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(lvalue->getKind() != ast::ExprKind::Variable)) {
    return makeParserError(MsgNo::CantCreateVarFrom, "field", lvalue->getKind());
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    gotoNextToken();
    return makeParserError(MsgNo::ExpIdInsteadOf, current->type);
  }
  auto fst = ast::VariableExpr::create(std::move(lvalue->takeVariable().get()), arena);
  ast::String id(current->lexeme.data(), current->lexeme.size(), arena);
  auto var = ast::VariableExpr::create(std::move(fst), std::move(id), arena);
  return ast::Expr::create(std::move(*var), arena); 
}

llvm::Expected<ast::ExprPtr> Parser::subscriptVariable(ast::ExprPtr lvalue) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(lvalue->getKind() != ast::ExprKind::Variable)) {
    return makeParserError(MsgNo::CantCreateVarFrom, "subscript", lvalue->getKind());
  }
  auto expr = orExpression();
  if (LLVM_UNLIKELY(!expr)) {
    return expr.takeError();
  } 
  if (LLVM_UNLIKELY(!match(TokenKind::RightBracket))) {
    return makeParserError(MsgNo::MissingCloser, TokenKind::RightBracket, "array index");
  }
  auto fst = ast::VariableExpr::create(std::move(lvalue->takeVariable().get()), arena);
  auto var = ast::VariableExpr::create(std::move(fst), std::move(expr.get()), arena);
  return ast::Expr::create(std::move(*var), arena); 
}

llvm::Expected<ast::ExprPtr> Parser::variable(ast::String &&id) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lvalue = simpleVariable(std::move(id));
  while (true) {
    if (LLVM_UNLIKELY(!lvalue)) {
      return lvalue.takeError();
    }
    if (match(TokenKind::Dot)) {
      lvalue = fieldVariable(std::move(lvalue.get()));
    } else if (match(TokenKind::LeftBracket)) {
      lvalue = subscriptVariable(std::move(lvalue.get()));
    } else {
      break;
    }
  }
  return lvalue;
}

llvm::Expected<ast::ExprPtr> Parser::expressionSequence(TokenKind closer) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  ast::ExprList exprs(arena);
  while (!match(closer)) {
    if (LLVM_UNLIKELY(!tokenStream.goOn())) {
      return makeParserError(MsgNo::MissingCloser, closer, "sequence expression");
    }
    if (LLVM_UNLIKELY(exprs.size() > 0 && !match(TokenKind::Semicolon))) {
      return makeParserError(MsgNo::MissingSeperator, TokenKind::Semicolon);
    }
    if (LLVM_UNLIKELY(match(closer))) {
      return makeParserError(MsgNo::EarlyCloser, "expression", TokenKind::Semicolon);
    }
    auto expr = orExpression();
    if (LLVM_UNLIKELY(!expr)) {
      return expr.takeError();
    }
    exprs.push_back(std::move(expr.get()));
  }
  ast::SequenceExpr se(std::move(exprs));
  return ast::Expr::create(std::move(se), arena);
}

llvm::Expected<ast::ExprPtr> Parser::ifExpression() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto condExpr = orExpression();
  if (LLVM_UNLIKELY(!condExpr)) {
    return condExpr.takeError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Then))) {
    return makeParserError(MsgNo::MissingKeywordAfter, "then", "condition", "if expression");
  }
  auto thenExpr = orExpression();
  if (LLVM_UNLIKELY(!thenExpr)) {
    return thenExpr.takeError();
  }
  if (match(TokenKind::Else)) {
    auto elseExpr = orExpression(); 
    ast::IfExpr ie(std::move(condExpr.get()), std::move(thenExpr.get()), std::move(elseExpr.get()));
    return ast::Expr::create(std::move(ie), arena);
  }
  ast::IfExpr ie(std::move(condExpr.get()), std::move(thenExpr.get()));
  return ast::Expr::create(std::move(ie), arena);
}

llvm::Expected<ast::ExprPtr> Parser::whileLoop() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto condExpr = orExpression();
  if (LLVM_UNLIKELY(!condExpr)) {
    return condExpr.takeError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Do))) {
    return makeParserError(MsgNo::MissingKeyword, "do", "while expression");
  }
  auto bodyExpr = orExpression();
  if (LLVM_UNLIKELY(!bodyExpr)) {
    return bodyExpr.takeError();
  }
  ast::WhileExpr we(std::move(condExpr.get()), std::move(bodyExpr.get()));
  return ast::Expr::create(std::move(we), arena);
}

llvm::Expected<ast::ExprPtr> Parser::forLoopVariable() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  ast::String id(current->lexeme.data(), current->lexeme.size(), arena);
  return simpleVariable(std::move(id));
}

llvm::Expected<ast::ExprPtr> Parser::forLoop() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    return makeParserError(MsgNo::MissingId, "loop variable", "for expression");
  }
  auto loopVarExpr = forLoopVariable();
  if (LLVM_UNLIKELY(!loopVarExpr)) {
    return loopVarExpr.takeError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Define))) {
    return makeParserError(MsgNo::MissingTokenAfter, TokenKind::Define, "for expression");
  }
  auto loopVarInitExpr = orExpression(); 
  if (LLVM_UNLIKELY(!match(TokenKind::To))) {
    return makeParserError(MsgNo::MissingKeyword, "to", "for expression");
  }
  auto loopVarBoundExpr = orExpression(); 
  if (LLVM_UNLIKELY(!match(TokenKind::Do))) {
    return makeParserError(MsgNo::MissingKeyword, "do", "for expression");
  }
  auto bodyExpr = orExpression();
  ast::ForExpr fe(
    std::move(loopVarExpr.get()), std::move(loopVarInitExpr.get()),
    std::move(loopVarBoundExpr.get()), std::move(bodyExpr.get()));
  return ast::Expr::create(std::move(fe), arena);
}

llvm::Expected<ast::ExprPtr> Parser::loopBreak() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  return ast::Expr::create(ast::BreakExpr(), arena);
}

llvm::Expected<ast::ExprPtr> Parser::assignment(ast::ExprPtr &&lvalue) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto expr = orExpression();
  if (LLVM_UNLIKELY(!expr)) {
    return expr.takeError();
  }
  ast::AssignExpr ae(std::move(lvalue), std::move(expr.get()));
  return ast::Expr::create(std::move(ae), arena);
}

llvm::Expected<ast::ExprPtr> Parser::orExpression() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lhs = andExpression();
  if (LLVM_UNLIKELY(!lhs)) {
    return lhs.takeError();
  }
  while (match(TokenKind::Or)) {
    auto rhs = andExpression();
    if (LLVM_UNLIKELY(!lhs)) {
      return lhs.takeError();
    }
    ast::OperatorExpr oe(
      std::move(lhs.get()), std::move(rhs.get()), ast::OperatorKind::Or);
    lhs = ast::Expr::create(std::move(oe), arena);
  }
  return lhs;
}

llvm::Expected<ast::ExprPtr> Parser::andExpression() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lhs = comparison();
  if (LLVM_UNLIKELY(!lhs)) {
    return lhs.takeError();
  }
  while (match(TokenKind::And)) {
    auto rhs = comparison();
    if (LLVM_UNLIKELY(!lhs)) {
      return lhs.takeError();
    }
    ast::OperatorExpr ae(
      std::move(lhs.get()), std::move(rhs.get()), ast::OperatorKind::And);
    lhs = ast::Expr::create(std::move(ae), arena);
  }
  return lhs;
}

std::optional<ast::OperatorKind> Parser::comparisonOperator() {
  if (match(TokenKind::Less)) {
    return ast::OperatorKind::LessThan;
  } else if (match(TokenKind::LessEquals)) {
    return ast::OperatorKind::LessEqual;
  } else if (match(TokenKind::Greater)) {
    return ast::OperatorKind::GreaterThan;
  } else if (match(TokenKind::GreaterEquals)) {
    return ast::OperatorKind::GreaterEqual;
  } else if (match(TokenKind::Equals)) {
    return ast::OperatorKind::Equals;
  } else if (match(TokenKind::NotEquals)) {
    return ast::OperatorKind::Unequal;
  }
  return std::nullopt;
}

llvm::Expected<ast::ExprPtr> Parser::comparison() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lhs = term();
  if (LLVM_UNLIKELY(!lhs)) {
    return lhs.takeError();
  }
  while (auto co = comparisonOperator()) {
    auto rhs = term();
    if (LLVM_UNLIKELY(!rhs)) {
      return rhs.takeError();
    }
    ast::OperatorExpr ce(std::move(lhs.get()), std::move(rhs.get()), *co);
    lhs = ast::Expr::create(std::move(ce), arena);
  }
  return lhs;
}

std::optional<ast::OperatorKind> Parser::additionOperator() {
  if (match(TokenKind::Plus)) {
    return ast::OperatorKind::Plus;
  } else if (match(TokenKind::Minus)) {
    return ast::OperatorKind::Minus;
  }
  return std::nullopt;
}

llvm::Expected<ast::ExprPtr> Parser::term() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lhs = factor();
  if (LLVM_UNLIKELY(!lhs)) {
    return lhs.takeError();
  }
  while (auto co = additionOperator()) {
    auto rhs = factor();
    if (LLVM_UNLIKELY(!lhs)) {
      return lhs.takeError();
    }
    ast::OperatorExpr ce(std::move(lhs.get()), std::move(rhs.get()), *co);
    lhs = ast::Expr::create(std::move(ce), arena);
  }
  return lhs;
}

std::optional<ast::OperatorKind> Parser::multiplicationOperator() {
  if (match(TokenKind::Star)) {
    return ast::OperatorKind::Times;
  } else if (match(TokenKind::Slash)) {
    return ast::OperatorKind::Divide;
  }
  return std::nullopt;
}

llvm::Expected<ast::ExprPtr> Parser::factor() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  auto lhs = expression();
  if (LLVM_UNLIKELY(!lhs)) {
    return lhs.takeError();
  }
  while (auto co = multiplicationOperator()) {
    auto rhs = expression();
    if (LLVM_UNLIKELY(!lhs)) {
      return lhs.takeError();
    }
    ast::OperatorExpr ce(std::move(lhs.get()), std::move(rhs.get()), *co);
    lhs = ast::Expr::create(std::move(ce), arena);
  }
  return lhs;
}

llvm::Expected<ast::ExprPtr> Parser::afterTypeIdentifier() {
  ast::String id(current->lexeme.data(), current->lexeme.size(), arena);
  if (match(TokenKind::LeftBracket)) {
    return arrayConstructor(std::move(id));
  } else if (match(TokenKind::LeftBrace)) {
    return recordConstructor(std::move(id));
  }
  gotoNextToken();
  return makeParserError(MsgNo::InvalidTokenAfter, current->type, "type identifier");
}

llvm::Expected<ast::ExprPtr> Parser::afterIdentifier() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  SymbolInfo sy = symbolTable->lookup(current->lexeme);
  if (sy.kind == SymbolKind::Type) {
    return afterTypeIdentifier();
  }
  ast::String id(current->lexeme.data(), current->lexeme.size(), arena);
  if (match(TokenKind::LeftParen)) {
    if (sy.kind == SymbolKind::Unknown) {
      deferredDeclarations->insert(std::make_pair(id.data(), SymbolKind::Function));
    }
    return functionCall(std::move(id));
  } else {
    auto lvalue = variable(std::move(id));
    if (LLVM_UNLIKELY(!lvalue)) {
      return lvalue.takeError();
    }
    if (match(TokenKind::Define)) {
      return assignment(std::move(lvalue.get()));
    }
    return lvalue;
  }
  gotoNextToken();
  return makeParserError(MsgNo::InvalidTokenAfter, current->type, "identifier");
}

llvm::Expected<ast::Decl> Parser::variableDeclaration() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    return makeParserError(MsgNo::MissingIdAfter, "variable", "'var' keyword", "variable declaration");
  }
  ast::String identifier(current->lexeme.data(), current->lexeme.size(), arena);
  ast::String type(arena);
  if (match(TokenKind::Colon)) {
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingId, "type", "variable declaration");
    }
    type = ast::String(current->lexeme.data(), current->lexeme.size(), arena);
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Define))) {
    return makeParserError(MsgNo::MissingToken, TokenKind::Define, "variable declaration");
  }
  auto expr = orExpression();
  if (LLVM_UNLIKELY(!expr)) {
    return expr.takeError();
  }
  ast::VarDecl vd(std::move(identifier), std::move(type), std::move(expr.get()));
  auto sy = symbolTable->declareSymbol(vd);
  if (LLVM_UNLIKELY(!sy)) {
    return sy.takeError();
  }
  return ast::Decl(std::move(vd));
}

llvm::Expected<ast::Decl> Parser::aliasTypeDeclaration(ast::String &&identifier) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!symbolTable->containsAs(current->lexeme, SymbolKind::Type))) {
    return makeParserError(MsgNo::SymbNoType, current->lexeme);
  }
  // note that we've already matched an identifier to get in here.
  ast::String alias(current->lexeme.data(), current->lexeme.size(), arena);
  ast::RenamedTypeDecl rtd(std::move(alias));
  ast::TypeDecl td(std::move(identifier), std::move(rtd));
  auto sy = symbolTable->declareSymbol(td);
  if (LLVM_UNLIKELY(!sy)) {
    return sy.takeError();
  }
  return ast::Decl(std::move(td));
}

llvm::Expected<ast::Decl> Parser::arrayTypeDeclaration(ast::String &&identifier) {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Of))) {
    return makeParserError(MsgNo::MissingKeywordAfter, "of", "keyword 'array'", "array type declaration");;
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    return makeParserError(MsgNo::MissingIdAfter, "type", "'of' keyword", "array type declaration");
  }
  if (LLVM_UNLIKELY(!symbolTable->containsAs(current->lexeme, SymbolKind::Type))) {
    return makeParserError(MsgNo::SymbNoType, current->lexeme);
  }
  ast::String typeIdentifer(current->lexeme.data(), current->lexeme.size(), arena);
  ast::ArrayTypeDecl atd(std::move(typeIdentifer));
  ast::TypeDecl td(std::move(identifier), std::move(atd));
  auto sy = symbolTable->declareSymbol(td); 
  if (LLVM_UNLIKELY(!sy)) {
    return sy.takeError();
  }
  return ast::Decl(std::move(td));
}

llvm::Expected<ast::Decl> Parser::recordTypeDeclaration(ast::String &&identifier) {
  ast::ParamList fields(arena);
  // To avoid duplicate field declarations a new "scope" is opened here 
  // and instantly closed after the field list. 
  pushScope(ScopeKind::Other);
  while (!match(TokenKind::RightBrace)) {
    if (LLVM_UNLIKELY(!current)) {
      return propagateLexerError();
    }
    if (LLVM_UNLIKELY(fields.size() > 0 && !match(TokenKind::Comma))) {
      return makeParserError(MsgNo::MissingToken, TokenKind::Comma, "between record field declarations");
    }
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingId, "field", "record field declaration");
    }
    ast::String fieldId(current->lexeme.data(), current->lexeme.size(), arena);
    if (LLVM_UNLIKELY(!match(TokenKind::Colon))) {
      return makeParserError(MsgNo::MissingTokenBetween, TokenKind::Colon, "field name", "type");
    }
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingId, "type", "record field");
    }
    if (LLVM_UNLIKELY(!symbolTable->containsAs(current->lexeme, SymbolKind::Type))) {
      return makeParserError(MsgNo::SymbNoType, current->lexeme);
    }
    ast::String fieldType(current->lexeme.data(), current->lexeme.size(), arena);
    ast::ParamDecl pd(std::move(fieldId), std::move(fieldType));
    auto sy = symbolTable->declareSymbol(pd);
    if (LLVM_UNLIKELY(!sy)) {
      return sy.takeError();
    }
    fields.push_back(std::move(pd));
  }
  popScope(ScopeKind::Other);
  ast::RecordTypeDecl rtd(std::move(fields));
  ast::TypeDecl td(std::move(identifier), std::move(rtd));
  auto sy = symbolTable->declareSymbol(td);
  if (LLVM_UNLIKELY(!sy)) {
    return sy.takeError();
  }
  return ast::Decl(std::move(td));
}

llvm::Expected<ast::Decl> Parser::typeDeclaration() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    return makeParserError(MsgNo::MissingIdAfter, "'type' keyword", "type declaration");
  }
  ast::String identifier(current->lexeme.data(), current->lexeme.size(), arena);
  if (LLVM_UNLIKELY(!match(TokenKind::Equals))) {
    return makeParserError(MsgNo::MissingToken, TokenKind::Equals, "type declaration");
  }
  if (match(TokenKind::Identifier)) {
    return aliasTypeDeclaration(std::move(identifier));
  } else if (match(TokenKind::LeftBrace)) {
    return recordTypeDeclaration(std::move(identifier));
  } else if (match(TokenKind::Array)) {
    return arrayTypeDeclaration(std::move(identifier));
  }
  return makeParserError(MsgNo::InvalidTokenBecause, current->type, "Token is not a type constructor");
}

llvm::Expected<ast::ParamList> Parser::functionParams() {
  ast::ParamList params(arena);
  while (!match(TokenKind::RightParen)) {
    if (LLVM_UNLIKELY(!current)) {
      return propagateLexerError();
    }
    if (LLVM_UNLIKELY(params.size() > 0 && !match(TokenKind::Comma))) {
      return makeParserError(MsgNo::MissingToken, TokenKind::Comma, "between function parameter declarations");
    }
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingId, "parameter", "function parameter declaration");
    }
    ast::String paramId(current->lexeme.data(), current->lexeme.size(), arena);
    if (LLVM_UNLIKELY(!match(TokenKind::Colon))) {
      return makeParserError(MsgNo::MissingTokenBetween, TokenKind::Colon, "function parameter name", "type");
    }
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingIdAfter, "type", TokenKind::Colon, "function parameter declaration");
    }
    if (LLVM_UNLIKELY(!symbolTable->containsAs(current->lexeme, SymbolKind::Type))) {
      return makeParserError(MsgNo::SymbNoType, current->lexeme);
    }
    ast::String paramType(current->lexeme.data(), current->lexeme.size(), arena);
    ast::ParamDecl pd(std::move(paramId), std::move(paramType));
    auto sy = symbolTable->declareSymbol(pd);
    if (LLVM_UNLIKELY(!sy)) {
      return sy.takeError();
    }
    params.push_back(std::move(pd));
  }
  return params;
}

llvm::Expected<ast::Decl> Parser::functionDeclaration() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
    return makeParserError(MsgNo::MissingIdAfter, "function", "'function' keyword", "function declaration");
  }
  pushScope(ScopeKind::FuncScope);
  ast::String identifier(current->lexeme.data(), current->lexeme.size(), arena);
  if (LLVM_UNLIKELY(!match(TokenKind::LeftParen))) {
    return makeParserError(MsgNo::MissingTokenAfter, TokenKind::LeftParen, "function name", "function declaration");
  }
  auto params = functionParams();
  if (LLVM_UNLIKELY(!params)) {
    return params.takeError();
  }
  ast::String returnType(arena); 
  if (match(TokenKind::Colon)) {
    if (LLVM_UNLIKELY(!match(TokenKind::Identifier))) {
      return makeParserError(MsgNo::MissingId, "return type", "function declaration");
    }
    if (LLVM_UNLIKELY(!symbolTable->containsAs(current->lexeme, SymbolKind::Type))) {
      return makeParserError(MsgNo::SymbNoType, current->lexeme);
    }
    returnType = ast::String(current->lexeme.data(), current->lexeme.size(), arena);
  }
  if (LLVM_UNLIKELY(!match(TokenKind::Equals))) {
    return makeParserError(MsgNo::MissingToken, TokenKind::Equals, "function declaration");
  }
  auto expr = orExpression();
  if (LLVM_UNLIKELY(!expr)) {
    return expr.takeError();
  }
  popScope(ScopeKind::FuncScope);
  ast::FunctionDecl fd(
    std::move(identifier), std::move(params.get()),
    std::move(returnType), std::move(expr.get()));
  auto sy = symbolTable->declareSymbol(fd);
  if (LLVM_UNLIKELY(!sy)) {
    return sy.takeError();
  }
  return ast::Decl(std::move(fd));
}

llvm::Expected<ast::DeclList> Parser::declarations() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  ast::DeclList decls(arena);
  while (!match(TokenKind::In)) {
    if (!tokenStream.goOn()) {
      return makeParserError(MsgNo::MissingCloser, TokenKind::In, "declaration list in let-expr");
    }
    if (match(TokenKind::Var)) {
      auto decl = variableDeclaration();
      if (LLVM_UNLIKELY(!decl)) {
        return decl.takeError();
      }
      decls.push_back(std::move(decl.get()));
    } else if (match(TokenKind::Type)) {
      auto decl = typeDeclaration();
      if (LLVM_UNLIKELY(!decl)) {
        return decl.takeError();
      }
      decls.push_back(std::move(decl.get()));
    } else if (match(TokenKind::Function)) {
      auto decl = functionDeclaration();
      if (LLVM_UNLIKELY(!decl)) {
        return decl.takeError();
      }
      decls.push_back(std::move(decl.get()));
    }
  }
  return decls;
}

llvm::Error Parser::checkDeferredDeclarations() {
  for (auto it = deferredDeclarations->begin(), E = deferredDeclarations->end(); it != E; ++it) {
    if (!symbolTable->containsAs(it->getKey(), it->getValue().kind)) {
      return makeParserError(MsgNo::SymbUnknown, it->getKey());
    }
  }
  return llvm::Error::success();
}

llvm::Expected<ast::ExprPtr> Parser::letExpression() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  pushScope(ScopeKind::LetScope);
  auto decls = declarations();
  if (LLVM_UNLIKELY(!decls)) {
    return decls.takeError();
  }
  if (auto err = checkDeferredDeclarations()) {
    return err;
  }
  auto exprs = expressionSequence(TokenKind::End);
  if (LLVM_UNLIKELY(!exprs)) {
    return exprs.takeError();
  }
  popScope(ScopeKind::LetScope);
  ast::LetExpr le(std::move(decls.get()), std::move(exprs.get()));
  return ast::Expr::create(std::move(le), arena);
}

llvm::Expected<ast::ExprPtr> Parser::expression() {
  if (LLVM_UNLIKELY(!current)) {
    return propagateLexerError();
  }
  if (match(TokenKind::StringLiteral)) {
    return strLit();
  } else if (match(TokenKind::IntegerLiteral)) {
    return intLit();
  } else if (match(TokenKind::Nil)) {
    return nil();
  } else if (match(TokenKind::Minus)) {
    return negation();
  } else if (match(TokenKind::Identifier)) {
    return afterIdentifier();
  } else if (match(TokenKind::LeftParen)) {
    return expressionSequence(TokenKind::RightParen);
  } else if (match(TokenKind::If)) {
    return ifExpression();
  } else if (match(TokenKind::While)) {
    return whileLoop();
  } else if (match(TokenKind::For)) {
    return forLoop();
  } else if (match(TokenKind::Break)) {
    return loopBreak();
  } else if (match(TokenKind::Let)) {
    return letExpression();
  } else if (match(TokenKind::Empty)) {
    return makeParserError(MsgNo::EmptyToken); // TODO....
  }
  return orExpression();
}

} // namespace
