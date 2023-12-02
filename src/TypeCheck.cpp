#include "TypeCheck.h"
#include "ErrorMessage.h"

namespace tig {

class TypeCheckExprVisitor : public ast::ConstExprVisitor {
public:
  virtual void operator()(const ast::NilExpr &);
  // virtual void operator()(const ast::VariableExpr &);
  virtual void operator()(const ast::IntLitExpr &);
  virtual void operator()(const ast::StrLitExpr &);
  // virtual void operator()(const ast::CallExpr &);
  virtual void operator()(const ast::OperatorExpr &);
  // virtual void operator()(const ast::RecordExpr &);
  virtual void operator()(const ast::SequenceExpr &);
  // virtual void operator()(const ast::AssignExpr &);
  // virtual void operator()(const ast::IfExpr &);
  // virtual void operator()(const ast::WhileExpr &);
  // virtual void operator()(const ast::ForExpr &);
  // virtual void operator()(const ast::BreakExpr &);
  virtual void operator()(const ast::LetExpr &);
  // virtual void operator()(const ast::ArrayExpr &);

  explicit TypeCheckExprVisitor(std::shared_ptr<TypeChecker> typeChecker):
    typeChecker(typeChecker),
    type(InfereType)
  {}

  TypeCheckExprVisitor(const TypeCheckExprVisitor &other):
    typeChecker(other.typeChecker),
    type(other.type)
  {}

  TypeCheckExprVisitor &operator=(const TypeCheckExprVisitor &other) {
    if (this != &other) {
      this->typeChecker = other.typeChecker;
      this->type = other.type;
    }
    return *this;
  }

  TypeIndex getType() const {
    return type;
  }

private:
  std::shared_ptr<TypeChecker> typeChecker;
  TypeIndex type;

  bool expectTruthyType(TypeIndex type);
  bool expectIntType(TypeIndex type);
};

bool TypeCheckExprVisitor::expectTruthyType(TypeIndex type) {
  if (!typeChecker->isTruthy(type)) {
    typeChecker->addError(emitStringError(MsgNo::NotTruthy, type));
    return false;
  }
  return true;
}

bool TypeCheckExprVisitor::expectIntType(TypeIndex type) {
  if (!typeChecker->resolvesTo(type, typeChecker->getInt())) {
    typeChecker->addError(emitStringError(MsgNo::NotIntegral, type));
    return false;
  }
  return true;
}

void TypeCheckExprVisitor::operator()(const ast::NilExpr &) {
  type = typeChecker->getSymbolTable().lookup("nil").typeIdx;
}

void TypeCheckExprVisitor::operator()(const ast::IntLitExpr &) {
  type = typeChecker->getSymbolTable().lookup("int").typeIdx;
}

void TypeCheckExprVisitor::operator()(const ast::StrLitExpr &) {
  type = typeChecker->getSymbolTable().lookup("string").typeIdx;
}

void TypeCheckExprVisitor::operator()(const ast::OperatorExpr &expr) {
  if (typeChecker->hasError()) {
    return;
  }
  TypeCheckExprVisitor leftVis(*this);
  TypeCheckExprVisitor rightVis(*this);
  expr.leftOperand().visit(leftVis);
  expr.rightOperand().visit(rightVis);

  bool success = true; 
  switch (expr.opKind()) {
    case ast::OperatorKind::And:
    case ast::OperatorKind::Or:
      success &= expectTruthyType(leftVis.type);
      success &= expectTruthyType(rightVis.type);
      break;
    case ast::OperatorKind::Plus:
    case ast::OperatorKind::Times:
    case ast::OperatorKind::Minus:
    case ast::OperatorKind::Divide:
      success &= expectIntType(leftVis.type);
      success &= expectIntType(rightVis.type);
      break;
    case ast::OperatorKind::GreaterEqual:
    case ast::OperatorKind::GreaterThan:
    case ast::OperatorKind::LessEqual:
    case ast::OperatorKind::LessThan:
    case ast::OperatorKind::Equals:
    case ast::OperatorKind::Unequal:
      success &= typeChecker->equivalent(leftVis.type, rightVis.type);
    default:
      break;
  }
  if (!success) {
    return;
  }
  if (leftVis.type != rightVis.type) {
    typeChecker->addError(emitStringError(MsgNo::TyMismatchBinOp, leftVis.type, rightVis.type));
    return;
  }
  type = leftVis.type;
}

void TypeCheckExprVisitor::operator()(const ast::SequenceExpr &expr) {
  for (auto it = expr.cbegin(), E = expr.cend(); it != E; ++it) {
    if (typeChecker->hasError()) {
      return;
    }
    (*it)->visit(*this);
  }
}

void TypeCheckExprVisitor::operator()(const ast::LetExpr &expr) {
  typeChecker->pushScope();
  for (auto it = expr.cbegin(), E = expr.cend(); it != E; ++it) {
    auto kind = it->getKind();
    if (kind == ast::DeclKind::Type) {
      const ast::TypeDecl &decl = it->toTypeDecl().get();
      const llvm::StringRef name = decl.typeName();
      switch (decl.getKind()) {
        case ast::TypeDeclKind::Array: {
          auto ty = typeChecker->addArrayTypeDeclaration(decl.toArrayDecl().get());
          if (!ty) {
            typeChecker->addError(ty.takeError());
            return;
          }
          typeChecker->getSymbolTable().insert(std::make_pair(name, SymbolInfo(SymbolKind::Type, *ty)));
          break;
        }
        case ast::TypeDeclKind::Record:
          typeChecker->addRecordTypeDeclaration(decl.toRecordDecl().get()); 
          break;
        case ast::TypeDeclKind::Rename:
          typeChecker->addRenamedTypeDeclaration(decl.toRenameDecl().get());
          break;
      }
    } else if (kind == ast::DeclKind::Function) {
      typeChecker->addFunctionDeclaration(it->toFunctionDecl().get());
    }
  }
  expr.innerExprs().visit(*this);
  typeChecker->popScope();
}

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

TypeChecker::TypeChecker():
  symbolTable(makeTopLevelScope()),  // ensures that symbols 'int' and 'string' exist in table
  error(llvm::Error::success())
{
  typeTable.push_back(TTPrimitiveType {TTPrimitiveType::Kind::Int});
  intTypeIndex = typeTable.size() - 1;
  symbolTable->find("int")->getValue().typeIdx = intTypeIndex;

  typeTable.push_back(TTPrimitiveType {TTPrimitiveType::Kind::String});
  stringTypeIndex = typeTable.size() - 1;
  symbolTable->find("string")->getValue().typeIdx = stringTypeIndex;

  // nil is not really a type but it is a reserved keyword and it can be 
  // used as fake type symbol in the type checker just for the nil expression.
  typeTable.push_back(TTPrimitiveType {TTPrimitiveType::Kind::Nil});
  nilTypeIndex = typeTable.size() - 1;
  symbolTable->find("nil")->getValue().typeIdx = nilTypeIndex;
}

llvm::Error TypeChecker::addTypeDeclaration(const ast::TypeDecl &decl) {
  llvm::StringRef name = decl.typeName();
  switch (decl.getKind()) {
    case ast::TypeDeclKind::Array: {
      auto idx = addArrayTypeDeclaration(decl.toArrayDecl().get());
      if (!idx) {
        return idx.takeError();
      }
      symbolTable->insert(std::make_pair(name, SymbolInfo(SymbolKind::Type, *idx)));
      break;
    }
    case ast::TypeDeclKind::Record: {
      auto idx = addRecordTypeDeclaration(decl.toRecordDecl().get());
      if (!idx) {
        return idx.takeError();
      }
      symbolTable->insert(std::make_pair(name, SymbolInfo(SymbolKind::Type, *idx)));
      break;
    }
    case ast::TypeDeclKind::Rename: {
      auto idx = addRenamedTypeDeclaration(decl.toRenameDecl().get());
      if (!idx) {
        return idx.takeError();
      }
      symbolTable->insert(std::make_pair(name, SymbolInfo(SymbolKind::Type, *idx)));
      break;
    }
  }
  return llvm::Error::success();
}

llvm::Expected<TypeIndex> TypeChecker::addRenamedTypeDeclaration(const ast::RenamedTypeDecl &decl) {
  llvm::StringRef originalType = decl.getOriginalType(); 
  auto syinfo = symbolTable->lookup(originalType);
  if (syinfo.kind == SymbolKind::Unknown) {
    return emitStringError(MsgNo::SymbUnknown, originalType);
  } else if (syinfo.kind != SymbolKind::Type) {
    return emitStringError(MsgNo::SymbNoType, originalType);
  }
  TTScalarType ty {syinfo.typeIdx};
  typeTable.push_back(ty);
  return typeTable.size() - 1;
}

llvm::Expected<TypeIndex> TypeChecker::addArrayTypeDeclaration(const ast::ArrayTypeDecl &decl) {
  llvm::StringRef baseType = decl.getElementType();
  auto syinfo = symbolTable->lookup(baseType);
  if (syinfo.kind == SymbolKind::Unknown) {
    return emitStringError(MsgNo::SymbUnknown, baseType);
  } else if (syinfo.kind != SymbolKind::Type) {
    return emitStringError(MsgNo::SymbNoType, baseType);
  }
  TTArray ty {syinfo.typeIdx};
  typeTable.push_back(ty);
  return typeTable.size() - 1;
}

llvm::Expected<TypeIndex> TypeChecker::addRecordFieldTypeDeclaration(const ast::ParamDecl &decl) {
  llvm::StringRef baseType = decl.paramType();
  auto syinfo = symbolTable->lookup(baseType);
  if (syinfo.kind == SymbolKind::Unknown) {
    return emitStringError(MsgNo::SymbUnknown, baseType);
  } else if (syinfo.kind != SymbolKind::Type) {
    return emitStringError(MsgNo::SymbNoType, baseType);
  }
  TTRecordField ty {syinfo.typeIdx};
  typeTable.push_back(ty); 
  return typeTable.size() - 1;
}

llvm::Expected<TypeIndex> TypeChecker::addRecordTypeDeclaration(const ast::RecordTypeDecl &decl) {
  TypeIndex idx = typeTable.size();
  for (auto it = decl.cbegin(), E = decl.cend(); it != E; ++it) {
    auto res = addRecordFieldTypeDeclaration(*it);
    if (!res) {
      return res.takeError();
    }
  }
  TTRecordFieldsEnd guard;
  typeTable.push_back(guard);
  return idx;
}

llvm::Error TypeChecker::addFunctionDeclaration(const ast::FunctionDecl &decl) {
  TypeIndex start = typeTable.size();
  for (auto it = decl.cbegin(), E = decl.cend(); it != E; ++it) {
    auto res = addFunctionParamDeclaration(*it);
    if (!res) {
      return res.takeError();
    }
  }
  TypeIndex typeIdx = InfereType;
  if (decl.hasReturnType()) {
    llvm::StringRef returnType = decl.functionReturnType();
    auto syinfo = symbolTable->lookup(returnType);
    if (syinfo.kind == SymbolKind::Unknown) {
      return emitStringError(MsgNo::SymbUnknown, returnType);
    } else if (syinfo.kind != SymbolKind::Type) {
      return emitStringError(MsgNo::SymbNoType, returnType);
    }
    typeIdx = syinfo.typeIdx;
  }
  TTFunctionReturnType ty {typeIdx};
  typeTable.push_back(ty);
  symbolTable->insert(std::make_pair(decl.functionName(), SymbolInfo(SymbolKind::Function, start)));
  return llvm::Error::success();
}

llvm::Expected<TypeIndex> TypeChecker::addFunctionParamDeclaration(const ast::ParamDecl &decl) {
  llvm::StringRef baseType = decl.paramType();
  auto syinfo = symbolTable->lookup(baseType);
  if (syinfo.kind == SymbolKind::Unknown) {
    return emitStringError(MsgNo::SymbUnknown, baseType);
  } else if (syinfo.kind != SymbolKind::Type) {
    return emitStringError(MsgNo::SymbNoType, baseType);
  }
  TTFunctionParam ty {syinfo.typeIdx};
  typeTable.push_back(ty); 
  return typeTable.size() - 1;
}

llvm::Error TypeChecker::addVariableDeclaration(const ast::VarDecl &decl) {
  return llvm::Error::success();
}

bool TypeChecker::resolvesTo(TypeIndex type, TypeIndex baseType) const {
  if (type == baseType) {
    return true;
  }
  const TTScalarType *scalarType = std::get_if<TTScalarType>(&typeTable.at(type));
  if (scalarType) {
    TypeIndex org = scalarType->originalType;
    if (org == baseType) {
      return true;
    } else {
      return resolvesTo(org, baseType);
    }
  }
  return false;
}

bool TypeChecker::isTruthy(TypeIndex type) const {
  return resolvesTo(type, intTypeIndex);
}

llvm::Error TypeChecker::check(const ast::Expr &expr) {
  TypeCheckExprVisitor visitor(getSharedPtr());
  expr.visit(visitor);
  if (hasError()) {
    return std::move(error);
  }
  return llvm::Error::success();
}

}