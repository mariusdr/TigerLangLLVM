#include "gtest/gtest.h"
#include "Parser.h"
#include "Ast.h"

using namespace tig;

class ParserTest : public ::testing::Test {
protected:
  ArenaScopeGuard ag;
};

/// @name declaration parser
/// @{

TEST_F(ParserTest, EmptyString) {
  const char *src = R"(
    let 
    in 
      ""
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));

  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::StrLitExpr &se = llvm::cantFail(le.innerExprs().toSequence()->at(0).toStrLit());
  EXPECT_EQ(se.getValue().size(), 0);
}

TEST_F(ParserTest, EmptyDeclarationList) {
  const char *src = R"(
    let 
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  EXPECT_EQ(le.size(), 0);
}

TEST_F(ParserTest, ParseLiterals) {
  const char *src = R"(
    let 
    in 
      /** decimals **/
      0; 00; 000; 0000; 00000;
      0000; 000000000000000;
      1; 2; 3231201921283028320;

      /** octals **/
      0232; 0122; 03232; 0121267;

      /** hex **/
      0x123; 0xdeadbeef;

      /** strings **/
      "foo"; "239283921"; "0xdeadbeef"
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
}

TEST_F(ParserTest, RenamedTypeDeclaration) {
  const char *src = R"(
    let 
      type myInt = int
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findTypeDecl("myInt");
  EXPECT_NE(it, le.cend()); 
  EXPECT_EQ(it->toTypeDecl()->getKind(), ast::TypeDeclKind::Rename);
  EXPECT_EQ(unwrapRenamedTypeDecl(*it).getOriginalType(), "int");
}

TEST_F(ParserTest, RecordTypeDeclaration) {
  const char *src = R"(
    let 
      type customType = int
      type myStruct = {
        x : int,
        y : string,
        z : customType
      }
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findTypeDecl("myStruct");
  EXPECT_NE(it, le.cend()); 
  const ast::RecordTypeDecl &td = ast::unwrapRecordTypeDecl(*it);
  EXPECT_NE(td.findField("x"), td.cend());
  EXPECT_NE(td.findField("y"), td.cend());
  EXPECT_NE(td.findField("z"), td.cend());
  EXPECT_EQ(td.findField("x")->paramType(), "int");
  EXPECT_EQ(td.findField("y")->paramType(), "string");
  EXPECT_EQ(td.findField("z")->paramType(), "customType");
}

TEST_F(ParserTest, EmptyRecordTypeDeclaration) {
  const char *src = R"(
    let 
      type myStruct = {}
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findTypeDecl("myStruct");
  EXPECT_NE(it, le.cend()); 
  const ast::RecordTypeDecl &td = ast::unwrapRecordTypeDecl(*it);
  EXPECT_EQ(td.size(), 0);
}

TEST_F(ParserTest, ArrayTypeDeclaration) {
  const char *src = R"(
    let 
      type ints = array of int
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findTypeDecl("ints");
  EXPECT_NE(it, le.cend()); 
  const ast::ArrayTypeDecl &td = ast::unwrapArrayTypeDecl(*it);
  EXPECT_EQ(td.getElementType(), "int");
}

TEST_F(ParserTest, MultipleTypeDeclarations) {
  const char *src = R"(
    let 
      type myInt = int
      type moreInt = myInt
      type myStruct = {
        foo: moreInt,
        bar: string
      }
      type myStructs = array of myStruct
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  EXPECT_NE(le.findDecl("myStructs"), le.cend());
  EXPECT_NE(le.findDecl("moreInt"), le.cend());
  EXPECT_NE(le.findDecl("myStruct"), le.cend());
  EXPECT_NE(le.findDecl("myInt"), le.cend());
}

TEST_F(ParserTest, VariableDeclarationWithoutType) {
  const char *src = R"(
    let 
      var x := 123
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findVarDecl("x");
  EXPECT_NE(it, le.cend());
  const ast::VarDecl &vd = ast::unwrapVarDecl(*it);
  EXPECT_EQ(vd.varType(), "");
}

TEST_F(ParserTest, VariableDeclarationWithType) {
  const char *src = R"(
    let 
      var x: myIntType := 123
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findVarDecl("x");
  EXPECT_NE(it, le.cend());
  const ast::VarDecl &vd = ast::unwrapVarDecl(*it);
  EXPECT_EQ(vd.varType(), "myIntType");
}

TEST_F(ParserTest, FunctionDeclarationWithoutTypeWithoutParams) {
  const char *src = R"(
    let 
      function myFunc() = nil
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findFunctionDecl("myFunc");
  EXPECT_NE(it, le.cend());
  const ast::FunctionDecl &fd = ast::unwrapFunctionDecl(*it);
  EXPECT_EQ(fd.functionBody().getKind(), ast::ExprKind::Nil);
}

TEST_F(ParserTest, FunctionDeclarationWithoutTypeWithParams) {
  const char *src = R"(
    let 
      function myFunc(a: int, x: int, y: int) =
        a * x + y
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findFunctionDecl("myFunc");
  EXPECT_NE(it, le.cend());
}

TEST_F(ParserTest, FunctionDeclarationWithTypeWithoutParams) {
  const char *src = R"(
    let 
      type customType = int
      function myFunc(): customType = nil
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findFunctionDecl("myFunc");
  EXPECT_NE(it, le.cend());
  const ast::FunctionDecl &fd = ast::unwrapFunctionDecl(*it);
  EXPECT_EQ(fd.functionBody().getKind(), ast::ExprKind::Nil);
  EXPECT_EQ(fd.functionReturnType(), "customType");
}

TEST_F(ParserTest, FunctionDeclarationWithTypeWithParams) {
  const char *src = R"(
    let 
      function myFunc(a: int, x: int, y: int): int =
        a * x + y
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  auto it = le.findFunctionDecl("myFunc");
  EXPECT_NE(it, le.cend());
  const ast::FunctionDecl &fd = ast::unwrapFunctionDecl(*it);
  EXPECT_EQ(fd.functionReturnType(), "int");
}


/// @}
/// @name validate scoping and shadowing rules
/// @{

TEST_F(ParserTest, DuplicateVariableDeclarationIsAnError) {
  const char *src = R"(
    let 
      var x := 123
      var y : int := 9923
      var z := 888
      var x : int := 721821
    in 
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_FALSE(bool(ast));
}

TEST_F(ParserTest, VariableShadowing) {
  const char *src = R"(
    let 
      var x := 123 
      var y := let 
        var x := 883823             /* redeclaration of x */
      in 
        x
      end
    in 
      let 
        var x := "hello world"     /* redeclaration of x */
      in 
        x
      end
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
}

TEST_F(ParserTest, MutuallyRecursiveFunctions) {
  const char *src = R"(
    let 
      function foo(x: int) = if x > 0 then bar(x - 1) else 0

      var x := let 
        function top(x: int) = bottom(-x)
        function middle(x: int) = top(-x) + bottom(x)
        function bottom(y: int) = top(-x)
      in
        let function foo(str: string) = bar("hi")   /* shadow top-level scope */
            function bar(str: string) = foo(str)
        in 
        end
      end

      function bar(y: int) = y + foo(y - 1)
    in 
      foo(123)
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
}

/// @}
/// @name test expression parser 
/// @{

TEST_F(ParserTest, NilExpression) {
  const char *src = R"(
    let 
    in 
      nil
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  EXPECT_EQ(se.at(0).getKind(), ast::ExprKind::Nil);
}

TEST_F(ParserTest, LiteralExpressions) {
  const char *src = R"(
    let 
    in 
      1323232;
      "foooobar"
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  EXPECT_EQ(se.at(0).getKind(), ast::ExprKind::IntLit);
  EXPECT_EQ(se.at(0).toIntLit()->getValue(), 1323232);
  EXPECT_EQ(se.at(1).getKind(), ast::ExprKind::StrLit);
  EXPECT_EQ(se.at(1).toStrLit()->getValue(), "foooobar");
}

TEST_F(ParserTest, UnaryExpression) {
  const char *src = R"(
    let 
    in 
      -1323232
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  EXPECT_EQ(se.at(0).getKind(), ast::ExprKind::Operator);
  ast::OperatorExpr &oe = se.at(0).toOperator().get(); 
  EXPECT_EQ(oe.opKind(), ast::OperatorKind::Minus);
  EXPECT_EQ(oe.leftOperand().toIntLit().get().getValue(), 0);
  EXPECT_EQ(oe.rightOperand().toIntLit().get().getValue(), 1323232);
}

TEST_F(ParserTest, BinaryExpression) {
  const char *src = R"(
    let 
    in 
      13 + 37;
      12 * 34;
      1 & 0;
      1 | 0
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());

  auto validate = [&se](size_t idx, int left, int right, ast::OperatorKind op) {
    EXPECT_GT(se.size(), idx);
    EXPECT_EQ(se.at(idx).getKind(), ast::ExprKind::Operator);
    ast::OperatorExpr &oe = se.at(idx).toOperator().get();
    EXPECT_EQ(oe.opKind(), op);
    EXPECT_EQ(oe.leftOperand().toIntLit().get().getValue(), left);
    EXPECT_EQ(oe.rightOperand().toIntLit().get().getValue(), right);
  };
  validate(0, 13, 37, ast::OperatorKind::Plus);
  validate(1, 12, 34, ast::OperatorKind::Times);
  validate(2, 1, 0, ast::OperatorKind::And);
  validate(3, 1, 0, ast::OperatorKind::Or);
}

TEST_F(ParserTest, ComparisonExpressions) {
  const char *src = R"(
    let 
    in 
      1 >= 0;
      1 > 0;
      1 <= 0;
      1 < 0;
      1 = 0;
      1 <> 0
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());

  auto validate = [&se](size_t idx, int left, int right, ast::OperatorKind op) {
    EXPECT_GT(se.size(), idx);
    EXPECT_EQ(se.at(idx).getKind(), ast::ExprKind::Operator);
    ast::OperatorExpr &oe = se.at(idx).toOperator().get();
    EXPECT_EQ(oe.opKind(), op);
    EXPECT_EQ(oe.leftOperand().toIntLit().get().getValue(), left);
    EXPECT_EQ(oe.rightOperand().toIntLit().get().getValue(), right);
  };
  validate(0, 1, 0, ast::OperatorKind::GreaterEqual);
  validate(1, 1, 0, ast::OperatorKind::GreaterThan);
  validate(2, 1, 0, ast::OperatorKind::LessEqual);
  validate(3, 1, 0, ast::OperatorKind::LessThan);
  validate(4, 1, 0, ast::OperatorKind::Equals);
  validate(5, 1, 0, ast::OperatorKind::Unequal);
}

TEST_F(ParserTest, PrecedenceParsing) {
  const char *src = R"(
    let 
    in 
      12 + 34 * 211 >= 32 | 23 <= 23 * 32 + 32
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());

  // the get() will fail on its own..
  ast::OperatorExpr &orEx = se.at(0).toOperator().get();
  ast::OperatorExpr &gtEx = orEx.leftOperand().toOperator().get();
  ast::OperatorExpr &sum1Ex = gtEx.leftOperand().toOperator().get();
  ast::IntLitExpr &i1Ex = sum1Ex.leftOperand().toIntLit().get();
  ast::OperatorExpr &mult1Ex = sum1Ex.rightOperand().toOperator().get();
  ast::IntLitExpr &i2Ex = mult1Ex.leftOperand().toIntLit().get();
  ast::IntLitExpr &i3Ex = mult1Ex.rightOperand().toIntLit().get();
  ast::OperatorExpr &ltEx = orEx.rightOperand().toOperator().get();
  ast::OperatorExpr &sum2Ex = ltEx.rightOperand().toOperator().get();
  ast::IntLitExpr &i4Ex = sum2Ex.rightOperand().toIntLit().get();
  ast::OperatorExpr &mult2Ex = sum2Ex.leftOperand().toOperator().get();
  ast::IntLitExpr &i5Ex = mult2Ex.leftOperand().toIntLit().get();
  ast::IntLitExpr &i6Ex = mult2Ex.rightOperand().toIntLit().get();
}

TEST_F(ParserTest, SimpleVariableAssignment) {
  const char *src = R"(
    let 
      var x: int := 0
    in 
      x := 123
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());

  ast::AssignExpr &ae = llvm::cantFail(se.at(0).toAssign());
  ast::VariableExpr &ve = llvm::cantFail(ae.variable().toVariable());
  EXPECT_EQ(ve.getKind(), ast::VariableKind::Simple);
  llvm::SmallString<64> sv;
  EXPECT_EQ(ve.getName(sv), "x");
  ast::IntLitExpr &ie = llvm::cantFail(ae.expr().toIntLit());
}

TEST_F(ParserTest, FieldVariableAssignment) {
  const char *src = R"(
    let 
      type ST = { inner: int }
      var myStruct := ST { inner = 0 }
    in 
      myStruct.inner := 123
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  ast::AssignExpr &ae = llvm::cantFail(se.at(0).toAssign());
  
  ast::VariableExpr &ve = llvm::cantFail(ae.variable().toVariable());
  EXPECT_EQ(ve.getKind(), ast::VariableKind::Field);
  llvm::SmallString<64> sv;
  EXPECT_EQ(ve.getName(sv), "myStruct.inner");
}

TEST_F(ParserTest, NestedFieldVariableAssignment) {
  const char *src = R"(
    let 
      type InnerS = { value: int }
      type OuterS = { inner: InnerS }

      var innerV := InnerS { value = 0 }
      var outerV := OuterS { inner = innerV }
    in 
      outerV.innerV.value := 42
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  ast::AssignExpr &ae = llvm::cantFail(se.at(0).toAssign());
  
  ast::VariableExpr &ve = llvm::cantFail(ae.variable().toVariable());
  EXPECT_EQ(ve.getKind(), ast::VariableKind::Field);
  llvm::SmallString<64> sv;
  EXPECT_EQ(ve.getName(sv), "outerV.innerV.value");
}

TEST_F(ParserTest, ArrayVariableAssignment) {
  const char *src = R"(
    let 
      var xs := int [10] of 0
    in 
      xs[0] := 42
    end
  )";
  Parser parser(src, ag.getAlloc());
  llvm::Expected<ast::ExprPtr> ast = parser.parse();
  ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
  ASSERT_TRUE(ast::hasKind(ast, ast::ExprKind::Let));
  ast::LetExpr &le = llvm::cantFail(ast.get()->toLet());
  ast::SequenceExpr &se = llvm::cantFail(le.innerExprs().toSequence());
  ast::AssignExpr &ae = llvm::cantFail(se.at(0).toAssign());
  
  ast::VariableExpr &ve = llvm::cantFail(ae.variable().toVariable());
  EXPECT_EQ(ve.getKind(), ast::VariableKind::Subscript);
  llvm::SmallString<64> sv;
  EXPECT_EQ(ve.getName(sv), "xs[]");
}

/// @}
