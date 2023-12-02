#include "gtest/gtest.h"
#include "Parser.h"
#include "Ast.h"
#include "TypeCheck.h"

using namespace tig;

class TypeCheckerTest : public ::testing::Test {
protected:
  ArenaScopeGuard ag;
  
  void _parseSrc(const char *src, ast::ExprPtr &ptr) {
    Parser p(src, ag.getAlloc());
    llvm::Expected<ast::ExprPtr> ast = p.parse();
    ASSERT_TRUE(bool(ast)) << llvm::toString(ast.takeError()) << "\n";
    ptr = std::move(ast.get());
  }
};

#define parseSrc(src, ptr) _parseSrc(src, ptr); ASSERT_TRUE(ptr)


TEST_F(TypeCheckerTest, PrimitiveExpressions) {
  const char *src = R"(
    let 
    in 
      1323;
      "foo"
    end
  )";
  ast::ExprPtr ast = nullptr;
  parseSrc(src, ast);

  auto tc = TypeChecker::create();
  llvm::Error err = tc->check(*ast);
  ASSERT_FALSE(bool(err)) << llvm::toString(std::move(err)) << "\n";
}

TEST_F(TypeCheckerTest, ValidBinaryOperators) {
  const char *src = R"(
    let 
    in 
      1323 + 323;
      1323 * 83283;
      112 / 323;
      1121 - 2325;
      132 & 0;
      0 | 0
    end
  )";
  ast::ExprPtr ast = nullptr;
  parseSrc(src, ast);

  auto tc = TypeChecker::create();
  llvm::Error err = tc->check(*ast);
  ASSERT_FALSE(bool(err)) << llvm::toString(std::move(err)) << "\n";
}

TEST_F(TypeCheckerTest, ValidComparisons) {
  const char *src = R"(
    let 
    in 
      /*== greater equal ==*/

      829232 >= 2832832;
      "auwheqiue23bi21b" >= "2ub2obob3urb2i";

      /*== greater than ==*/

      2392323 > 23923031;
      "jwqehjqhe231iu31" > "qioueh2ub2131obro";

      /*== less equal ==*/

      92913319 <= 919313813;
      "qehowuqheuqe2b1ob1" <= "qieqeieqiuh2h32";

      /*== less than ==*/
      29329 < 120129192;
      "qijweqie" < "2j32323h";

      /*== equals ==*/
      293139328 = 2393239121;
      "jkqjkrhqrkqjh" = "N31k3b1i3bi1b";

      /* unequals */
      2393881212 <> 392838238;
      "iehqiehqiehqi" <> "jh2ih1i3h1i"
    end
  )";
  ast::ExprPtr ast = nullptr;
  parseSrc(src, ast);

  auto tc = TypeChecker::create();
  llvm::Error err = tc->check(*ast);
  ASSERT_FALSE(bool(err)) << llvm::toString(std::move(err)) << "\n";
}
