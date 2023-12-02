cmake_minimum_required(VERSION 3.18)
project(TigerLang C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(TigerLang 
  src/main.cpp
  src/Lexer.cpp
  src/Token.cpp
  src/Logger.cpp
  src/Ast.cpp
  src/ErrorMessage.cpp
  src/VisualizeAst.cpp
  src/Parser.cpp
  src/SymbolTable.cpp
  src/TypeCheck.cpp
)

target_compile_options(TigerLang PRIVATE -Wall -Wextra -Wpedantic -flto)
target_link_libraries(TigerLang PRIVATE readline)

llvm_map_components_to_libnames(llvm_libs Support Core Irreader)
target_link_libraries(TigerLang PRIVATE ${llvm_libs})

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

### Googletest 

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(ParserTest 
  src/ParserTest.cpp 
  src/Ast.cpp 
  src/Token.cpp 
  src/Parser.cpp 
  src/Lexer.cpp 
  src/ErrorMessage.cpp
  src/SymbolTable.cpp
)

target_link_libraries(ParserTest PRIVATE 
  GTest::gtest_main
  ${llvm_libs})

include(GoogleTest)
gtest_discover_tests(ParserTest)


add_executable(TypeCheckTest 
  src/TypeCheckTest.cpp 
  src/Ast.cpp 
  src/Token.cpp 
  src/Parser.cpp 
  src/Lexer.cpp 
  src/ErrorMessage.cpp
  src/SymbolTable.cpp
  src/TypeCheck.cpp
)

target_link_libraries(TypeCheckTest PRIVATE 
  GTest::gtest_main
  ${llvm_libs})

include(GoogleTest)
gtest_discover_tests(TypeCheckTest)



