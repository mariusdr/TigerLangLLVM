#include "Logger.h"
#include <iostream>
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"

namespace tig {

Logger::Logger():
  out(std::cout),
  err(std::cerr)
{}



} // namespace
