#ifndef TIGER_LANG_LOGGER
#define TIGER_LANG_LOGGER

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/FormatVariadic.h"

namespace tig {

class Logger {
public:
  Logger();
  llvm::raw_ostream &stdOut() { return out; }
  llvm::raw_ostream &stdErr() { return err; }

  template <typename... Ts>
  void logv(const char *fmt, Ts &&... vals) {
    out << llvm::formatv(fmt, std::forward<Ts>(vals)...);
  }

  template <typename... Ts> 
  void errv(const char *fmt, Ts &&... vals) {
    err << llvm::formatv(fmt, std::forward<Ts>(vals)...);
  }

private:
  llvm::raw_os_ostream out;
  llvm::raw_os_ostream err;
};

} // namespace tig

#endif
