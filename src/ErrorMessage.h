#ifndef TIGER_LANG_ERROR_MESSAGE_H
#define TIGER_LANG_ERROR_MESSAGE_H
#include <cstdint>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormatVariadic.h>
#include <system_error>

namespace tig {
enum class ErrC {
  LexerError,
  ParserError,
  TypeCheckError
};
}

namespace std {
template <> 
struct is_error_code_enum<tig::ErrC> : true_type {};
}

namespace tig {

std::error_code make_error_code(ErrC);

enum class MsgNo : uint16_t {
  #define DEF(no, txt) no,
  #include "ErrorMessage.inl"
  #undef DEF
};

/// @brief Read message text for the given message.
/// @param msg message number
/// @return structure with message text and class
const char *message(MsgNo msg);

class TigError : public llvm::ErrorInfo<TigError> {
public:
  static char ID;
  ErrC errc;
  MsgNo num;
  std::string buffer;

  template <typename... Ts> 
  TigError(ErrC errc, MsgNo num, Ts &&... vals): 
    errc(errc), num(num) 
  {
    const char *txt = tig::message(num);
    llvm::raw_string_ostream stream(buffer);
    stream << llvm::formatv(txt, std::forward<Ts>(vals)...);
  }

  void log(llvm::raw_ostream &os) const override {
    os << buffer;
  }

  std::error_code convertToErrorCode() const override {
    return errc;
  }
};

template <typename... Ts>
llvm::Error makeError(ErrC ec, MsgNo num, Ts &&... vals) {
  return llvm::make_error<TigError>(ec, num, std::forward<Ts>(vals)...);
}

// template <typename... Ts>
// llvm::Error makeParserError(MsgNo num, Ts &&... vals) {
//   return llvm::make_error<TigError>(ErrC::ParserError, num, std::forward<Ts>(vals)...);
// }

// template <typename... Ts>
// llvm::Error makeLexerError(MsgNo num, Ts &&... vals) {
//   return llvm::make_error<TigError>(ErrC::LexerError, num, std::forward<Ts>(vals)...);
// }

// template <typename... Ts>
// llvm::Error makeTypeCheckError(MsgNo num, Ts &&... vals) {
//   return llvm::make_error<TigError>(ErrC::TypeCheckError, num, std::forward<Ts>(vals)...);
// }



template <typename... Ts>
llvm::Error emitStringError(const char *err, Ts &&... vals) {
  auto fmt = llvm::formatv(err, std::forward<Ts>(vals)...);
  auto ec = std::make_error_code(std::errc::invalid_argument);
  return llvm::createStringError(ec, fmt.str());
}

template <typename... Ts>
llvm::Error emitStringError(MsgNo msgNum, Ts &&... vals) {
  return emitStringError(message(msgNum), std::forward<Ts>(vals)...);
}

} // namespace
#endif
