#include "ErrorMessage.h"
#include <vector>

namespace tig {

static const char *MsgTexts[] = {
  #define DEF(no, txt) txt,
  #include "ErrorMessage.inl"
  #undef DEF
};

const char *message(MsgNo msg) {
  return MsgTexts[static_cast<size_t>(msg)];
}

char TigError::ID;

struct TigErrCategory : std::error_category {
  const char *name() const noexcept override {
    return "TigerLang";
  }

  std::string message(int ev) const override {
    switch (static_cast<ErrC>(ev)) {
      case ErrC::LexerError:
        return "LexerError";
      case ErrC::ParserError:
        return "ParserError";
      case ErrC::TypeCheckError:
        return "TypeCheckError";
      default:
        return "Uknown";
    }
  }
};

const TigErrCategory Category {};

std::error_code make_error_code(ErrC ec) {
  return {static_cast<int>(ec), Category};
}

} // namespace
