#pragma once

#include "meta.h"
#include "tokenizer.h"
#include <concepts>
#include <cstdint>
#include <istream>
#include <memory>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <sys/stat.h>
#include <unordered_map>
#include <utility>

class Parser {
public:
  explicit Parser(Tokenizer &&tok);

  std::unique_ptr<SyntaxObject> ReadList();

  std::unique_ptr<SyntaxObject> Read();

  std::unique_ptr<SyntaxObject> ReadProper();

  bool IsEnd() { return tokenizer_.IsEnd(); }

private:
  void ParenClose();
  void ParenOpen();
  Tokenizer tokenizer_;
  int64_t paren_count_ = 0;
};
