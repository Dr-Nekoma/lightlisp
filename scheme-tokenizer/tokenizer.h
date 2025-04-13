#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <variant>

struct SymbolToken {
  SymbolToken(std::string new_name) : name(std::move(new_name)) {}
  bool operator==(const SymbolToken &rhs) const { return (name == rhs.name); }

  std::string name;
};

struct StringToken {
  StringToken(std::string new_name) : name(std::move(new_name)) {}
  bool operator==(const StringToken &rhs) const { return (name == rhs.name); }

  std::string name;
};

enum class SyntaxToken { Quote, Dot, Null, ParenOpen, ParenClose };

struct NumberToken {
  explicit NumberToken(int64_t number) : value(number) {}
  bool operator==(const NumberToken &rhs) const { return (value == rhs.value); }

  int64_t value;
};

using Token = std::variant<SymbolToken, NumberToken, StringToken, SyntaxToken>;

inline Token Tokenize_special(char ch) {
  switch (ch) {
  case '(':
    return SyntaxToken::ParenOpen;
  case ')':
    return SyntaxToken::ParenClose;
  case '\'':
    return SyntaxToken::Quote;
  case '.':
    return SyntaxToken::Dot;
  case '*':
  case '/':
    return SymbolToken({ch});
  default:
    throw std::runtime_error("unreachable");
  }
}

inline Token MakeLongToken(std::string &&symbols) {
  if (isdigit(symbols.at(0)) ||
      ((symbols.at(0) == '-' || symbols.at(0) == '+') && symbols.length() > 1 &&
       std::all_of(std::next(symbols.begin()), symbols.end(), isdigit))) {
    return NumberToken(std::stol(symbols));
  }
  return SymbolToken(symbols);
}
template <typename... Args>
inline bool Matches(char cur, char first, Args... rest) {
  return cur == first || (... || (cur == rest));
}

class Tokenizer {
public:
  Tokenizer(std::istream *in) : working_stream_(in) { Step(); }

  bool IsEnd() { return cur_ == EOF; }

  void Next() {
    this_token_.reset();
    if (IsEnd())
      return;

    std::string accum_token;

    for (; !IsEnd(); Step()) {
      if (Matches(cur_, '(', ')', '.', '*', '/', '\'')) {
        if (accum_token.empty()) {
          this_token_ = Tokenize_special(cur_);
          return Step();
        }
        return RecordLongToken(std::move(accum_token));
      } else if (cur_ == '"') {
        Step();
        while (cur_ != '"') {
          if (IsEnd())
            throw std::runtime_error("Unexpected end of string input");
          if (cur_ == '"')
            break;
          accum_token.push_back(cur_);
          Step();
        }
        this_token_ = StringToken(std::move(accum_token));
        return Step();
      } else if (std::isspace(cur_) && !accum_token.empty()) {
        return RecordLongToken(std::move(accum_token));
      } else if (isalnum(cur_) ||
                 Matches(cur_, '?', '!', '#', '>', '<', '=', '_', '%')) {
        accum_token.push_back(cur_);
      } else if (Matches(cur_, '+', '-')) {
        if (accum_token.empty() && !isdigit(Peek())) {
          this_token_ = SymbolToken({cur_});
          return Step();
        }
        if (!isalpha(accum_token.at(0)))
          return RecordLongToken(std::move(accum_token));
        accum_token.push_back(cur_);
      }
    }
    if (!accum_token.empty())
      RecordLongToken(std::move(accum_token));
  }

  std::optional<Token> GetToken() { return this_token_; }

private:
  void RecordLongToken(std::string &&accum_token) {
    this_token_ = MakeLongToken(std::move(accum_token));
  }

  char Peek() { return cur_; }
  void Step() { cur_ = working_stream_->get(); }

  std::optional<Token> this_token_ = std::nullopt;
  std::istream *working_stream_;
  char cur_;
};
