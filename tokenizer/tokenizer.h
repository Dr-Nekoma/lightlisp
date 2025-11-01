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

class Token {
  std::variant<SymbolToken, NumberToken, StringToken, SyntaxToken> tok_;

public:
  Token(SymbolToken &&tok) : tok_(tok) {}
  Token(NumberToken &&tok) : tok_(tok) {}
  Token(StringToken &&tok) : tok_(tok) {}
  Token(SyntaxToken &&tok) : tok_(tok) {}

  Token(char ch) : tok_(SyntaxToken::ParenOpen) {
    switch (ch) {
    case '(':
      break;
    case ')':
      tok_ = SyntaxToken::ParenClose;
      break;
    case '\'':
      tok_ = SyntaxToken::Quote;
      break;
    case '.':
      tok_ = SyntaxToken::Dot;
      break;
    case '*':
    case '/':
      tok_ = SymbolToken({ch});
      break;
    default:
      throw std::runtime_error("unreachable");
    }
  }

  Token(std::string &&symbols) : tok_(SyntaxToken::ParenOpen) {
    if (isdigit(symbols.at(0)) ||
        ((symbols.at(0) == '-' || symbols.at(0) == '+') &&
         symbols.length() > 1 &&
         std::all_of(std::next(symbols.begin()), symbols.end(), isdigit))) {
      tok_ = NumberToken(std::stol(symbols));
    } else {
      tok_ = SymbolToken(symbols);
    }
  }

  bool operator==(const Token &other) const { return tok_ == other.tok_; }
  bool operator!=(const Token &other) const { return !(*this == other); }

  template <typename T> const T *get_if() const {
    return std::get_if<T>(&tok_);
  }
};

template <typename... Args>
inline bool Matches(char cur, char first, Args... rest) {
  return cur == first || (... || (cur == rest));
}

class TokenView {
  std::istream *working_stream_;

public:
  TokenView(std::istream *in) : working_stream_(in) {}

  class ConstIterator {
    using iterator_category = std::forward_iterator_tag;
    using value_type = Token;
    using difference_type = std::ptrdiff_t;
    using pointer = const value_type *;
    using reference = const value_type &;

  public:
    ConstIterator() = default;
    ConstIterator(const ConstIterator &) = default;
    ConstIterator &operator=(const ConstIterator &) = default;
    ConstIterator(std::istream *in) : working_stream_(in) {
      Step();
      ++(*this);
    }

    ConstIterator(std::istream *in, char)
        : this_token_(std::nullopt), working_stream_(in), cur_(EOF) {}

    reference operator*() const { return this_token_.value(); }
    pointer operator->() const { return &this_token_.value(); }

    ConstIterator &operator++() {
      this_token_.reset();
      if (IsEnd())
        return *this;

      std::string accum_token;

      for (; !IsEnd(); Step()) {
        if (Matches(cur_, '(', ')', '.', '*', '/', '\'')) {
          if (accum_token.empty()) {
            this_token_ = Token(cur_);
            Step();
            return *this;
          }
          RecordLongToken(std::move(accum_token));
          return *this;
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
          Step();
          return *this;
        } else if (cur_ == '\n' && !accum_token.empty()) {
          line++;
          RecordLongToken(std::move(accum_token));
          return *this;
        } else if (std::isspace(cur_) && !accum_token.empty()) {
          RecordLongToken(std::move(accum_token));
          return *this;
        } else if (isalnum(cur_) ||
                   Matches(cur_, '?', '!', '#', '>', '<', '=', '_', '%')) {
          accum_token.push_back(cur_);
        } else if (Matches(cur_, '+', '-')) {
          if (accum_token.empty() && !isdigit(cur_)) {
            this_token_ = SymbolToken({cur_});
            Step();
            return *this;
          }
          if (!isalpha(accum_token.at(0))) {
            RecordLongToken(std::move(accum_token));
            return *this;
          }
          accum_token.push_back(cur_);
        }
      }
      if (!accum_token.empty())
        RecordLongToken(std::move(accum_token));
      return *this;
    }
    ConstIterator operator++(int) {
      ConstIterator tmp = *this;
      ++(*this);
      return tmp;
    }

    bool operator==(const ConstIterator &other) const {
      return working_stream_ == other.working_stream_ &&
             this_token_ == other.this_token_ && cur_ == other.cur_;
    }
    bool operator!=(const ConstIterator &other) const {
      return !(*this == other);
    }

  private:
    bool IsEnd() { return cur_ == EOF; }

    void Step() { cur_ = working_stream_->get(); }

    void RecordLongToken(std::string &&accum_token) {
      this_token_ = Token(std::move(accum_token));
    }

    std::optional<Token> this_token_ = std::nullopt;
    std::istream *working_stream_;
    char cur_;
    size_t line = 1;
  };

  ConstIterator begin() { return {working_stream_}; }
  ConstIterator end() { return {working_stream_, EOF}; }
};