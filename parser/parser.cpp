#include "parser.h"
#include "compiler.h"
#include <variant>

Parser::Parser(TokenView &&tok) : tokenizer_(tok), it_(tokenizer_.begin()) {}

void Parser::ParenClose() {
  paren_count_--;
  if (paren_count_ < 0)
    throw std::runtime_error("Unexpected closing parentheses!");
}

void Parser::ParenOpen() { paren_count_++; }

std::unique_ptr<SyntaxObject> Parser::ReadProper() {
  if (it_ == tokenizer_.end())
    throw std::runtime_error("Unexpected end of expression");

  if (auto symbol = it_->get_if<SymbolToken>()) {
    return std::make_unique<SyntaxObject>(std::move(symbol->name));
  } else if (auto num_tok = it_->get_if<NumberToken>()) {
    return std::make_unique<SyntaxObject>(num_tok->value);
  } else {
    auto syntax = it_->get_if<SyntaxToken>();
    if (*syntax == SyntaxToken::Dot) {
      throw std::runtime_error("Unexpected symbol");
    } else {
      if (*syntax == SyntaxToken::ParenClose)
        ParenClose();
      else
        ParenOpen();
      return ReadList();
    }
  }
  throw std::runtime_error("Unexpected symbol");
}

std::unique_ptr<SyntaxObject> Parser::ReadList() {
  ++it_;
  if (it_ == tokenizer_.end())
    throw std::runtime_error("Input not complete");

  std::unique_ptr<SyntaxObject> head;
  Cell *tail = nullptr;
  while (true) {
    if (auto syntax = it_->get_if<SyntaxToken>(); syntax != nullptr) {
      if (*syntax == SyntaxToken::ParenClose) {
        ParenClose();
        ++it_;
        return head;
      } else if (*syntax == SyntaxToken::Dot) {
        ++it_;
        if (tail == nullptr)
          throw std::runtime_error("Improper list syntax");

        auto second = ReadProper();
        tail->get<1>() = std::move(second);
        ++it_;
        if (auto syntax = it_->get_if<SyntaxToken>();
            !(syntax && *syntax == SyntaxToken::ParenClose))
          throw std::runtime_error("Improper list syntax");

        continue;
      }
    }
    auto current_object = ReadProper();
    if (current_object && !current_object->is<Cell>()) // hacky
      ++it_;
    Cell new_cell;
    new_cell.get<0>() = std::move(current_object);
    auto new_node = std::make_unique<SyntaxObject>(std::move(new_cell));
    auto new_cell_ptr = new_node->get_if<Cell>();
    if (head == nullptr)
      head = std::move(new_node);
    else
      tail->get<1>() = std::move(new_node);
    tail = new_cell_ptr;
  }
}

std::unique_ptr<SyntaxObject> Parser::Read() {
  auto ret = ReadProper();
  return ret;
}