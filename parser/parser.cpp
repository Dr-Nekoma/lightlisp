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
    auto result = std::make_unique<SyntaxObject>(std::move(symbol->name));
    ++it_;
    return result;
  }

  if (auto num_tok = it_->get_if<NumberToken>()) {
    auto result = std::make_unique<SyntaxObject>(num_tok->value);
    ++it_;
    return result;
  }

  auto syntax = it_->get_if<SyntaxToken>();
  if (!syntax)
    throw std::runtime_error("Unexpected token");

  if (*syntax == SyntaxToken::Dot)
    throw std::runtime_error("Unexpected dot");

  if (*syntax == SyntaxToken::ParenClose)
    throw std::runtime_error("Unexpected closing parenthesis");

  ParenOpen();
  ++it_;
  return ReadList();
}

std::unique_ptr<SyntaxObject> Parser::ReadList() {
  std::unique_ptr<SyntaxObject> head = nullptr;
  Cell *tail = nullptr;

  while (true) {
    if (it_ == tokenizer_.end())
      throw std::runtime_error("Unclosed list");

    if (auto syntax = it_->get_if<SyntaxToken>()) {
      if (*syntax == SyntaxToken::ParenClose) {
        ParenClose();
        ++it_;
        return head ? std::move(head) : std::make_unique<SyntaxObject>("nil");
      }

      if (*syntax == SyntaxToken::Dot) {
        if (tail == nullptr)
          throw std::runtime_error("Dot in improper position");
        ++it_;

        tail->get<1>() = ReadProper();

        if (it_ == tokenizer_.end() || !it_->get_if<SyntaxToken>() ||
            *it_->get_if<SyntaxToken>() != SyntaxToken::ParenClose)
          throw std::runtime_error("Expected ) after dot expression");
        continue;
      }
    }

    auto element = ReadProper();

    Cell new_cell;
    new_cell.get<0>() = std::move(element);
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