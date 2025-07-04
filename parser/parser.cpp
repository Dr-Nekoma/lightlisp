#include "parser.h"
#include "compiler.h"
#include <variant>

Parser::Parser(Tokenizer &&tok) : tokenizer_(tok) {}

void Parser::ParenClose() {
  paren_count_--;
  if (paren_count_ < 0)
    throw std::runtime_error("Unexpected closing parentheses!");
}

void Parser::ParenOpen() { paren_count_++; }

std::unique_ptr<SyntaxObject> Parser::ReadProper() {
  if (tokenizer_.IsEnd())
    throw std::runtime_error("Unexpected end of expression");

  auto current_object = tokenizer_.GetToken().value();

  if (auto symbol = std::get_if<SymbolToken>(&current_object)) {
    return std::make_unique<SyntaxObject>(Symbol(symbol->name));
  } else if (auto num_tok = std::get_if<NumberToken>(&current_object)) {
    return std::make_unique<SyntaxObject>(Number(num_tok->value));
  } else {
    auto syntax = std::get<SyntaxToken>(current_object);
    if (syntax == SyntaxToken::Dot) {
      throw std::runtime_error("Unexpected symbol");
    } else {
      if (syntax == SyntaxToken::ParenClose)
        ParenClose();
      else
        ParenOpen();
      return ReadList();
    }
  }
  throw std::runtime_error("Unexpected symbol");
}

std::unique_ptr<SyntaxObject> Parser::ReadList() {
  tokenizer_.Next();
  if (tokenizer_.IsEnd())
    throw std::runtime_error("Input not complete");

  std::unique_ptr<SyntaxObject> head;
  Cell *tail = nullptr;
  while (true) {
    auto current_token = tokenizer_.GetToken().value();
    if (auto syntax = std::get_if<SyntaxToken>(&current_token);
        syntax != nullptr) {
      if (*syntax == SyntaxToken::ParenClose) {
        ParenClose();
        tokenizer_.Next();
        return head;
      } else if (*syntax == SyntaxToken::Dot) {
        tokenizer_.Next();
        if (tail == nullptr)
          throw std::runtime_error("Improper list syntax");

        auto second = ReadProper();
        tail->get<1>() = std::move(second);
        tokenizer_.Next();
        if (auto syntax = std::get_if<SyntaxToken>(&current_token);
            !(syntax && *syntax == SyntaxToken::ParenClose))
          throw std::runtime_error("Improper list syntax");

        continue;
      }
    }
    auto current_object = ReadProper();
    if (current_object &&
        !std::holds_alternative<Cell>(*current_object)) // hacky
      tokenizer_.Next();
    Cell new_cell;
    new_cell.get<0>() = std::move(current_object);
    auto new_node = std::make_unique<SyntaxObject>(std::move(new_cell));
    Cell *new_cell_ptr = &std::get<Cell>(*new_node);
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