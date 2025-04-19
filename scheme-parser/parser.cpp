#include "parser.h"
#include <variant>

Parser::Parser(Tokenizer &&tok) : tokenizer_(tok) {}

void Parser::ParenClose() {
  paren_count_--;
  if (paren_count_ < 0)
    throw std::runtime_error("Unexpected closing parentheses!");
}

void Parser::ParenOpen() { paren_count_++; }

std::shared_ptr<SyntaxObject> Parser::ReadProper() {
  if (tokenizer_.IsEnd())
    throw std::runtime_error("Unexpected end of expression");

  auto current_object = tokenizer_.GetToken().value();

  if (SymbolToken *symbol = std::get_if<SymbolToken>(&current_object)) {
    return std::make_shared<SyntaxObject>(Symbol(symbol->name));
  } else if (NumberToken *num_tok = std::get_if<NumberToken>(&current_object)) {
    return std::make_shared<SyntaxObject>(Number(num_tok->value));
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

std::shared_ptr<SyntaxObject> Parser::ReadList() {
  tokenizer_.Next();
  if (tokenizer_.IsEnd())
    throw std::runtime_error("Input not complete");

  std::shared_ptr<SyntaxObject> head;
  Cell *tail = nullptr;
  while (true) {
    auto current_token = tokenizer_.GetToken().value();
    if (auto syntax = std::get_if<SyntaxToken>(&current_token);
        syntax != nullptr) {
      if (*syntax == SyntaxToken::ParenClose) {
        ParenClose();
        if (paren_count_ != 0)
          tokenizer_.Next();
        return head;
      } else if (*syntax == SyntaxToken::Dot) {
        tokenizer_.Next();
        if (tail == nullptr)
          throw std::runtime_error("Improper list syntax");

        auto second = ReadProper();
        tail->get<1>() = second;
        tokenizer_.Next();
        if (auto syntax = std::get_if<SyntaxToken>(&current_token);
            !(syntax && *syntax == SyntaxToken::ParenClose))
          throw std::runtime_error("Improper list syntax");

        continue;
      }
    }
    auto current_object = ReadProper();
    if (!std::holds_alternative<Cell>(*current_object)) // hacky
      tokenizer_.Next();
    Cell new_cell;
    new_cell.get<0>() = current_object;
    auto new_node = std::make_shared<SyntaxObject>(std::move(new_cell));
    Cell *new_cell_ptr = &std::get<Cell>(*new_node);
    if (head == nullptr)
      head = new_node;
    else
      tail->get<1>() = new_node;
    tail = new_cell_ptr;
  }
}

std::shared_ptr<SyntaxObject> Parser::Read() {
  tokenizer_.Next();
  return ReadProper();
}