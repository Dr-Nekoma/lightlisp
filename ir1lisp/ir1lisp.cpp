#include "ir1lisp.h"

ObjectBuilder::ObjectBuilder() {
  auto if_builder = [](ObjectBuilder &builder, SyntaxObject &syntax) -> ObjPtr {
    auto body = std::get<Cell>(syntax);
    auto condCell = body.get<0>();
    auto cond = codeWalk(builder, *condCell);
    auto cdr = *body.get<1>();
    if (auto cdrCell = std::get_if<Cell>(&cdr); cdrCell) {
      auto thenCell = cdrCell->get<0>();
      auto then = codeWalk(builder, *thenCell);
      auto cdr = *cdrCell->get<1>();
      if (auto cdrCell = std::get_if<Cell>(&cdr); cdrCell) {
        auto elseCell = cdrCell->get<0>();
        auto maybeElse = codeWalk(builder, *elseCell);
        return std::make_shared<If>(cond, then, maybeElse);
      } else {
        throw std::runtime_error("Improper if body");
      }
    }
    throw std::runtime_error("Improper if body");
  };
  builders_["if"] = if_builder;

  auto def_builder = [](ObjectBuilder &builder,
                        SyntaxObject &syntax) -> ObjPtr {
    auto body = std::get<Cell>(syntax);
    auto name = body.get<0>();
    if (!std::holds_alternative<Symbol>(*name)) {
      throw std::runtime_error("bad function name");
    }
    auto cdr = *body.get<1>();
    if (auto cdrCell = std::get_if<Cell>(&cdr); cdrCell) {
      auto args = cdrCell->get<0>();
      auto argVec = parseArgList(*args);
      auto cdr = *cdrCell->get<1>();
      if (auto cdrCell = std::get_if<Cell>(&cdr); cdrCell) {
        auto bodyCell = cdrCell->get<0>();
        auto body = codeWalk(builder, *bodyCell);
        auto name_str = std::get<Symbol>(*name).getName();
        if (body) {
          auto Proto = std::make_unique<Prototype>(name_str, argVec);
          return std::make_unique<Function>(std::move(Proto), std::move(body));
        } else {
          return std::make_unique<Prototype>(name_str, argVec);
        }
      } else {
        throw std::runtime_error("Improper def body");
      }
    }
    throw std::runtime_error("Improper def body");
  };

  builders_["def"] = def_builder;
}

std::vector<std::string> parseArgList(SyntaxObject &cell) {
  std::vector<std::string> res;
  for (auto current = cell;;) {
    if (auto current_cell = std::get_if<Cell>(&current); current_cell) {
      auto fst = *current_cell->get<0>();
      if (auto sym = std::get_if<Symbol>(&fst)) {
        res.emplace_back(sym->getName());
      } else {
        throw std::runtime_error("Not a valid arg name");
      }
      auto snd = current_cell->get<1>();
      if (!snd) {
        break;
      }
      current = std::move(*snd);
    } else {
      throw std::runtime_error("Bad List");
    }
  }
  return res;
}

std::vector<ObjPtr> lispListToVec(ObjectBuilder &builder, SyntaxObject &cell) {
  std::vector<ObjPtr> res;
  for (auto current = cell;;) {
    if (auto current_cell = std::get_if<Cell>(&current); current_cell) {
      auto fst = current_cell->get<0>();
      auto compiled = codeWalk(builder, *fst);
      res.push_back(compiled);
      auto snd = current_cell->get<1>();
      if (!snd) {
        break;
      }
      current = std::move(*snd);
    } else {
      throw std::runtime_error("Bad List");
    }
  }
  return res;
}

ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax) {
  if (Number *number = std::get_if<Number>(&syntax)) {
    return std::shared_ptr<Number>(number);
  } else if (Symbol *symbol = std::get_if<Symbol>(&syntax)) {
    return std::make_shared<Variable>(symbol->getName());
  } else {
    Cell cell = std::get<Cell>(syntax);
    auto fst = cell.get<0>();
    if (!fst)
      throw std::runtime_error(
          "Incorrect parsing, empty beginning of the cell");
    auto fstObj = *fst;
    if (std::holds_alternative<Number>(fstObj)) {
      throw std::runtime_error("Incorrect form, cannot funcall a number");
    } else if (Symbol *symbol = std::get_if<Symbol>(&syntax)) {
      auto name = symbol->getName();
      auto body = cell.get<1>();
      if (!std::holds_alternative<Cell>(*body)) {
        throw std::runtime_error("Imporper syntax");
      }
      if (auto search = builder.builders_.find(name);
          search != builder.builders_.end()) {
        return search->second(builder, *body);
      } else {
        auto args = lispListToVec(builder, *body);
        return std::make_shared<Call>(name, args);
      }
    } else {
      // for now
      throw std::runtime_error("Lambdas not yet supported");
    }
  }
}

ObjPtr ir1LispTransform(SyntaxObject &syntax) {
  ObjectBuilder builder;
  return codeWalk(builder, syntax);
}