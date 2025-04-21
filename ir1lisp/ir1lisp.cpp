#include "ir1lisp.h"

ObjectBuilder::ObjectBuilder() {
  auto if_builder = [](ObjectBuilder &builder, std::string &,
                       SyntaxObject &syntax) -> ObjPtr {
    auto view = Cell::ListView{std::make_shared<SyntaxObject>(syntax)};
    auto it = view.begin();
    auto cond = codeWalk(builder, *it);
    it++;
    auto thenClause = codeWalk(builder, *it);
    it++;
    auto elseClause = codeWalk(builder, *it);
    it++;
    assert(it == view.end());
    return std::make_shared<If>(std::move(cond), std::move(thenClause),
                                std::move(elseClause));
  };
  builders_["if"] = if_builder;

  auto def_builder = [](ObjectBuilder &builder, std::string &,
                        SyntaxObject &syntax) -> ObjPtr {
    auto view = Cell::ListView{std::make_shared<SyntaxObject>(syntax)};
    auto it = view.begin();
    std::string name;
    if (auto sym = std::get_if<Symbol>(&*it)) {
      name = sym->getName();
    } else {
      throw std::runtime_error("bad function name");
    }
    it++;
    auto argVec = parseArgList(*it);
    it++;
    auto body = codeWalk(builder, *it);
    if (body) {
      auto Proto = std::make_unique<Prototype>(name, argVec);
      return std::make_shared<Function>(std::move(Proto), std::move(body));
    } else {
      return std::make_shared<Prototype>(name, std::move(argVec));
    }
  };

  builders_["def"] = def_builder;

  auto builtin_builder = [](ObjectBuilder &builder, std::string &name,
                            SyntaxObject &syntax) -> ObjPtr {
    auto view = Cell::ListView{std::make_shared<SyntaxObject>(syntax)};
    auto it = view.begin();
    auto fst = codeWalk(builder, *it);
    it++;
    auto snd = codeWalk(builder, *it);
    // assert(cdrCell->get<1>() == nullptr);
    return std::make_shared<BuiltInOp>(name, std::move(fst), std::move(snd));
  };

  builders_["+"] = builtin_builder;
  builders_["-"] = builtin_builder;
  builders_["*"] = builtin_builder;
  builders_["<"] = builtin_builder;
  builders_["setq"] = builtin_builder;

  auto tagbody_builder = [](ObjectBuilder &builder, std::string &,
                            SyntaxObject &syntax) -> ObjPtr {
    auto view = Cell::ListView{std::make_shared<SyntaxObject>(syntax)};
    std::vector<std::variant<ObjPtr, std::string>> body;
    for (auto val : view) {
      if (auto sym = std::get_if<Symbol>(&val))
        body.emplace_back(sym->getName());
      else
        body.emplace_back(codeWalk(builder, val));
    }
    return std::make_shared<Goto>(std::move(body));
  };

  builders_["tagbody"] = tagbody_builder;

  auto go_builder = [](ObjectBuilder &builder, std::string &name,
                       SyntaxObject &syntax) -> ObjPtr {
    auto view = Cell::ListView{std::make_shared<SyntaxObject>(syntax)};
    auto it = view.begin();
    if (auto sym = std::get_if<Symbol>(&*it)) {
      auto name = sym->getName();
      return std::make_shared<Go>(std::move(name));
    } else {
      throw std::runtime_error("Non symbol cannot be a tag");
    }
  };

  builders_["go"] = go_builder;
}

std::vector<std::string> parseArgList(SyntaxObject &cell) {
  std::vector<std::string> res;
  auto view = Cell::ListView{std::make_shared<SyntaxObject>(cell)};
  for (auto node : view) {
    if (auto sym = std::get_if<Symbol>(&node)) {
      res.emplace_back(sym->getName());
    } else {
      throw std::runtime_error("Not a valid arg name");
    }
  }
  return res;
}

std::vector<ObjPtr> lispListToVec(ObjectBuilder &builder, SyntaxObject &cell) {
  std::vector<ObjPtr> res;
  auto view = Cell::ListView{std::make_shared<SyntaxObject>(cell)};
  for (auto node : view) {
    res.emplace_back(codeWalk(builder, node));
  }
  return res;
}

ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax) {
  if (Number *number = std::get_if<Number>(&syntax)) {
    return std::make_shared<Number>(number->getValue());
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
    } else if (Symbol *symbol = std::get_if<Symbol>(&fstObj)) {
      auto name = symbol->getName();
      auto body = cell.get<1>(); // can be nullptr
      if (!std::holds_alternative<Cell>(*body)) {
        throw std::runtime_error("Imporper syntax");
      }
      if (auto search = builder.builders_.find(name);
          search != builder.builders_.end()) {
        return search->second(builder, name, *body);
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

ObjPtr ir1LispTransform(std::shared_ptr<SyntaxObject> syntax) {
  ObjectBuilder builder;
  return codeWalk(builder, *syntax);
}