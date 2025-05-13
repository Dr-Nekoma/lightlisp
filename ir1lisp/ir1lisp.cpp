#include "ir1lisp.h"

ObjectBuilder::ObjectBuilder() {
  auto if_builder = [](ObjectBuilder &builder, std::string &,
                       SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    auto cond = codeWalk(builder, *it);
    it++;
    auto thenClause = codeWalk(builder, *it);
    it++;
    auto elseClause = codeWalk(builder, *it);
    it++;
    assert(it == view.end());
    return std::make_unique<If>(std::move(cond), std::move(thenClause),
                                std::move(elseClause));
  };
  builders_["if"] = if_builder;

  auto def_builder = [](ObjectBuilder &builder, std::string &,
                        SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    std::string name;
    if (auto sym = std::get_if<Symbol>(&*it)) {
      name = sym->getName();
      if (name == "main")
        name = "lisp_main";
    } else {
      throw std::runtime_error("bad function name");
    }
    it++;
    auto maybeCell = it.getCell();
    if (auto cell = std::get_if<Cell>(maybeCell)) {
      auto argVec = parseArgList(cell->get<0>().get());
      it++;
      auto body = codeWalk(builder, *it);
      return std::make_unique<Function>(std::move(name), std::move(argVec),
                                        std::move(body));
    } else {
      throw std::runtime_error("Arg list is not a list");
    }
  };

  builders_["def"] = def_builder;

  auto setq_builder = [](ObjectBuilder &builder, std::string &name,
                         SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    auto fst = codeWalk(builder, *it);
    it++;
    auto snd = codeWalk(builder, *it);
    // assert(cdrCell->get<1>() == nullptr);
    return std::make_unique<Setq>(name, std::move(fst), std::move(snd));
  };

  builders_["setq"] = setq_builder;

  auto tagbody_builder = [](ObjectBuilder &builder, std::string &,
                            SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    std::vector<std::variant<ObjPtr, std::string>> body;
    for (auto &val : view) {
      if (auto sym = std::get_if<Symbol>(&val))
        body.emplace_back(sym->getName());
      else
        body.emplace_back(codeWalk(builder, val));
    }
    return std::make_unique<Goto>(std::move(body));
  };

  builders_["tagbody"] = tagbody_builder;

  auto go_builder = [](ObjectBuilder &builder, std::string &name,
                       SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto sym = std::get_if<Symbol>(&*it)) {
      auto name = sym->getName();
      return std::make_unique<Go>(std::move(name));
    } else {
      throw std::runtime_error("Non symbol cannot be a tag");
    }
  };

  builders_["go"] = go_builder;

  auto let_builder = [](ObjectBuilder &builder, std::string &name,
                        SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto sym = std::get_if<Symbol>(&*it)) {
      auto name = sym->getName();
      it++;
      auto init = codeWalk(builder, *it);
      it++;
      auto body = codeWalk(builder, *it);
      return std::make_unique<Let>(std::move(name), std::move(init),
                                   std::move(body));
    } else {
      throw std::runtime_error("Non symbol cannot be a variable name");
    }
  };

  builders_["let"] = let_builder;
}

std::vector<std::string> parseArgList(SyntaxObject *syntax) {
  std::vector<std::string> res;
  if (!syntax)
    return res;

  auto view = Cell::ListView(syntax);
  for (auto &node : view) {
    if (auto sym = std::get_if<Symbol>(&node)) {
      res.emplace_back(sym->getName());
    } else {
      throw std::runtime_error("Not a valid arg name");
    }
  }
  return res;
}

std::vector<ObjPtr> lispListToVec(ObjectBuilder &builder,
                                  SyntaxObject *syntax) {
  std::vector<ObjPtr> res;
  if (!syntax)
    return res;
  auto view = Cell::ListView(syntax);
  for (auto &node : view) {
    res.emplace_back(codeWalk(builder, node));
  }
  return res;
}

ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax) {
  if (Number *number = std::get_if<Number>(&syntax)) {
    return std::make_unique<Number>(number->getValue());
  } else if (Symbol *symbol = std::get_if<Symbol>(&syntax)) {
    return std::make_unique<Variable>(symbol->getName());
  } else {
    Cell cell = std::get<Cell>(std::move(syntax));
    auto &fst = cell.get<0>();
    if (!fst)
      throw std::runtime_error(
          "Incorrect parsing, empty beginning of the cell");
    auto &fstObj = *fst;
    if (std::holds_alternative<Number>(fstObj)) {
      throw std::runtime_error("Incorrect form, cannot funcall a number");
    } else if (Symbol *symbol = std::get_if<Symbol>(&fstObj)) {
      auto name = symbol->getName();
      auto &body = cell.get<1>(); // can be nullptr
      if (body && !std::holds_alternative<Cell>(*body)) {
        throw std::runtime_error("Imporper syntax");
      }
      if (auto search = builder.builders_.find(name);
          search != builder.builders_.end()) {
        return search->second(builder, name, body.get());
      } else {
        auto callee = codeWalk(builder, fstObj);
        auto args = lispListToVec(builder, body.get());
        return std::make_unique<Call>(std::move(callee), std::move(args));
      }
    } else {
      // for now
      throw std::runtime_error("Lambdas not yet supported");
    }
  }
}

ObjPtr ir1LispTransform(std::unique_ptr<SyntaxObject> syntax) {
  ObjectBuilder builder;
  return codeWalk(builder, *syntax);
}