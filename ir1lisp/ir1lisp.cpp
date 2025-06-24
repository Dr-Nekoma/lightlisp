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

  auto setq_builder = [](ObjectBuilder &builder, std::string &,
                         SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (std::holds_alternative<Symbol>(*it)) {
      auto genericVar = codeWalk(builder, *it);
      if (auto varPtr = dynamic_cast<Symbol *>(genericVar.get())) {
        genericVar.release();
        auto var = *varPtr;
        it++;
        auto snd = codeWalk(builder, *it);
        // assert(cdrCell->get<1>() == nullptr);
        return std::make_unique<Setq>(std::move(var), std::move(snd));
      }
      throw std::runtime_error("Cannnot setq what is not a variable");
    }
    throw std::runtime_error("Cannnot setq what is not a variable");
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

  auto decl_builder = [](ObjectBuilder &builder, std::string &name,
                         SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (std::holds_alternative<Symbol>(*it)) {
      auto genericVar = codeWalk(builder, *it);
      if (auto varPtr = dynamic_cast<Symbol *>(genericVar.get())) {
        auto var = *varPtr;
        genericVar.release();
        it++;
        auto init = codeWalk(builder, *it);
        if (name == "def") {
          return std::make_unique<Def>(std::move(var), std::move(init));
        }
        it++;
        auto body = codeWalk(builder, *it);

        return std::make_unique<Let>(std::move(var), std::move(init),
                                     std::move(body));
      } else {
        throw std::runtime_error("Declaration has to be for variable");
      }
    } else {
      throw std::runtime_error("Non symbol cannot be a variable name");
    }
  };

  builders_["let"] = decl_builder;

  builders_["def"] = decl_builder;

  auto lambda_builder = [](ObjectBuilder &builder, std::string &,
                           SyntaxObject *syntax) -> ObjPtr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    auto maybeCell = it.getCell();
    if (auto cell = std::get_if<Cell>(maybeCell)) {
      auto argVec = parseArgList(cell->get<0>().get());
      it++;
      auto body = codeWalk(builder, *it);
      return std::make_unique<Lambda>(std::move(argVec), std::move(body));
    } else {
      throw std::runtime_error("Arg list is not a list");
    }
  };

  builders_["lambda"] = lambda_builder;
}

std::vector<Symbol> parseArgList(SyntaxObject *syntax) {
  std::vector<Symbol> res;
  if (!syntax)
    return res;

  auto view = Cell::ListView(syntax);
  for (auto &node : view) {
    if (auto sym = std::get_if<Symbol>(&node)) {
      res.emplace_back(*sym);
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
    return std::make_unique<Number>(std::move(*number));
  } else if (Symbol *symbol = std::get_if<Symbol>(&syntax)) {
    return std::make_unique<Symbol>(std::move(*symbol));
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
      // should be now
      auto &body = cell.get<1>(); // can be nullptr
      if (body && !std::holds_alternative<Cell>(*body)) {
        throw std::runtime_error("Imporper syntax");
      }
      auto callee = codeWalk(builder, fstObj);
      auto args = lispListToVec(builder, body.get());
      return std::make_unique<Call>(std::move(callee), std::move(args));
    }
  }
}

ObjPtr ir1LispTransform(std::unique_ptr<SyntaxObject> syntax) {
  ObjectBuilder builder;
  return codeWalk(builder, *syntax);
}