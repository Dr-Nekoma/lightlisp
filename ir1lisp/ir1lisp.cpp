#include "ir1lisp.h"
#include "compiler.h"

ObjectBuilder::ObjectBuilder() {
  auto if_builder = [](ObjectBuilder &builder, std::string &,
                       SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    auto cond = codeWalk(builder, *it);
    it++;
    auto thenClause = codeWalk(builder, *it);
    it++;
    auto elseClause = codeWalk(builder, *it);
    it++;
    assert(it == view.end());
    auto res = std::make_unique<If<NotExpanded>>(
        std::move(cond), std::move(thenClause), std::move(elseClause));
    return ExprPtr<NotExpanded>(std::move(res));
  };
  builders_["if"] = if_builder;

  auto setq_builder = [](ObjectBuilder &builder, std::string &,
                         SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto sym = it->get_if<Symbol>()) {
      it++;
      auto snd = codeWalk(builder, *it);
      auto res =
          std::make_unique<Setq<NotExpanded>>(std::move(*sym), std::move(snd));
      // assert(cdrCell->get<1>() == nullptr);
      return ExprPtr<NotExpanded>(std::move(res));
    }
    throw std::runtime_error("Cannnot setq what is not a variable");
  };

  builders_["setq"] = setq_builder;

  auto tagbody_builder = [](ObjectBuilder &builder, std::string &,
                            SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    std::vector<std::variant<IR1Expr, std::string>> body;
    for (auto &val : view) {
      if (auto sym = val.get_if<Symbol>())
        body.emplace_back(sym->getName());
      else
        body.emplace_back(codeWalk(builder, val));
    }
    return ExprPtr<NotExpanded>(
        std::make_unique<Goto<NotExpanded>>(std::move(body)));
  };

  builders_["tagbody"] = tagbody_builder;

  auto go_builder = [](ObjectBuilder &builder, std::string &name,
                       SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto sym = it->get_if<Symbol>()) {
      return ExprPtr<NotExpanded>(
          std::make_unique<Go<NotExpanded>>(std::move(sym->getName())));
    }
    throw std::runtime_error("Non symbol cannot be a tag");
  };

  builders_["go"] = go_builder;

  auto decl_builder = [](ObjectBuilder &builder, std::string &name,
                         SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto sym = it->get_if<Symbol>()) {
      it++;
      auto init = codeWalk(builder, *it);
      if (name == "def")
        return ExprPtr<NotExpanded>(std::make_unique<Def<NotExpanded>>(
            std::move(*sym), std::move(init)));
      it++;
      auto body = codeWalk(builder, *it);
      return ExprPtr<NotExpanded>(std::make_unique<Let<NotExpanded>>(
          std::move(*sym), std::move(init), std::move(body)));
    }
    throw std::runtime_error("Non symbol cannot be a variable name");
  };

  builders_["let"] = decl_builder;

  builders_["def"] = decl_builder;

  auto lambda_builder = [](ObjectBuilder &builder, std::string &,
                           SyntaxObject *syntax) -> IR1Expr {
    auto view = Cell::ListView(syntax);
    auto it = view.begin();
    if (auto cell = it->get_if<Cell>()) {
      auto argVec = parseArgList(cell->get<0>().get());
      it++;
      auto body = codeWalk(builder, *it);
      return ExprPtr<NotExpanded>(std::make_unique<Lambda<NotExpanded>>(
          std::move(argVec), std::move(body)));
    }
    throw std::runtime_error("Arg list is not a list");
  };

  builders_["lambda"] = lambda_builder;
}

std::vector<Symbol> parseArgList(SyntaxObject *syntax) {
  std::vector<Symbol> res;
  if (!syntax)
    return res;

  auto view = Cell::ListView(syntax);
  for (auto &node : view) {
    if (auto sym = node.get_if<Symbol>()) {
      res.emplace_back(*sym);
    } else {
      throw std::runtime_error("Not a valid arg name");
    }
  }
  return res;
}

std::vector<IR1Expr> lispListToVec(ObjectBuilder &builder,
                                   SyntaxObject *syntax) {
  std::vector<IR1Expr> res;
  if (!syntax)
    return res;
  auto view = Cell::ListView(syntax);
  for (auto &node : view) {
    res.emplace_back(codeWalk(builder, node));
  }
  return res;
}

IR1Expr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax) {
  if (auto number = syntax.get_if<Number>()) {
    return std::make_unique<Number>(std::move(*number));
  } else if (auto symbol = syntax.get_if<Symbol>()) {
    return std::make_unique<Symbol>(std::move(*symbol));
  } else {
    Cell &cell = syntax.get<Cell>();
    auto &fst = cell.get<0>();
    if (!fst)
      throw std::runtime_error(
          "Incorrect parsing, empty beginning of the cell");
    auto &fstObj = *fst;
    if (fstObj.is<Number>()) {
      throw std::runtime_error("Incorrect form, cannot funcall a number");
    } else if (auto symbol = fstObj.get_if<Symbol>()) {
      auto name = symbol->getName();
      auto &body = cell.get<1>(); // can be nullptr
      if (body && !body->is<Cell>()) {
        throw std::runtime_error("Imporper syntax");
      }
      if (auto search = builder.builders_.find(name);
          search != builder.builders_.end()) {
        return search->second(builder, name, body.get());
      } else {
        auto callee = codeWalk(builder, fstObj);
        auto args = lispListToVec(builder, body.get());
        auto res = std::make_unique<Call<NotExpanded>>(std::move(callee),
                                                       std::move(args));
        return ExprPtr<NotExpanded>(std::move(res));
      }
    } else {
      auto &body = cell.get<1>(); // can be nullptr
      if (body && !body->is<Cell>()) {
        throw std::runtime_error("Imporper syntax");
      }
      auto callee = codeWalk(builder, fstObj);
      auto args = lispListToVec(builder, body.get());
      return ExprPtr<NotExpanded>(std::make_unique<Call<NotExpanded>>(
          std::move(callee), std::move(args)));
    }
  }
}

IR1Expr ir1LispTransform(std::unique_ptr<SyntaxObject> syntax) {
  static ObjectBuilder builder;
  return codeWalk(builder, *syntax);
}
