#include "ir2lisp.h"
#include "compiler.h"

FinalExpr ir2LispTransform(IR1Expr expr) {
  if (auto expr_ptr = std::get_if<ExprPtr<NotExpanded>>(&expr)) {
    auto &not_expanded = *expr_ptr;

    if (auto atom_ptr = std::get_if<AtomPtr>(&not_expanded)) {
      return std::move(*atom_ptr);
    }

    auto &spf_ptr = std::get<SpFPtr<NotExpanded>>(not_expanded);

    if (auto let_ptr =
            std::get_if<std::unique_ptr<Let<NotExpanded>>>(&spf_ptr)) {
      auto &node = *let_ptr;
      auto init_exp = ir2LispTransform(std::move(node->init()));
      auto body_exp = ir2LispTransform(std::move(node->body()));
      return FinalExpr{std::make_unique<Let<Expanded>>(
          std::move(node->var()), std::move(init_exp), std::move(body_exp))};
    }

    if (auto def_ptr =
            std::get_if<std::unique_ptr<Def<NotExpanded>>>(&spf_ptr)) {
      auto &node = *def_ptr;
      auto init_exp = ir2LispTransform(std::move(node->init()));
      return FinalExpr{std::make_unique<Def<Expanded>>(std::move(node->var()),
                                                       std::move(init_exp))};
    }

    if (auto if_ptr = std::get_if<std::unique_ptr<If<NotExpanded>>>(&spf_ptr)) {
      auto &node = *if_ptr;
      auto cond_exp = ir2LispTransform(std::move(node->cond()));
      auto then_exp = ir2LispTransform(std::move(node->thenBranch()));
      auto else_exp = ir2LispTransform(std::move(node->elseBranch()));
      return FinalExpr{std::make_unique<If<Expanded>>(
          std::move(cond_exp), std::move(then_exp), std::move(else_exp))};
    }

    if (auto goto_ptr =
            std::get_if<std::unique_ptr<Goto<NotExpanded>>>(&spf_ptr)) {
      auto &node = *goto_ptr;
      std::vector<std::variant<FinalExpr, std::string>> body_exp;
      body_exp.reserve(node->body().size());
      for (auto &elem : node->body()) {
        if (auto irp = std::get_if<IR1Expr>(&elem)) {
          body_exp.emplace_back(ir2LispTransform(std::move(*irp)));
        } else {
          body_exp.emplace_back(std::get<std::string>(elem));
        }
      }
      return FinalExpr{std::make_unique<Goto<Expanded>>(std::move(body_exp))};
    }

    if (auto go_ptr = std::get_if<std::unique_ptr<Go<NotExpanded>>>(&spf_ptr)) {
      auto &node = *go_ptr;
      return FinalExpr{std::make_unique<Go<Expanded>>(std::move(node->tag()))};
    }

    if (auto setq_ptr =
            std::get_if<std::unique_ptr<Setq<NotExpanded>>>(&spf_ptr)) {
      auto &node = *setq_ptr;
      auto val_exp = ir2LispTransform(std::move(node->newval()));
      return FinalExpr{std::make_unique<Setq<Expanded>>(std::move(node->var()),
                                                        std::move(val_exp))};
    }

    if (auto lambda_ptr =
            std::get_if<std::unique_ptr<Lambda<NotExpanded>>>(&spf_ptr)) {
      auto &node = *lambda_ptr;
      auto body_exp = ir2LispTransform(std::move(node->body()));
      return FinalExpr{std::make_unique<Lambda<Expanded>>(
          std::move(node->args()), std::move(body_exp))};
    }

    if (auto call_ptr =
            std::get_if<std::unique_ptr<Call<NotExpanded>>>(&spf_ptr)) {
      auto &node = *call_ptr;
      auto callee_exp = ir2LispTransform(std::move(node->callee()));
      std::vector<FinalExpr> args_exp;
      args_exp.reserve(node->args().size());
      for (auto &e : node->args()) {
        args_exp.push_back(ir2LispTransform(std::move(e)));
      }
      return FinalExpr{std::make_unique<Call<Expanded>>(std::move(callee_exp),
                                                        std::move(args_exp))};
    }

    throw std::runtime_error("Unknown SpFPtr variant state encountered");
  } else if (auto macro_ptr = std::get_if<MacroPtr>(&expr)) {
    auto expanded = (*macro_ptr)->expand();

    while (std::holds_alternative<MacroPtr>(expanded)) {
      auto &mp = std::get<MacroPtr>(expanded);
      expanded = mp->expand();
    }

    return ir2LispTransform(std::move(expanded));
  } else {
    throw std::runtime_error("Invalid IR1Expr variant state.");
  }
}