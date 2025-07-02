#include "ir2lisp.h"
#include "compiler.h"

FinalExpr ir2LispTransform(IR1Expr expr) {
  if (auto expr_ptr = std::get_if<ExprPtr<NotExpanded>>(&expr)) {
    auto &not_expanded = *expr_ptr;

    if (auto atom_ptr = std::get_if<AtomPtr>(&not_expanded)) {
      return std::move(*atom_ptr);
    }

    auto &spf_ptr = std::get<SpFPtr<NotExpanded>>(not_expanded);

    // Check each possible type explicitly
    if (auto let_ptr =
            std::get_if<std::unique_ptr<Let<NotExpanded>>>(&spf_ptr)) {
      if (!*let_ptr)
        throw std::runtime_error("Null Let pointer");
      return (*let_ptr)->expand();
    }

    if (auto def_ptr =
            std::get_if<std::unique_ptr<Def<NotExpanded>>>(&spf_ptr)) {
      if (!*def_ptr)
        throw std::runtime_error("Null Def pointer");
      return (*def_ptr)->expand();
    }

    if (auto if_ptr = std::get_if<std::unique_ptr<If<NotExpanded>>>(&spf_ptr)) {
      if (!*if_ptr)
        throw std::runtime_error("Null If pointer");
      return (*if_ptr)->expand();
    }

    if (auto goto_ptr =
            std::get_if<std::unique_ptr<Goto<NotExpanded>>>(&spf_ptr)) {
      if (!*goto_ptr)
        throw std::runtime_error("Null Goto pointer");
      return (*goto_ptr)->expand();
    }

    if (auto go_ptr = std::get_if<std::unique_ptr<Go<NotExpanded>>>(&spf_ptr)) {
      if (!*go_ptr)
        throw std::runtime_error("Null Go pointer");
      return (*go_ptr)->expand();
    }

    if (auto setq_ptr =
            std::get_if<std::unique_ptr<Setq<NotExpanded>>>(&spf_ptr)) {
      if (!*setq_ptr)
        throw std::runtime_error("Null Setq pointer");
      return (*setq_ptr)->expand();
    }

    if (auto lambda_ptr =
            std::get_if<std::unique_ptr<Lambda<NotExpanded>>>(&spf_ptr)) {
      if (!*lambda_ptr)
        throw std::runtime_error("Null Lambda pointer");
      return (*lambda_ptr)->expand();
    }

    if (auto call_ptr =
            std::get_if<std::unique_ptr<Call<NotExpanded>>>(&spf_ptr)) {
      if (!*call_ptr)
        throw std::runtime_error("Null Call pointer");
      return (*call_ptr)->expand();
    }

    throw std::runtime_error("Unknown SpFPtr variant state encountered");
  } else if (auto macro_ptr = std::get_if<MacroPtr>(&expr)) {
    return (*macro_ptr)->expand();
  } else {
    throw std::runtime_error("Invalid IR1Expr variant state.");
  }
}