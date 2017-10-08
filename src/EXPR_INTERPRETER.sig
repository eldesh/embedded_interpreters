
signature EXPR_INTERPRETER =
sig
  type Exp = Expr.Exp
  type U = Univ.U

  (* bound variable names *)
  type staticenv  = string list
  (* bound variable values *)
  type dynamicenv = U list

  (**
   * provides Expr -> U relation
   *
   * interpreter provides a semantics of Expr DSL
   * as a denotational semantics
   *)
  val interpret : Exp * staticenv -> dynamicenv -> U

  (* evaluate expr on empty context *)
  val interpretclosed : Exp -> U
end

