open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

(*
  | True
  | False
  | Var of string
  | Const of int     
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
*)

let rec eval_expr (st : state) (e : expr) : exprval =
    match e with
    | True -> Bool true
    | False -> Bool false
    | Var x -> st x
    | Const n -> Nat n
    | Not b -> (
        match eval_expr st b with
            | Bool b2 -> Bool (not b2)
            | _ -> failwith("La regola non è applicabile")
        )
    | And (e1, e2) -> (
        let b1 = eval_expr st e1 in
        let b2 = eval_expr st e2 in
        match (b1, b2) with
            | (Bool v1, Bool v2) -> Bool (v1 && v2)
            | _ -> failwith("La regola non è applicabile")
        )
    | Or (e1, e2) -> (
        let b1 = eval_expr st e1 in
        let b2 = eval_expr st e2 in
        match (b1, b2) with
            | (Bool v1, Bool v2) -> Bool (v1 || v2)
            | _ -> failwith("La regola non è applicabile")
        )
    | Add (e1, e2) -> (
        let n1 = eval_expr st e1 in
        let n2 = eval_expr st e2 in
        match (n1, n2) with
            | (Nat v1, Nat v2) -> Nat (v1 + v2)
            | _ -> failwith("La regola non è applicabile")
        )
    | Sub (e1, e2) -> (
        let n1 = eval_expr st e1 in
        let n2 = eval_expr st e2 in
        match (n1, n2) with
            | (Nat v1, Nat v2) -> Nat (v1 - v2)
            | _ -> failwith("La regola non è applicabile")
        )
    | Mul (e1, e2) -> (
        let n1 = eval_expr st e1 in
        let n2 = eval_expr st e2 in
        match (n1, n2) with
            | (Nat v1, Nat v2) -> Nat (v1 * v2)
            | _ -> failwith("La regola non è applicabile")
        )
    | Eq (e1, e2) -> (
        let e1' = eval_expr st e1 in
        let e2' = eval_expr st e2 in
        match (e1', e2') with
            | (v1, v2) -> Bool (v1 = v2)
        )
    | Leq (e1, e2) -> (
        let n1 = eval_expr st e1 in
        let n2 = eval_expr st e2 in
        match (n1, n2) with
            | (Nat v1, Nat v2) -> Bool (v1 <= v2)
            | _ -> failwith("La regola non è applicabile")
        )
;;


(******************************************************************************)
(*                     Small-step semantics of expressions                    *)
(******************************************************************************)

exception UnboundVar of string
exception NoRuleApplies

let bot = fun x -> raise (UnboundVar x)

(*bind : state -> ide -> exprval -> state*)
let bind (st : state) (x : ide) (v : exprval) : state = fun y -> if x = y then v else st y;;


(*
  conf = St of state | Cmd of cmd * state

  cmd
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
*)


let rec trace1 (ev : conf) : conf =
    match ev with
    | St _ -> raise NoRuleApplies
    | Cmd (c, st) -> (
            match c with
            | Skip -> St st
            | Assign (s, e) -> let v = eval_expr st e in St(bind st s v)
            | Seq (c1, c2) -> (
                let c1' = trace1 (Cmd(c1, st)) in
                match c1' with
                | St st' -> Cmd(c2, st')
                | Cmd (c1', st') -> Cmd (Seq (c1', c2), st'))

            | If (e, t, f) -> (
                let e' = eval_expr st e in
                match e' with
                | Bool true -> Cmd (t, st)
                | Bool false -> Cmd (f, st)
                | _ -> raise (TypeError "If"))

            | While (e, c1) -> (
                let e' = eval_expr st e in
                match e' with
                | Bool false -> St st
                | Bool true -> Cmd (Seq (c1, While (e, c1)), st)
                | _ -> raise (TypeError "While")
            )
        )
;;


(*let rec trace (n : int) (c : cmd) : conf list =
    if n <= 0 then []
    else
        (trace1 (Cmd(c, bot))) :: (trace (n-1) c)
;;*)

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> cmd -> conf list

 Usage: trace n t performs n steps of the small-step semantics
 **********************************************************************)

let trace n t = trace_rec n (Cmd(t,bot))
