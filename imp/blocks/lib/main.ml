open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf


(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let rec eval_expr (st : state) (e : expr) : memval =
    match e with
    | True -> Bool true
    | False -> Bool false
    | Var x -> 
        match (topenv st) x with
        | BVar l | IVar l -> (getmem st) l
    | Const n -> Int n
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

exception NoRuleApplies

let bindvar (st : state) (x : ide) (v : memval) : state =
    let firstEnv = (topenv st) in
    match (env x, v) with           (*matcha env x = envval = BVar of loc | IVar of loc && v = memval = Bool of bool | Int of int*)
    | IVar l, Int _ | BVar l, Bool _ -> 
        let mem' = bind_mem (getmem st) l v in
        setmem st mem'
    | IVar _, Bool _ | BVar _, Int _ -> raise(TypeError "Non puoi assegnare Bool a Int o viceversa")
;;