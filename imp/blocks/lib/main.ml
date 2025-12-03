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
  
  | Var x -> (
    match (topenv st) x with
    | BVar l | IVar l -> (getmem st) l
  )
  
  | Const n -> Int n

  | Not b -> (
    match eval_expr st b with
    | Bool b1 -> Bool (not b1)
    | _ -> raise(TypeError "Not")
  )
  
  | And(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> raise(TypeError "And")
  )
  
  | Or(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> raise(TypeError "Or")
  )

  | Add(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int (n1 + n2)
    | _ -> raise(TypeError "Add")
  )

  | Sub(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int (n1 - n2)
    | _ -> raise(TypeError "Sub")
  )

  | Mul(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int (n1 * n2)
    | _ -> raise(TypeError "Mul")
  )

  | Eq(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool (n1 = n2)
    | _ -> raise(TypeError "Eq")
  )

  | Leq(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool (n1 <= n2)
    | _ -> raise(TypeError "Leq")
  )
;;


(******************************************************************************)
(*                     Small-step semantics of expressions                    *)
(******************************************************************************)

exception NoRuleApplies

(* 
###Esempi di lista di ambienti###
        z = l5
        y = l2 
        x = l4
        Bottom
        ___________
        z = l3
        y = l2 
        x = l1
        Bottom
        ___________
        y = l2
        x = l1
        Bottom
        _________
        Bottom
*)

let bindvar (st : state) (x : ide) (v : memval) : state =
    let firstEnv = (topenv st) in
    match (firstEnv x, v) with              (*matcha firstEnv x = envval = BVar of loc | IVar of loc && v = memval = Bool of bool | Int of int*)
    | IVar l, Int _ | BVar l, Bool _ ->     (*Se trova la locazione di memoria allora assegna il valore alla locazione corrispondente*)
        let mem' = bind_mem (getmem st) l v in
        setmem st mem'
    | IVar _, Bool _ | BVar _, Int _ -> raise(TypeError "Non puoi assegnare Bool a Int o viceversa")
;;

(*
var x;     --> newloc()
*)

let rec eval_decl_rec (en : env) (dl : decl list) (i : loc) : (env * loc) =
    match dl with
    | [] -> (en, i)
    | h :: t -> (
        let en' = (
            match h with
            | IntVar x -> bind_env en x (IVar i)
            | BoolVar x -> bind_env en x (BVar i))
        in
        eval_decl_rec en' t (i+1)
    )
;;

let eval_decl (st : state) (dl : decl list) : state =
    let (en', i) = eval_decl_rec (topenv st) dl (getloc st) in
    make_state (en' :: (getenv st)) (getmem st) i
;;

let rec trace1 (ev : conf) : conf =
    match ev with
    | St _ -> raise NoRuleApplies
    | Cmd (c, st) -> (
            match c with
            | Skip -> St st
            | Assign (s, e) -> let v = eval_expr st e in St(bindvar st s v)
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
                | _ -> raise (TypeError "While"))
            | Decl (dl, c1) -> Cmd(Block(c1), eval_decl st dl)
            | Block (command) -> (
                match trace1 (Cmd(command, st)) with
                | St st' -> St (setenv st' (popenv st'))
                | Cmd(c', st') -> Cmd(Block(c'), st')
            )
        )
;;

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

let trace n t = trace_rec n (Cmd(t,state0))