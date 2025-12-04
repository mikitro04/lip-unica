open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

