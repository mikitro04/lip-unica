open Ast
exception NoRuleApplies

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast
    

(* ---BIG STEP SEMANTIC--- *)
let rec eval (x : boolExpr) : bool = 
    match x with
    | True -> true
    | False -> false
    | If (b1, b2, b3) -> if eval b1 then eval b2 else eval b3
;;


(* ---SMALL STEP SEMANTIC--- *)
let rec trace1 (e : boolExpr) : boolExpr = 
    match e with
    | If(True, t, _) -> t
    | If(False, _, f) -> f
    | If(c, t, f) -> If((trace1 c), t, f)    
    | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e  
        in e::(trace e')
    with NoRuleApplies -> [e]
;;