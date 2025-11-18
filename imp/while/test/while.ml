open WhileLib.Types
open WhileLib.Prettyprint       
open WhileLib.Main

let test_trace (cmd,n_steps,var,exp_val) =
  cmd
  |> parse
  |> fun c -> last (trace n_steps c)
  |> fun t -> match t with
    St s -> s var = exp_val
  | Cmd(_,s) -> s var = exp_val


let%test "test_trace1" = test_trace
    ("x:=0", 1, "x", Nat 0)

let%test "test_trace2" = test_trace
    ("x:=0; x:=x+1", 2, "x", Nat 1)

let%test "test_trace3" = test_trace
    ("x:=0; y:=x+1; x:=y+1", 3, "x", Nat 2)
    
let%test "test_trace4" = test_trace
    ("x:=0; if x=0 then y:=1 else y:=2", 3, "y", Nat 1)

let%test "test_trace5" = test_trace
    ("x:=1; if x=0 then y:=1 else y:=2", 3, "y", Nat 2)

let%test "test_trace6" = test_trace
  ("x:=3; y:=2; r:=0; while 1<=y do (r:=r+x; y:=y-1)", 10, "r", Nat 6)

let%test "test_trace7" = test_trace
  ("x:=3; while 0<=x and not 0=x do x:=x-1; x:=5", 10, "x", Nat 5)

let%test "test_trace8" = test_trace
  ("x:=5; y:=3; if x<=y then min:=x else min:=y", 10, "min", Nat 3)

let%test "test_trace9" = test_trace
  ("x:=1; y:=2; z:=3; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)

let%test "test_trace10" = test_trace
    ("x:=2; y:=1; z:=3; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)

let%test "test_trace11" = test_trace
    ("x:=2; y:=3; z:=1; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)





(**********************************************************************
 file test : leggi un programma da un file e testalo
 **********************************************************************)

(* Funzione di utility per leggere il contenuto di un file in una stringa *)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* La funzione di test vera e propria *)
let test_file (filename, n_steps, var, exp_val) =
  (* Leggi il contenuto del file 'filename' *)
  let cmd = read_file filename in
  
  (* Il resto della logica è identico alla tua funzione test_trace *)
  cmd
  |> parse
  |> fun c -> last (trace n_steps c)
  |> fun t -> match t with
    St s -> s var = exp_val
  | Cmd(_,s) -> s var = exp_val

(* Il nostro nuovo test che usa il file 'min' *)
let%test "test_trace12_from_file" = test_file
    ("min", 20, "min", Nat 1)

let%test "test_trace13_from_file" = test_file
    ("min2", 20, "min", Nat 1)

let%test "test_trace14_from_file" = test_file
    ("fib", 100, "a", Nat 13)

let%test "test_trace15_from_file" = test_file
    ("fact", 100, "f", Nat 120)

let%test "test_trace16_from_file" = test_file
    ("euclid", 100, "a", Nat 12)

(* Dentro ogni test che legge da file abbiamo il nome del file, quanti passi di esecuzione deve fare, 
la variabile del file di cui controllare il valore e il valore aspettato *)

    
