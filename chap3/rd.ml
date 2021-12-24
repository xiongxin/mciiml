(* recursive descent *)

type token =
  | IF
  | THEN
  | ELSE
  | BEGIN
  | END
  | PRINT
  | SEMI
  | NUM
  | EQ

let getToken () = IF
let error () = failwith "ERROR"
let tok = ref (getToken ())
let advance () = tok := getToken ()
let eat t = if !tok = t then advance () else error ()

let rec s () =
  match !tok with
  | IF ->
    eat IF;
    e ();
    eat THEN;
    s ();
    eat ELSE;
    s ()
  | BEGIN ->
    eat BEGIN;
    s ();
    l ()
  | PRINT ->
    eat PRINT;
    e ()
  | _ -> failwith "S()"

and l () =
  match !tok with
  | END -> eat END
  | SEMI ->
    eat SEMI;
    s ();
    l ()
  | _ -> failwith "S()"

and e () =
  eat NUM;
  eat EQ;
  eat NUM
;;
