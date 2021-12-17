type id = string

type binop =
  | Plus
  | Minus
  | Times
  | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

type table = (id * int) list

let rec maxargs : stm -> int = function
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e1) -> maxexp e1
  | PrintStm l -> List.length l

and maxexp : exp -> int = function
  | EseqExp (s1, e1) -> max (maxargs s1) (maxexp e1)
  | _ -> 0
;;

let rec interp (prog : stm) : table = interpStm prog []

and interpStm (s : stm) (t : table) : table =
  match s with
  | CompoundStm (s1, s2) -> interpStm s2 (interpStm s1 t)
  | AssignStm (i, e) ->
    let r, t1 = interpExp e t in
    (i, r) :: t1
  | PrintStm l -> snd (interpList l t [])

and interpExp (e : exp) (t : table) : int * table =
  match e with
  | IdExp i -> List.assoc i t, t
  | NumExp n -> n, t
  | OpExp (e1, b1, e2) -> eval e1 b1 e2 t
  | EseqExp (s, e) -> interpExp e (interpStm s t)

and interpList (l : exp list) (t : table) (il : int list) : int list * table =
  match l with
  | [] ->
    printHelper il;
    il, t
  | hd :: tl ->
    let a, b = interpExp hd t in
    interpList tl b (il @ [ a ])

and printHelper (il : int list) =
  match il with
  | [] -> ()
  | [ hd ] -> Printf.printf "%d\n" hd
  | hd :: tl ->
    Printf.printf "%d, " hd;
    printHelper tl

and eval (e1 : exp) (b : binop) (e2 : exp) (t : table) : int * table =
  let v1, _ = interpExp e1 t in
  let v2, t3 = interpExp e2 t in
  match b with
  | Plus -> v1 + v2, t3
  | Minus -> v1 - v2, t3
  | Times -> v1 * v2, t3
  | Div -> v1 / v2, t3
;;

let () =
  let prog =
    CompoundStm
      ( AssignStm ("a", NumExp 0)
      , PrintStm
          [ NumExp 5
          ; EseqExp (PrintStm [ IdExp "a"; NumExp 3 ], OpExp (IdExp "a", Plus, NumExp 1))
          ; EseqExp (AssignStm ("a", NumExp 7), IdExp "a")
          ] )
  in
  Format.printf "maxargs prog = %d\n" (maxargs prog)
;;
