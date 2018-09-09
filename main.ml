type ident = string

module Sx = struct
  type t =
    | Car of t
    | Cdr of t
    | Cons of t * t
    | App of t * t
    | Lambda of ident * t
    | Define of ident * t
    | Var of ident
    | Symbol of string
    | Pair of t * t (* Unavailable in User input *)
end

module IdentEnv = Map.Make(String)

module Env = struct
  type t = Sx.t IdentEnv.t
  let empty : t = IdentEnv.empty
end

module Evaluator = struct
  let rec eval (env : Env.t) (s_x : Sx.t) : Sx.t * Env.t =
    match s_x with
    | Sx.Car(cons) ->
       let (pair, _) = eval env cons in
       (match pair with
        | Sx.Pair(e, _) -> (e, env)
        | _ -> failwith "Unmatch type with Sx.Pair in Car")
    | Sx.Cdr(cons) ->
       let (pair, _) = eval env cons in
       (match pair with
        | Sx.Pair(_, e) -> (e, env)
        | _ -> failwith "Unmatch type with Sx.Pair in Cdr")
    | Sx.Cons(e1, e2) ->
       let (e1', _) = eval env e1 in
       let (e2', _) = eval env e2 in
       (Sx.Pair(e1', e2'), env)
    | Sx.App(e, arg) ->
       let (e', _) = eval env e in
       let (arg', _) = eval env arg in
       (match e' with
        | Sx.Lambda(x, body) ->
           let env' = IdentEnv.add x arg' env in
           eval env' body
        | _ -> failwith "Unmatch type with Sx.Lambda")
    | Sx.Lambda(x, body) -> (s_x, env)
    | Sx.Define(x, v) ->
       let env' = IdentEnv.add x v env in
       (Sx.Var(x), env')
    | Sx.Var(x) -> (IdentEnv.find x env, env)
    | Sx.Symbol(str) -> (s_x, env)
    | Sx.Pair(v1, v2) -> (s_x, env)
end

module Test = struct
  let test (program : Sx.t list) : Sx.t list =
    let rec walk (env: Env.t) (program : Sx.t list) : Sx.t list =
      match program with
      | hd :: tl ->
         let (s_x, env') = Evaluator.eval env hd in
         (s_x) :: (walk env' tl)
      | [] -> []
    in
    walk Env.empty program
end

let program =
  [
    Sx.Car(Sx.Cons(Sx.Symbol("hello"), Sx.Symbol("world")));
    Sx.Cdr(Sx.Cons(Sx.Symbol("hello"), Sx.Symbol("world")));
    Sx.Define("x", Sx.Lambda("y", Sx.Var("y")));
    Sx.App(Sx.Var("x"), Sx.Cdr(Sx.Cons(Sx.Symbol("hello"), Sx.Symbol("world"))));
  ]

let result = Test.test program
