exception Unexpected

module Sx = struct
  type t =
    | Car of t
    | Cdr of t
    | Cons of t * t
    | Symbol of string
end

module type VAR = sig
  type t
  val intro : t -> t
  val fmt : t -> string
  val equal : t * t -> bool
  val compare : t -> t -> int
end

module Var : VAR = struct
  type t = string
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
end

module IdentEnv = Map.Make(Var)
module Env = struct
  type t = Sx.t IdentEnv.t

  let empty : t = IdentEnv.empty
end

module Evaluator = struct
  let rec walk (env : Env.t) (s_x : Sx.t) : Sx.t =
    match s_x with
    | Sx.Car(cons) ->
       let cons' = walk env cons in
       (match cons' with
        | Sx.Cons(v, _) -> v
        | _ -> raise Unexpected)
    | Sx.Cdr(cons) ->
       let cons' = walk env cons in
       (match cons' with
        | Sx.Cons(_, v) -> v
        | _ -> raise Unexpected)
    | Sx.Cons(v1, v2) ->
       let v1' = walk env v1 in
       let v2' = walk env v2 in
       Sx.Cons(v1', v2')
    | Sx.Symbol(str) -> s_x

  let eval (s_x : Sx.t) : Sx.t = walk Env.empty s_x
end

module Test = struct
  let s_x_list = [
      Sx.Car(Sx.Cons(Sx.Symbol("hello"), Sx.Symbol("world")));
      Sx.Car(Sx.Cons(Sx.Symbol("hello"), Sx.Symbol("world")));
    ]

  let test () : Sx.t list = List.map (fun x -> Evaluator.eval x) s_x_list
end

let result = Test.test ()
