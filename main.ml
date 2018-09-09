type ident = string
module IdentEnv = Map.Make(String)

module Sx = struct
  type t =
    | Car of t
    | Cdr of t
    | Cons of t * t
    | App of t * t
    | Callcc of t
    | Lambda of ident * t
    | Define of ident * t
    | Var of ident
    | String of string
    | LambdaVal of ident * env * t (* Unavailable in User input *)
    | ContVal of env * (t * env -> t * env) (* Unavailable in User input *)
    | PairVal of t * t (* Unavailable in User input *)
   and env = t IdentEnv.t
end

module Evaluator = struct
  let rec eval (env : Sx.t IdentEnv.t) (s_x : Sx.t) (cont : Sx.t * Sx.t IdentEnv.t -> Sx.t * Sx.t IdentEnv.t ) : Sx.t * Sx.t IdentEnv.t =
    let merge_env env env' =
      IdentEnv.merge (fun k xo yo -> match xo, yo with
                                     | Some x, Some y -> xo
                                     | None, y -> yo
                                     | x, None -> xo
                     ) env env' in
    match s_x with
    | Sx.Car(cons) ->
       eval env cons (fun (pair, _) ->
              match pair with
              | Sx.PairVal(e, _) -> cont (e, env)
              | _ -> failwith "Unmatch type with Sx.Pair in Car")
    | Sx.Cdr(cons) ->
       eval env cons (fun (pair, _) ->
              match pair with
              | Sx.PairVal(_, e) -> cont (e, env)
              | _ -> failwith "Unmatch type with Sx.Pair in Car")
    | Sx.Cons(e1, e2) ->
       eval env e1 (fun (e1', _) ->
              eval env e2 (fun (e2', _) ->
                     cont (Sx.PairVal(e1', e2'), env)))
    | Sx.App(e, arg) ->
       eval env e (fun (e', _) ->
              eval env arg (fun (arg', _) ->
                     (match e' with
                      | Sx.LambdaVal(x, env', body) ->
                         let env' = IdentEnv.add x arg' env' in
                         eval (merge_env env env') body cont
                      | Sx.ContVal(env', body) ->
                         body (arg', (merge_env env env'))
                      | _ -> failwith "Unmatch type with Sx.LambdaVal or Sx.ContVal in App")))
    | Sx.Callcc(e) ->
       eval env e (fun (e', _) ->
              (match e' with
               | Sx.LambdaVal(x, env', body) ->
                  let env' = IdentEnv.add x (Sx.ContVal(env, cont)) env in
                  eval env' body cont
               | _ -> failwith "Unmatch type with Sx.Lambda in Callcc"))
    | Sx.Lambda(x, body) -> cont (Sx.LambdaVal(x, env, body), env)
    | Sx.Define(x, v) ->
       eval env v (fun (v', _) ->
              let env' = IdentEnv.add x v' env in
              cont (Sx.Var(x), env'))
    | Sx.Var(x) -> cont (IdentEnv.find x env, env)
    | Sx.String(str) -> cont (s_x, env)
    | _ -> failwith "Unexpected expression by User Program"
end

module Test = struct
  let test (program : Sx.t list) : Sx.t list =
    let rec walk (env: Sx.t IdentEnv.t) (program : Sx.t list) : Sx.t list =
      match program with
      | hd :: tl ->
         let (s_x, env') = Evaluator.eval env hd (fun x -> x) in
         (s_x) :: (walk env' tl)
      | [] -> []
    in
    walk IdentEnv.empty program
end

let program =
  [
    Sx.Car(Sx.Cons(Sx.String("hello"), Sx.String("world")));
    Sx.Cdr(Sx.Cons(Sx.String("hello"), Sx.String("world")));
    Sx.Define("x", Sx.Lambda("y", Sx.Cdr(Sx.Var("y"))));
    Sx.App(Sx.Var("x"), Sx.Cons(Sx.String("hello"), Sx.String("world")));
  ]

let callcc_program =
  [
    Sx.Cons(Sx.Callcc(Sx.Lambda("c", Sx.String("hello"))), Sx.String("world"));
    Sx.Cons(Sx.Callcc(Sx.Lambda("c", Sx.Cons(Sx.String("hello"), Sx.App(Sx.Var("c"), Sx.String("HELLO"))))), Sx.String("world"));
  ]

let loop_program =
  [
    Sx.Define("x", Sx.Lambda("g", Sx.App(Sx.Var("y"), Sx.Var("g"))));
    Sx.Define("y", Sx.Lambda("g", Sx.App(Sx.Var("x"), Sx.Var("g"))));
    Sx.App(Sx.Var("x"), Sx.String("loop"));
  ]

let result = Test.test program
let loop_result = Test.test loop_program
let callcc_result = Test.test callcc_program
