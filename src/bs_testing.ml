type mocha_callback = unit -> unit

(* mocha's function that wrap up tests *)
external describe: string -> (unit -> unit [@bs]) -> unit = "" [@@bs.val]

(* mocha's function that include all tests *)
external it: string -> (unit -> unit [@bs]) -> unit = "" [@@bs.val]
external itAsync: string -> (mocha_callback -> unit [@bs]) -> unit = "it" [@@bs.val]


module Assertion = struct

  external ok: Js.boolean -> unit = "ok" [@@bs.module "assert"]
  external eq: 'a -> 'a -> unit = "deepEqual" [@@bs.module "assert"]
  external neq: 'a -> 'a -> unit = "notDeepEqual" [@@bs.module "assert"]
  external strict_eq: 'a -> 'a -> unit = "deepStrictEqual" [@@bs.module "assert"]
  external strict_neq: 'a -> 'a -> unit = "notDeepStrictEqual" [@@bs.module "assert"]
  external fail: string -> unit = "fail" [@@bs.module "assert"]

end

type assertion =
  | Eq: 'a * 'a -> assertion
  | NotEq: 'a * 'a -> assertion
  | StrictEq: 'a * 'a -> assertion
  | NotStrictEq: 'a * 'a -> assertion
  | Ok: bool -> assertion
  | NotOk: bool -> assertion
  | Fail: string -> assertion

let assert_ok v = Ok v
let assert_not_ok v = NotOk v
let assert_eq a b = Eq (a, b)
let assert_neq a b = NotEq (a, b)
let assert_strict_eq a b = StrictEq (a, b)
let assert_not_strict_eq a b = NotStrictEq (a, b)
let assert_fail message = Fail (message)

let assertion_to_assert = function
  | Eq (a, b) -> Assertion.(eq a b)
  | NotEq (a, b) -> Assertion.(neq a b)
  | StrictEq (a, b) -> Assertion.(strict_eq a b)
  | NotStrictEq (a, b) -> Assertion.(strict_neq a b)
  | Ok v -> Assertion.(ok (Js.Boolean.to_js_boolean v))
  | NotOk v -> Assertion.(ok (not v |> Js.Boolean.to_js_boolean))
  | Fail v -> Assertion.(fail v)

type test =
  | Sync of (string * (unit -> assertion))
  | Async of (string * (unit -> assertion Js.Promise.t))


let suite name tests =
  describe name (fun [@bs]() ->
      List.iter (function
          | Sync (name, f) -> it name (fun [@bs]() -> assertion_to_assert (f ()))
          | Async (name, f) ->
             itAsync name (fun [@bs]cb ->
                 let promise = f () |> Js.Promise.then_ (fun assertion ->
                                           assertion_to_assert assertion |> Js.Promise.resolve
                                         )
                               |> Js.Promise.then_ (fun _ -> cb () |> Js.Promise.resolve) in
                 promise |> ignore
               )
        )
        tests
    )
