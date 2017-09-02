type mocha_callback = unit -> unit

(* mocha's function that wrap up tests *)
external describe: string -> (unit -> unit [@bs]) -> unit = "" [@@bs.val]

(* mocha's function that include all tests *)
external it: string -> (unit -> unit [@bs]) -> unit = "" [@@bs.val]
external itAsync: string -> (mocha_callback -> unit [@bs]) -> unit = "it" [@@bs.val]

type chai
external chai: chai = "" [@@bs.val]

module Chai_assert = struct
  type t

  external assertion: chai -> t = "assert" [@@bs.get]

  external ok: Js.boolean -> unit = "isOk" [@@bs.send.pipe:t]
  external not_ok: Js.boolean -> unit = "isNotOk" [@@bs.send.pipe:t]
  external eq: 'a -> 'a -> unit = "deepEqual" [@@bs.send.pipe:t]
  external neq: 'a -> 'a -> unit = "notDeepEqual" [@@bs.send.pipe:t]
  external fail: string -> unit = "fail" [@@bs.send.pipe:t]

end

type assertion =
  | Eq: 'a * 'a -> assertion
  | NotEq: 'a * 'a -> assertion
  | Ok: bool -> assertion
  | NotOk: bool -> assertion
  | Fail: string -> assertion

let assert_ok v = Ok v
let assert_not_ok v = NotOk v
let assert_eq a b = Eq (a, b)
let assert_neq a b = NotEq (a, b)
let assert_fail message = Fail (message)

let assertion_to_assert = function
  | Eq (a, b) -> Chai_assert.(assertion chai |> eq a b)
  | NotEq (a, b) -> Chai_assert.(assertion chai |> neq a b)
  | Ok v -> Chai_assert.(assertion chai |> ok (Js.Boolean.to_js_boolean v))
  | NotOk v -> Chai_assert.(assertion chai |> not_ok (Js.Boolean.to_js_boolean v))
  | Fail v -> Chai_assert.(assertion chai |> fail v)

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
