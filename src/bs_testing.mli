(* `done` callback *)
type mocha_callback = unit -> unit

(* Type of assertion *)
type assertion

(* assertions *)
val assert_ok : bool -> assertion
val assert_not_ok : bool -> assertion
val assert_eq : 'a -> 'a -> assertion
val assert_neq : 'a -> 'a -> assertion
val assert_fail : string -> assertion
val assertion_to_assert : assertion -> unit

(* create test case *)
type test =
  | Sync of (string * (unit -> assertion))
  | Async of (string * (unit -> assertion Js.Promise.t))

(* create suite *)
val suite : string -> test list -> unit
