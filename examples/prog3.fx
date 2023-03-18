(* basic effect handlers for io *)

fn work(x : int) -> int {
  let x = perform("write","hello") in
  let y = perform("write","world") in
  x + y
}

fn main() -> int {
  handle work(0) with
  | "write", v , k -> print(v) ; continue(1, k)
}
