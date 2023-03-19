/* basic effect handlers for io */

fn work(x : int) -> int {
  let y = perform("write","hello") in
  let z = perform("write","world") in
  x + y + z
}

fn main() -> int {
  handle work(1) with
  | "write", v , k -> print(v) ; continue(1, k)
}
