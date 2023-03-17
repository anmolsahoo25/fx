fn work(x : int) -> int {
  perform("put", "hello world");
  0
}

fn main() -> int {
  let x = 10 in
  handle work(x) with
  | "put" , v , k -> print(v) ; continue(0, k)
}
