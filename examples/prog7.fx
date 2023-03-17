fn work(x : int) -> int {
  perform("put", "hello world");
  0
}

fn main() -> int {
  let k1 = 10 in
  handle work(k1) with
  | "put" , v , k -> 
      let _ = handle work(k1) with
      | "put" , v , k -> print(v) ; continue(0, k) in
      print(v) ; continue(0,k)
}
