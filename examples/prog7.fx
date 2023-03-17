fn work1(x : int) -> int {
  perform("put", "hello world from 1");
  print("finishing from 1");
  0
}

fn work2(x : int) -> int {
  perform("put", "hello world from 2");
  print("finishing from 2");
  0
}

fn main() -> int {
  let mut k1 = null in
  handle work1(10) with
  | "put" , v , k ->
      let _ = print(v) in
      let _ = k1 := k in
      let _ = handle work2(10) with
      | "put" , v , k ->
        let _ = print(v) in
        continue(0, k) in
      continue(0,!k1)
}
