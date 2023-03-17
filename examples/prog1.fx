fn add2(x : int) -> int {
  let y = perform("get", 10) in
  x + y
}

fn main() -> int {
  let a = 10 in
  handle add2(a) with
  | "get",v,k -> continue(v+1, k)
}
