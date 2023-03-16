fn add2(x : int) -> int {
  let y = perform("get",2) in
  x + 4
}

fn main() -> int {
  let a = 10 in
  let b = 20 in

  let y = handle add2(a) with
  | "get" v , k -> continue(k,2) in

  let x = handle add2(b) with
  | "get" v , k -> continue(k,2) in

  x + y
}
