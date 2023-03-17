fn add2(x : int) -> int {
  let y = perform("get",2) in
  x + y
}

fn main() -> int {
  let a = 1 in
  let b = 2 in

  let y = handle add2(a) with
  | "get", v , k -> let _ = print("handler hell") in continue(v,k)
  | "put", v , k -> continue(v,k) in

  let x = handle add2(b) with
  | "get", v , k -> continue(v,k) in

  x + y
}
