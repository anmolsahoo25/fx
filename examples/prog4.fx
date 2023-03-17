fn add2(x : int) -> unit {
  let a = perform("get",2) in
  print(a)
}

fn main() -> int {
  let a = "hello world" in
  let b = "what do you mean" in

  let _ = handle add2(a) with
  | "get", v , k -> let _ = print("handler hell") in continue(a,k)
  | "put", v , k -> continue(v,k) in

  let _ = handle add2(b) with
  | "get", v , k -> continue(b,k) in

  0
}
