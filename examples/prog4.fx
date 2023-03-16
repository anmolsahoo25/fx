fn add2(x : int) -> int {
  x + 4
}

fn main() -> int {
  handle add2(10) with
  | "get" v , k -> continue(k,2)
}
