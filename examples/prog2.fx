fn add2(x : int) -> int {
  x + 2
}

fn main() -> int {
  handle add2(10) with
  | v -> v
}
