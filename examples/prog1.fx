/* simple function calls, let binding */

fn work(x : int) -> int {
  print("hello world");
  x + 2
}

fn main() -> int {
  let a = 40 in
  work(a)
}
