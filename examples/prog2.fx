(* mutable variables, sequencing *)

fn work(x : mut int, s : mut string) -> int {
  x := 10;
  s := "hello";
  0
}

fn main() -> int {
  let mut x = 0 in
  let mut s = "" in
  work(x,s);
  print(!s);
  !x
}
