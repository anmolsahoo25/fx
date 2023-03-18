(* effect handlers for state *)

fn work(x : int) -> unit {
  perform("put",4);
  perform("get", 0)
}

fn main() -> int {
  let mut x = 0 in

  handle work(0) with
  | "put", v , k -> x := v ; continue(0,k)
  | "get", v , k -> continue(!x,k)
}
