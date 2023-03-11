fn thread(x : int, y : int) -> int {
  x
}

fn worker(v : string) -> int {
  let _ = perform (fork,thread,20) in
  0
}

fn main() -> unit {
  let x = 10 in
  let y = 20 in
  handle thread with
  | (write,10) -> print(10)
  | _ -> print(20)
}
