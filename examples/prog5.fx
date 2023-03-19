/* handlers for coroutines */

fn work1(x : int) -> int {
  perform("suspend", "hello world from 1");
  print("finishing from 1")
}

fn work2(x : int) -> int {
  perform("suspend", "hello world from 2");
  print("finishing from 2")
}

fn main() -> int {
  let mut k1 = null in

  handle work1(0) with
  | "suspend" , v , k ->
      print(v);
      k1 := k;
      begin handle work2(0) with
      | "suspend" , v , k ->
        print(v);
        continue(0, k)
      end;
      continue(0,!k1)
}
