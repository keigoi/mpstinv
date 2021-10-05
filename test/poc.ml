[@@@warning "-27"]

let rec ta () =
  `B(object 
      method left v = 
        Printf.printf "%d\n" v;
        `B(`ack((), ta ()))
      method right v =
        print_endline v;
        ()
    end)

let rec tb cnt =
  if cnt = 0 then
    `A(`right("finish", ()))
  else
    `A(`left(cnt, `A(object method ack () = tb (cnt-1) end)))

open Domainslib

let () =
  let (ah, bh) =
    (* ここから *)
    (* [%handler.gen] *)
    let a2b = Chan.make_unbounded () in
    let b2a = Chan.make_unbounded () in
    let rec a_handler h =
      let `B(h) = h in
      match Chan.recv b2a with
      | `left(v) -> 
        let `B(`ack (v, h)) = h#left v in
        Chan.send a2b (`ack v);
        a_handler h
      | `right(v) -> 
        h#right v
    in
    let rec b_handler h =
      let `A(h) = h in
      match h with
      | `left(v, h) ->
        Chan.send b2a (`left(v));
        let `ack(v) = Chan.recv a2b in
        let `A(h) = h in
        b_handler (h#ack v)
      | `right(v, ()) ->
        Chan.send b2a (`right(v))
    in
    a_handler, b_handler
    (* ここまで *)
  in
  let t = Thread.create ah (ta ()) in
  bh (tb 10);
  Thread.join t

let f x = 
  x + 1
   
type 't local = {run: 'k. ('t -> 'k) -> 'k}


