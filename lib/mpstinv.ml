open Domainslib
open Types

type 't local = {run: 't -> unit}

type ('obj,'t) method_ = 
  {call_obj: 'obj -> 't}

let f () =
  let msg2 = {call_obj=(fun obj -> obj#msg2)} in
  msg2.call_obj (object method msg2 = () end);
  msg2


type ('obj,'obj2,'t) method2 = 
  {call_obj2: 'obj -> 't; make_obj2: 't -> 'obj2}


let msg =
  {call_obj2=(fun obj -> obj#msg); make_obj2=(fun t -> object method msg = t end)}

(* 
let f : 'o 'a. (<msg: 'a; ..> as 'o, 'a) method2 -> 'a -> 'a = 
  fun (lab : (<msg: 'a; ..>, 'a) method2) x -> lab.call_obj2 (object method msg = x end) *)

(* let rec msg = 
  let f = (fun obj -> obj#msg) in
  {call_obj2=f; 
   compare=(fun lab x -> ignore @@ f (object method msg = x end))} *)

(* let msg2 = {call_obj2=(fun obj -> obj#msg2); make_obj2=(fun k -> k )} *)

(* let f () =
  let msg2 = {call_obj=(fun obj -> obj#msg2)} in
  let msg2' = {call_obj=(fun obj -> obj#msg2)} in
  let mk x = object method msg2 = x end in *)


(* type ('obj,'t) method__ = 
  {call_obj_: 'obj -> 't}
type 'obj t= {k:'t. ('obj,'t) method__ -> 't}

let o = {k=(fun k -> k.call_obj_ (object method msg=() end))}
 *)
type 't t = {runCont: 'k. ('t -> 'k) -> 'k}

type (-'s, +'t, +'a, -'b) lens =
  {get: 's -> 'a; put: 's -> 'b -> 't}

type (-'s, +'t, +'a, -'b) t =
  { op : 'r. ('a -> ('b -> 'r) -> 'r) -> ('s -> ('t -> 'r) -> 'r) }

let lens get set =
  let op acont s tcont =
    acont (get s) (fun b -> tcont (set s b))
  in { op }

let _1 () = lens fst (fun (_, x) b -> (b, x))

let __1 () =
  let op acont s tcont =
    acont (fst s) (fun b -> tcont (let (_,x) = s in (b, x)))
  in
  {op}

class ['a] msg (x:'a) = object
  method msg = x
end

let f (x: 'a #msg) = x#msg
  
let __msg =
  let op acont s tcont =
    acont (s#msg) (fun b -> tcont (let _ = s#msg in object method msg=b end))
  in
  {op}

type (-'obj,'t,'obj2) method__ = {call_obj__:'obj -> 't; make_obj__: 't -> 'obj2}

let f m n t =
  let r = ref None in
  ignore (r:= Some (n.call_obj__ (m.make_obj__ t)));
  !r

type (-'obj,'t) method__ = {call_obj__:'obj -> 't; make_obj__: 'k 'obj2. ((('obj2 -> 't) -> 'k) -> 'k)}
let v = {call_obj__=(fun obj -> obj#msg); make_obj__=(fun k -> k (fun _t -> failwith ""))}

type _ out
type 't partial =
  | Inp : ('obj, 't) method_ * 't sess -> 'obj partial
and 't sess =
  {gen: unit Event.event -> 't partial}

type 't global =
  Comm : 
    ('r1 sess, 'r1x sess, 'c, 'd, 'b, 'e) role *
    ('r2 sess, 'r2x sess, 'c, 'd, 'b, 'e) role *
    ('obj,'t,'var,'u) label * 'x global -> 't global


(* let cast : 'var 'v 't. ('var, 'v * 't) constr -> ('var, 'v) constr = *)

let (-->) r1 r2 msg g =
  let ch = Chan.make_unbounded () in
  let handle_r1 h =
    let v, h = msg.var.match_var (r1.role_label.match_var h) in
    Chan.send ch v;
    (Seq.get r1.role_index g).run h
  in
  let handle_r2 h =
    let v = Chan.recv ch in
    let h = msg.obj.call_obj (r2.role_label.match_var h) v in
    (Seq.get r1.role_index g).run h
  in
  let g = Seq.put r2.role_index g {run=handle_r2} in
  let g = Seq.put r1.role_index g {run=handle_r1} in
  g



  (* let mid1 = 
    let ch = Chan.make_unbounded () in
    {run=(fun h -> h#mid )}
   *)