

type _ t =
  | SeqCons : 'hd * 'tl t -> [ `cons of 'hd * 'tl ] t
  | SeqNil : ([ `cons of unit * 'a ] as 'a) t

let head : type hd tl. [`cons of hd * tl] t -> hd =
  function
  | SeqCons(hd,_) -> hd
  | SeqNil -> ()

let tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqNil -> SeqNil

let rec get : type a b xs ys. (a, b, xs, ys) Types.idx -> xs t -> a = fun ln xs ->
  match ln with
  | Zero -> head xs
  | Succ ln' -> get ln' (tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) Types.idx -> xs t -> b -> ys t =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(b, tail xs)
  | Succ ln' -> SeqCons(head xs, put ln' (tail xs) b)
