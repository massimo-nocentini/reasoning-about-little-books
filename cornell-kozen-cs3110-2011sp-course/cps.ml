(* straightforward recursion *)
let rec sum s =
    match s with
        [] -> 0
      | x::xs -> x + sum xs

(* tail recursive *)
let sum s =
  let rec sum' s a =
    match s with
        [] -> a
      | x::xs -> sum' xs (a + x) in
    sum' s 0

(* using continuations *)
let sum s =
  let rec sum' s k =
    match s with
        [] -> k 0
      | x::xs -> sum' xs (fun a -> k (x + a)) in
  sum' s (fun x -> x)


(* (\* straightforward recursion *\) *)
(* let rec fold_right (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b = *)
(*   match s with *)
(*       [] -> b *)
(*     | x::xs -> f x (fold_right f xs b) *)

(* (\* using continuations *\) *)
(* let fold_right (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b = *)
(*   let rec fold_right' s k = *)
(*     match s with *)
(*         [] -> k b *)
(*       | x::xs -> fold_right' xs (fun y -> k (f x y)) in *)
(*     fold_right' s (fun x -> x) *)

(* (\* straightforward recursion *\) *)
(* fun foldr (f:&#39;a * &#39;b -&gt; &#39;b) (b:&#39;b) (s:&#39;a list) : &#39;b = *)
(*   case s of *)
(*     [] =&gt; b *)
(*   | x::t =&gt; f(x,foldr f b t) *)

(* (\* using continuations *\) *)
(* fun foldr (f:&#39;a * &#39;b -&gt; &#39;b) (b:&#39;b) (s:&#39;a list) : &#39;b = let *)
(*   fun foldr&#39; [] k = k b *)
(*     | foldr&#39; (x::t) k = foldr&#39; t (fn y =&gt; k(f(x,y))) *)
(* in *)
(*   foldr&#39; s (fn x =&gt; x) *)
(* end *)
