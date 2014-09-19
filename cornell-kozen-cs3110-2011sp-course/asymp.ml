let even a =
  a = 2 * (a/2)

let rec times1 a b =
  if b = 0 then 0
  else a + times1 a (b - 1)

let rec times2 a b =
  if b = 0 then 0
  else if even b then times2 (a*2) (b/2)
  else a + times2 a (b - 1)

let rec times3 a b =
  if b = 0 then 0
  else if even b then times3 (a*2) (b/2)
  else a + times3 (a*2) ((b-1)/2)

(* split xs is a pair (ys, zs) where half (rounding up) of
 * the elements of xs are in ys and the rest are in zs.
 *) 

let split xs = 
  let rec loop xs l r =
    match xs with
	[] -> (l, r)
      | [x] -> (x :: l, r)
      | x :: y :: t -> loop t (x :: l) (y :: r)
  in loop xs [] []

(* A simpler way to write split using fold. What is the asymptotic
 * performance of fold_left f lst0 lst where f is an O(1) function and
 * lst is an n-element list? O(n). *)

let split2 xs = 
  List.fold_left (fun (l, r) x -> (x :: r, l)) ([], []) xs
    
(* merge left right is a sorted list (in ascending order) containing
 * all the elements of left and right.  Requires: left and right
 * are sorted lists *)

let rec merge xs ys =
  match (xs, ys) with
      ([], _) -> ys
    | (_, []) -> xs
    | (x :: xs', y :: ys') -> 
	 if x > y then y :: merge xs ys'
         else x :: merge xs' ys

(* merge_sort xs is a list containing the same elements as xs but in
 * ascending (nondescending) sorted order.  *)

let rec merge_sort xs =
  match xs with
      []
    | [_] -> xs
    | _ -> let (l, r) = split xs in 
        merge (merge_sort l) (merge_sort r)

(* silly sort - sorts first two-thirds, last two-thirds then first
 * two-thirds again. *)

let rec drop l m =
  match l with
      [] -> []
    | x :: r -> if m = 0 then l else drop r (m - 1)

let rec take l m =
  match l with
      [] -> []
    | x :: r -> if m = 0 then [] else x :: (take r (m - 1))

let rec sort3 a =
  match a with
      [] -> []
    | [x] -> [x]
    | [x; y] -> [min x y; max x y]
    | _ -> 
	let n = List.length a in
	let m = (2*n + 2) / 3 in
	let res1 = sort3 (take a m) @ drop a m in
	let res2 = take res1 (n - m) @ sort3 (drop res1 (n - m)) in
        sort3 (take res2 m) @ drop res2 m
