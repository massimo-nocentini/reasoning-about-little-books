signature ORDERED_FUNCTIONAL_SET = sig
  (* Overview: a "set" is a set of distinct
   * elements of type "elem". Each element is     
   * identified by a unique key, which may be the 
   * same as the element itself. Two elements are 
   * considered distinct if they have different   
   * keys.  Keys are a totally ordered set.       
   *
   * A set can be used to represent an ordinary   
   * set if key = elem.  It can be used to        
   * represent a mapping if elem = key * value.   
   *
   * For example, if key and elem are int,        
   * then a set might be {1,-11,0}, {}, or        
   * {1001}. If key is string and elem is int,    
   * a set could be {("elephant", 2), ("rhino",   
   * 25), ("zebra", 2)} *)                        
  type key
  type elem
  type set

  (* compare(k1,k2) reports the ordering of k1 and k2. *)
  val compare: key * key -> order
  (* keyOf(e) is the key of e. *)
  val keyOf: elem -> key
  (* empty() is the empty set. *)
  val empty : unit -> set
  (* Effects: add(s,e) is s union {e}. Returns true
   * if e already in s, false otherwise. *)
  val add: set * elem -> set * bool
  (* remove(s,k) is (s',eo) where s' = s - {k} (set difference)
   * and eo is either SOME e if there is an e in s
   * where k is e's key, or NONE otherwise. *)
  val remove: set * key -> set * elem option
  (* lookup(s,k) is SOME e where k = keyOf(e), or NONE if
   * the set contains no such e. *)
  val lookup: set * key -> elem option
  (* size(s) is the number of elements in s. *)
  val size: set -> int

  (* Ordered set operations *)

  (* first(s) is SOME of the element of s with the smallest key,
   * or NONE if s is empty. *)
  val first: set -> elem option
  (* last(s) is SOME of the element of s with the largest key,
   * or NONE if s is empty. *)
  val last: set -> elem option
  (* A fold operation on ordered sets takes a key argument
   * that defines the element where the fold starts. *)
  type 'b folder = ((elem*'b)->'b) -> 'b -> key -> set -> 'b
  (* fold over the elements in key order. *)
  val fold_forward: 'b folder
  (* fold over the elements in reverse key order. *)
  val fold_backward: 'b folder

  val print: set -> unit
end

signature ORDERED_SET_PARAMS = sig
  type key
  type elem
  val keyOf: elem -> key
  val compare: key * key -> order
  val toString: elem -> string
end

functor SplayTree(structure Params : ORDERED_SET_PARAMS)
  :> ORDERED_FUNCTIONAL_SET where type key = Params.key and
                                  type elem = Params.elem =
struct
  type key = Params.key
  type elem = Params.elem
  val compare = Params.compare
  val keyOf = Params.keyOf
  datatype tree =
    Empty
  | Node of tree * elem * tree
  type node = tree * elem * tree
  (* Representation invariant: given a node (L, V, R),
   * All values in the left subtree L are less than V, and
   * all values in the right subtree R are greater than V, and
   * both L and R also satisfy the RI.
   *)
  type set = int * (tree ref)
  (* Representation invariant: size is the number of elements in
   * the referenced tree. *)

  fun empty() = (0, ref Empty)

  (* splay(n,k) is a BST node n' where n' contains
   * all the elements that n does, and if an
   * element keyed by k is in under n, #value n'
   * is that element.  Requires: n satisfies the
   * BST invariant.
   *)
  fun splay((L, V, R), k: key): node =
    case compare(k, keyOf(V))
      of EQUAL => (L, V, R)
       | LESS =>
        (case L
           of Empty => (L, V, R) (* not found *)
            | Node (LL, LV, LR) =>
             case compare(k, keyOf(LV))
               of EQUAL => (LL, LV, Node(LR, V, R)) (* 1: zig *)
                | LESS =>
                 (case LL
                    of Empty => (Empty, LV, Node(LR, V, R))
				(* not found *)
                     | Node n => (* 2: zig-zig *)
                      let val (LLL, LLV, LLR) = splay(n,k) in
                        (LLL,LLV,Node(LLR,LV,Node(LR,V,R)))
                      end)
                | GREATER =>
                    (case LR
                       of Empty => (LL, LV, Node(Empty, V, R))
                        | Node n =>  (* 3: zig-zag *)
                         let val (LRL, LRV, LRR) = splay(n,k) in
                           (Node(LL,LV,LRL),LRV,Node(LRR,V,R))
                         end))
       | GREATER =>
	(case R
	    of Empty => (L, V, R) (* not found *)
	    | Node (RL, RV, RR) =>
	    case compare(k, keyOf(RV))
		of EQUAL => (Node(L,V,RL),RV,RR) (* 1: zag *)
		| GREATER =>
		(case RR
		    of Empty => (Node(L,V,RL),RV,RR) (* not found *)
		    | Node n => (* 3: zag-zag *)
			let val (RRL, RRV, RRR) = splay(n,k) in
			(Node(Node(L,V,RL),RV,RRL),RRV,RRR)
			end)
		| LESS =>
		(case RL
		    of Empty => (Node(L,V,RL),RV,RR) (* not found *)
		    | Node n => (* 2: zag-zig *)
			let val (RLL, RLV, RLR) = splay(n,k) in
			(Node(L,V,RLL),RLV,Node(RLR,RV,RR))
			end))

  fun lookup((size,tr),k) =
    case !tr of
      Empty => NONE
    | Node n =>
        let val n' as (L,V,R) = splay(n,k) in
          tr := Node n';
          if compare(k, keyOf(V)) = EQUAL then SOME(V)
          else NONE
        end

  fun add((size,tr):set, e:elem) = let
    val (t', b) = add_tree(!tr, e)
    val t'': node = splay(t', keyOf(e))
    val size' = if b then size else size+1
  in
    ((size', ref (Node(t''))),b)
  end
  and add_tree(t: tree, e: elem): node * bool =
    case t
      of Empty => ((Empty, e, Empty), false)
       | Node (L,V,R) =>
        (case compare (keyOf(V),keyOf(e))
           of EQUAL => ((L,e,R),true)
            | GREATER => let val (n',b) = add_tree(L, e) in
                           ((Node(n'),V,R),b)
                         end
            | LESS =>    let val (n',b) = add_tree(R, e) in
                           ((L,V,Node(n')),b)
                         end)


  fun size(s,tr) = s

  type 'b folder = ((elem*'b)->'b) -> 'b -> key -> set -> 'b

  fun fold_forward f b k (size,tr) = fold_forward_tree f b k (!tr)
  and fold_forward_tree (f: elem*'b->'b) (b:'b) (k:key) (t:tree) =
    case t
      of Empty => b
       | Node (L,V,R) =>
        (case compare(keyOf(V), k) of
           EQUAL => fold_forward_tree f (f(V,b)) k R
         | LESS => fold_forward_tree f b k R
         | GREATER => let val lv = fold_forward_tree f b k L in
             fold_forward_tree f (f(V,lv)) k R
           end)

  fun first((size,tr)): elem option = raise Fail "first: not implemented"
  fun remove(s,e) = raise Fail "remove: not implemented"
  fun last(s,tr) = raise Fail "last: not implemented"
  fun fold_backward f b k s = raise Fail "fold_backward: not implemented"

(* The remainder of this code is for pretty-printing binary trees *)
  fun spaces(n) =
    if n > 0 then " " ^ spaces(n-1) else
      if n <= 0 then "" else
        raise Fail "negative space!"
  fun rdiag(n): string list =
    if n = 0 then []
    else rdiag(n-1) @ [spaces(n-1) ^ "\\"]
  fun ldiag(n) =
    if n = 0 then []
    else  [spaces(n-1) ^ "/"] @ ldiag(n-1)

  fun indent(sl,n) = let val ws = spaces(n) in
    map (fn(s) => ws^s) sl
  end
  fun pad(sl, n) = map (fn(s) => s ^ spaces(n - String.size(s))) sl
  fun hsplice(h1::t1,h2::t2,w1,w2) =
    (h1^h2) :: hsplice(t1,t2,w1,w2)
    | hsplice(sl1: string list, nil,w1,w2) = pad(sl1,w1+w2)
    | hsplice(nil, sl2,w1,w2) = indent(sl2, w1)
  (* toStrings(t) is (sl,w,h,r) where "sl" is a list of h strings of
   * length w representing a drawing of "t", where the root of the tree
   * is positioned in the first string at offset "r" *)
  fun toStrings(t: tree): (string list)*int*int*int =
    case t of
      Empty => ([], 0, 0, 0)
    | Node (L,V,R) => let
        val vs = Params.toString(V)
        val vl = String.size(vs)
        val (sl1,w1,h1,r1) = toStrings(L)
        val (sl2,w2,h2,r2) = toStrings(R)
        val padding = case r2 + w1 - r1
          of 0 => 2
           | 1 => 1
           | 2 => 0
           | diff => if diff mod 2 = 0 then 0 else 1
        val w = Int.max(w1 + w2 + padding,vl)
        val diagsize = (r2 + w1 - r1 + padding) div 2
        val leftarc = case L of
          Empty => []
          | _ => ldiag(diagsize)
        val rightarc = case R of
          Empty => []
        | _ => rdiag(diagsize)
        val sl = pad(indent([vs], r1 + diagsize - (vl div 2)), w) @
          pad(indent(hsplice(pad(leftarc, diagsize+1), pad(rightarc, diagsize),
                             diagsize+1, diagsize),
                     r1), w) @
          hsplice(sl1, indent(sl2, padding), w1, w2+padding)
      in
        (sl, w, Int.max(h1,h2)+diagsize+1, diagsize+r1)
      end
  fun print((size,tr)) =
    let val (sl,w,h,r) = toStrings(!tr) in
      List.app (fn(s:string) => TextIO.print (s^"\n")) sl
    end
end

structure I_Params = struct
  type key = int
  type elem = int
  fun keyOf x = x
  val compare = Int.compare
  val toString = Int.toString
end

structure IST = SplayTree(structure Params = I_Params)

open IST
fun ins_n(n) =
  if n = 0 then empty() else #1(add(ins_n(n-1), n))
