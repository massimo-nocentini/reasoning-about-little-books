

signature FIX_POINT_COMBINATOR = 
sig
    type source
    type target

    val fix: (source -> target) -> source -> target
end

(* signature SPEC_FIX_POINT_COMBINATOR = FIX_POINT_COMBINATOR where type source = ('a -> 'b) *)
structure simple = 
    struct

    type target = string

    fun int_ctor (anInt: int) : target = "hello"

    fun string_ctor (aString: string) : target = aString

    datatype 'a generic = Item of 'a

    (* fun matching_attemp (Item int_ctor anInt) = anInt *)
    (*   | matching_attemp (string_ctor aString) = 4 *)

    end

signature SELF_APPLICATION = 
sig

    type recur
    datatype 'b into = Into of 'b into -> 'b
    type use = recur into

end

signature A = 
sig
    val id: 'a -> 'a
end

(* signature SEXP =  *)
(* sig *)
(*     include SEXP_SPEC *)
    
(*     type sexp *)
(*     type slist *)

(*     datatype ta = tc *)
(* 		      withtype tb = int *)
(* 	 and tc = tb *)

(* end *)

(* local *)
(*     datatype a = A *)
(* in *)
(* signature SEXP_SPEC = SEXP where type sexp = a *)
(* end *)

signature SEXP =
sig

    
    type 'a sexp
    type slist
end

