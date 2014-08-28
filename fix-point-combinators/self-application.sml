
signature SELF_APPLICATION = 
    sig
        datatype 'a self = Into of 'a self -> 'a

        val self_apply_one_arg : ('a -> 'b) self -> 'a -> 'b
        val self_apply_two_args : ('a -> 'b -> 'c)  self -> 'a -> 'b -> 'c
        val self_apply_three_args : ('a -> 'b -> 'c -> 'd)  self -> 'a -> 'b -> 'c -> 'd
    end

functor SelfApplication () 
    :> SELF_APPLICATION 
    = 
    struct

	datatype 'a self = Into of 'a self -> 'a

    fun self_apply_one_arg (into as Into aFn) x1 = aFn into x1
    fun self_apply_two_args (into as Into aFn) x1 x2 = aFn into x1 x2
    fun self_apply_three_args (into as Into aFn) x1 x2 x3 = aFn into x1 x2 x3

    (*
     The following is another way to implement self application, 
     inspired by Little MLer book, in the last page of the book.
     *)
    (* fun Y f = H f (Into (H f)) *)
    (* (* and H f into = f (G into) *) *)
    (* and H f = f o G *)

    (* using the following definition the type-checker takes very long
     time to type-check the phrase.*)
    (* and G (into as Into aFn) = aFn into *)

	end
    
