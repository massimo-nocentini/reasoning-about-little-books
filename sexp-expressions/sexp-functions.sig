signature SEXP_FUNCTIONS = 
sig
    type 'a sexp
    type 'a slist

    val equal: ('a -> 'b -> bool) -> 'a sexp -> 'b sexp -> bool
    val to_string: ('a -> string) -> 'a sexp -> string
    val is_atom: 'a sexp -> bool
    val combine: ('a slist -> 'a slist -> 'a slist) -> 
		 'a sexp -> 'a sexp -> 'a sexp

    val combine_slists_staged: 'a slist -> 'a slist -> 'a slist
    val combine_slists_curried: 'a slist -> 'a slist -> 'a slist

    val two_in_a_row: ('a -> 'a -> bool) -> 'a sexp -> bool
end

