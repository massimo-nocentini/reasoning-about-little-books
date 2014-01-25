signature SEXP_FUNCTIONS = 
sig
    type 'a sexp
    type 'a slist

    val equal: ('a -> 'b -> bool) -> 'a sexp -> 'b sexp -> bool
    val to_string: ('a -> string) -> 'a sexp -> string
    val is_atom: 'a sexp -> bool
end

