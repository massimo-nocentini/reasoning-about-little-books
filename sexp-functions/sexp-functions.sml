
signature SEXP_FUNCTIONS_COLLECTION = 
sig
    structure Sexp : SEXP
    (*
		 The following declaration remembers us that using `sharing type'
		 doesn't require to explicitly say type variables ('a, 'b, ...), 
		 only the type constructor is required.
    *)
    structure Equal: SEXP_EQUAL sharing type Equal.sexp = Sexp.sexp
    structure ToString: SEXP_TO_STRING sharing type  Equal.sexp =  ToString.sexp
    structure CombineStaged: SEXP_COMBINE sharing type  ToString.sexp =  CombineStaged.sexp
    structure CombineCurried: SEXP_COMBINE sharing type  CombineStaged.sexp =  CombineCurried.sexp
    structure Pick : SEXP_PICK sharing type  CombineCurried.sexp =  Pick.sexp
    structure Occurs : SEXP_OCCURS sharing type Occurs.sexp = Sexp.sexp
    structure Subst : SEXP_SUBST sharing type Subst.sexp = Sexp.sexp
end

functor SexpFunctionsCollection 
	(structure Sexp : SEXP
	 structure Equal: SEXP_EQUAL sharing type  Equal.sexp =  Sexp.sexp
	 structure ToString: SEXP_TO_STRING sharing type  Equal.sexp =  ToString.sexp
	 structure CombineStaged: SEXP_COMBINE sharing type  ToString.sexp =  CombineStaged.sexp
	 structure CombineCurried: SEXP_COMBINE sharing type  CombineStaged.sexp =  CombineCurried.sexp
	 structure Pick : SEXP_PICK sharing type  CombineCurried.sexp =  Pick.sexp
	 structure Occurs : SEXP_OCCURS sharing type Occurs.sexp = Sexp.sexp
	 structure Subst : SEXP_SUBST sharing type Subst.sexp = Sexp.sexp)
	:> SEXP_FUNCTIONS_COLLECTION where type 'a Sexp.sexp = 'a Sexp.sexp (* we hope that this will be sufficient *)
	=
	struct

		structure Sexp = Sexp
		structure Equal = Equal
		structure ToString = ToString
		structure CombineStaged = CombineStaged
		structure CombineCurried = CombineCurried
		structure Pick = Pick
		structure Occurs = Occurs
		structure Subst = Subst
	end

