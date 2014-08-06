
signature SEXP_FUNCTIONS_COLLECTION = 
	sig
		structure Sexp : SEXP
		structure Equal: SEXP_EQUAL sharing type 'a Equal.sexp = 'a Sexp.sexp
		structure ToString: SEXP_TO_STRING sharing type 'a Equal.sexp = 'a ToString.sexp
		structure CombineStaged: SEXP_COMBINE sharing type 'a ToString.sexp = 'a CombineStaged.sexp
		structure CombineCurried: SEXP_COMBINE sharing type 'a CombineStaged.sexp = 'a CombineCurried.sexp
		structure Pick : SEXP_PICK sharing type 'a CombineCurried.sexp = 'a Pick.sexp
	end

functor SexpFunctionsCollection 
	(structure Sexp : SEXP
	 structure Equal: SEXP_EQUAL sharing type 'a Equal.sexp = 'a Sexp.sexp
	 structure ToString: SEXP_TO_STRING sharing type 'a Equal.sexp = 'a ToString.sexp
	 structure CombineStaged: SEXP_COMBINE sharing type 'a ToString.sexp = 'a CombineStaged.sexp
	 structure CombineCurried: SEXP_COMBINE sharing type 'a CombineStaged.sexp = 'a CombineCurried.sexp
	 structure Pick : SEXP_PICK sharing type 'a CombineCurried.sexp = 'a Pick.sexp)
	:> SEXP_FUNCTIONS_COLLECTION where type 'a Sexp.sexp = 'a Sexp.sexp (* we hope that this will be sufficient *)
	=
	struct

		structure Sexp = Sexp
		structure Equal = Equal
		structure ToString = ToString
		structure CombineStaged = CombineStaged
		structure CombineCurried = CombineCurried
		structure Pick = Pick

	end

