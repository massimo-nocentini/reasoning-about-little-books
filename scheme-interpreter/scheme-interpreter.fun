
structure SchemeInterpreterEnvironment = 
    struct

    type identifier = string

    datatype scheme_term = integer of int
			 | boolean of bool
			 | cons
			 | car 
			 | cdr
			 | null_p
			 | eq_p
			 | atom_p				     
			 | zero_p
			 | succ
			 | pred
			 | number_p
			 | identifier of identifier
			 | quote
			 | lambda
			 | cond

    functor MakeTypeSchemeTerm () 
	    :> TYPE where type aType = scheme_term
            =
	    struct type aType = scheme_term end

    structure Sexp = MakeSexp (
	structure ObjectType = MakeTypeSchemeTerm ())

    datatype scheme_meaning = Primitive of Sexp.sexp
			    | Quotation of Sexp.sexp
			    | Integer of int
			    | Boolean of bool
			    (* | NonPrimitive of { *)
			    (* 	table: table, *)
			    (* 	formals: Sexp.sexp, *)
			    (* 	body: Sexp.sexp *)
			    (* } *)
			    | Stub (* to delete when finished! *)

    functor MakeTypeSchemeMeaning () 
	    :> TYPE where type aType = scheme_meaning
            =
	    struct type aType = scheme_meaning end

    functor MakeInterpreter (
	structure Sexp: SEXP where type object = scheme_term
	val mk_primitive: Sexp.sexp -> scheme_meaning
	val mk_quotation: Sexp.sexp -> scheme_meaning
	structure Table: TABLE where type identifier = string
                               where type stuff = scheme_meaning) 
	    :> SEXP_INTERPRETER where type term = scheme_term
	                        where type meaning = scheme_meaning
				where type identifier = Table.identifier
            =
	    struct

		structure Sexp = Sexp

	        type term = scheme_term

		type meaning = scheme_meaning

		type identifier = Table.identifier

		type action = Sexp.sexp 
			      -> Table.table 
			      -> meaning

		exception EmptyListNotAllowedForNonPrimitiveExpression
		exception IdentifierNotBound of identifier

		fun const_type (Sexp.Atom (integer i)) _ = Integer i
		  | const_type (Sexp.Atom (boolean b)) _ = Boolean b
		  | const_type (atom as Sexp.Atom _) _ = mk_primitive atom

		fun quote_type (Sexp.List 
				    (Sexp.Cons 
					 (Sexp.Atom quote, conses))) _ =
		    mk_quotation (Sexp.List conses)

		fun identifier_type (Sexp.Atom (identifier key)) aTable = 
		    Table.lookup_in_table 
			(fn anotherKey => key = anotherKey)
			aTable
			(fn (Table.KeyNotFound _) => 
			    raise IdentifierNotBound key)

		fun lambda_type aSexp aTable = Stub

		fun cond_type aSexp aTable = Stub
		fun application_type aSexp aTable = Stub

		fun sexp_to_action (atom as Sexp.Atom term) = 
		    (case term of
			integer _ => const_type
		      | boolean _ => const_type
		      | cons => const_type
		      | car => const_type
		      | cdr => const_type
		      | null_p => const_type
		      | eq_p => const_type
		      | atom_p => const_type		     
		      | zero_p => const_type
		      | succ => const_type
		      | pred => const_type
		      | number_p => const_type
		      | _ => identifier_type)
		  | sexp_to_action (list as Sexp.List conses) =
		    case conses of
			Sexp.Cons (Sexp.Atom atom, _) =>
			let
			    fun A quote = quote_type
			      | A lambda = lambda_type
			      | A cond = cond_type
			      | A _ = application_type
			in A atom end	    
		      | Sexp.Cons (_, _) => application_type
		      | Sexp.Null => 
			raise EmptyListNotAllowedForNonPrimitiveExpression

		fun value aSexp = sexp_to_action aSexp 
						 aSexp 
						 Table.empty_table
						 

	    end

    structure Interpreter = MakeInterpreter (
	structure Sexp = Sexp
	val mk_primitive = Primitive
	(* simply, instead of (fn sexp_obj => Quotation sexp_obj) *)
	val mk_quotation = Quotation 
	structure Table = MakeTableDoubleListImpl (
	    structure IdentifierType = MakeTypeString ()
	    structure StuffType = MakeTypeSchemeMeaning ()))

    end
