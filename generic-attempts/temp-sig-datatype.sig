signature WITH_DATATYPE =
sig
    datatype 'a t = Constr of 'a
end

signature WITH_DATATYPE_SPEC = WITH_DATATYPE where type 'a t = 'a list

(* functor SpecializedFunctor () :> WITH_DATATYPE_SPEC = *)
(* struct *)

(*     type 'a t = 'a list *)

(* end *)

(* compiler output
signature WITH_DATATYPE =
  sig
    datatype 'a t = Constr of 'a
  end
signature WITH_DATATYPE_SPEC =
  sig
    datatype t = datatype list
  end
*)
