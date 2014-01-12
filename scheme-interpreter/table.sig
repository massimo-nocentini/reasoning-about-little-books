signature TABLE = 
sig

    type identifier

    type 'a keys
    type 'a values
    type 'a entry

    val new_entry: identifier keys -> 'a values -> 'a entry

    datatype 'a lookup_failure = KeyNotFound of identifier -> bool
			       | ValuesNotEnough of 'a values

    type 'a table
    val empty_table: 'a table
    val extend_table: 'a entry -> 'a table -> 'a table
    val lookup_in_table: (identifier -> bool) 
			 -> 'a table 
			 -> ('a lookup_failure -> 'a)
			 -> 'a

end

functor MakeTableDoubleListImpl (
    structure IdentifierType: TYPE)
	:> TABLE where type identifier = IdentifierType.aType
		 where type 'a values = 'a list
                 where type 'a keys = 'a list
        =
	struct

	type identifier = IdentifierType.aType

	type 'a values = 'a list
        type 'a keys = 'a list

	type 'a entry = (identifier keys) * ('a values)
	type 'a table = 'a entry list

	datatype 'a lookup_failure = KeyNotFound of identifier -> bool
				   | ValuesNotEnough of 'a values

	fun new_entry keys values = (keys, values)

	fun lookup_in_entry pred 
			    (anEntry as (orig_keys, orig_values))
			    aFailureHandler = 
	    let
		fun L ([], _) = aFailureHandler (KeyNotFound pred)
		  | L (aKey :: keys, []) = 
		    aFailureHandler (ValuesNotEnough orig_values)
		  | L (aKey :: keys, aValue :: values) = 
		    if pred aKey
		    then aValue
		    else L (keys, values)
	    in
		L anEntry
	    end
			   
	val empty_table = []
			      
	fun extend_table anEntry aTable = anEntry :: aTable

	fun lookup_in_table pred aTable aFailureHandler = 
	    let
		fun L [] = aFailureHandler (KeyNotFound pred)
		  | L (anEntry :: entries) = 
		    lookup_in_entry pred 
				    anEntry 
				    (failure_handler entries)
		and failure_handler entries (KeyNotFound _) = 
		    L entries
		  | failure_handler _ (vne as ValuesNotEnough _) =
		    aFailureHandler vne
	    in
		L aTable
	    end

	end
