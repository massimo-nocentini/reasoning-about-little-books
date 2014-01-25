signature TABLE_CONTENT = 
sig
    type key
    type stuff
end

signature TABLE = 
sig

    (* maybe this approach it is much better to use a inner structure  *)
    type key
    type stuff

    type 'a key_collection
    type 'a value_collection

    val new_entry: key key_collection -> stuff value_collection -> table

    type table
end

functor MkTable (structure aTableContent: TABLE_CONTENT)
	:> TABLE 
	=
	struct

	type key = aTableContent.key
	type value = aTableContent.value

	type 'a keys
	type 'a values

	type table

	end

signature SCHEME_SEMANTIC = 
sig
    type table

    datatype meaning = Quotation
		     | NonPrimitive of {aTable: table}
end

functor MkTableContentWithSchemeSemantic (
    structure aSchemeSemantic: SCHEME_SEMANTIC)
	:> TABLE_CONTENT 
	=
	struct
	
	type key = string
	type stuff = aSchemeSemantic.meaning

	end

functor MkSchemeSemantic (structure aTable: TABLE)
	:> SCHEME_SEMANTIC where type table = aTable.table
	=
	struct
	
	type table = aTable.table

	datatype meaning = Quotation
			 | NonPrimitive of {aTable: table}


	end


