signature BASE = 
sig
    type 'a aType
end

functor MkBaseAsList () :> BASE where type 'a aType = 'a list
= 
struct 
    type 'a aType = 'a list
end

signature WITH_FUNCTOR = 
sig

    type object
    structure Base: BASE

    functor MkBase (): BASE where type 'a aType = object Base.aType

end

functor WithFunctor (structure aBase: BASE)
	:> WITH_FUNCTOR where type object = int
        =
	
	struct

	type object = int
	structure Base = aBase

	functor MkBase () :> BASE where type 'a aType = object Base.aType 
	    =
	    struct
	    
	    type 'a aType = object Base.aType 

	    end

	end

structure Unknown = WithFunctor (structure aBase = MkBaseAsList ())
open Unknown;
structure TranBase = Unknown.MkBase ()
open TranBase;

structure AnotherUnknown = WithFunctor (structure aBase = TranBase)
open AnotherUnknown
open AnotherUnknown.Base

functor Op (structure aBase: BASE) = 
	struct
	
	fun id (x: 'a aBase.aType) = x

	end

structure OpStr = Op (structure aBase = TranBase)
val _ = OpStr.id 
