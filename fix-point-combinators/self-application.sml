

functor SelfApplication () :> SELF_APPLICATION = struct

	datatype 'a T = Into of 'a T -> 'a

	end

functor SelfApplicationOperators (
    structure SelfApplication : SELF_APPLICATION)
	:> SELF_APPLICATION_OPERATORS where type 'a T = 'a SelfApplication.T
	=
	struct

	type 'a T = 'a SelfApplication.T

	fun G0 (into as SelfApplication.Into aFn) = 
	    aFn into ()
	fun G1 (into as SelfApplication.Into aFn) x1 = 
	    aFn into x1
	fun G2 (into as SelfApplication.Into aFn) x1 x2 = 
	    aFn into x1 x2
	fun G3 (into as SelfApplication.Into aFn) x1 x2 x3 = 
	    aFn into x1 x2 x3
	fun G4 (into as SelfApplication.Into aFn) x1 x2 x3 x4 = 
	    aFn into x1 x2 x3 x4
	fun G5 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 = 
	    aFn into x1 x2 x3 x4 x5
	fun G6 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 x6 =
	    aFn into x1 x2 x3 x4 x5 x6 
	fun G7 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 x6 x7 = 
	    aFn into x1 x2 x3 x4 x5 x6 x7
	fun G8 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 = 
	    aFn into x1 x2 x3 x4 x5 x6 x7 x8
	fun G9 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 x9 = 
	    aFn into x1 x2 x3 x4 x5 x6 x7 x8 x9
	fun G10 (into as SelfApplication.Into aFn) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = 
	    aFn into x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 


	end
