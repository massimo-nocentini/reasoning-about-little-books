(* this file works essentially as a test runner, so every suite we
would like to exercise should appear as ``val _ = ...'' assigment as
done below: *)

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (SExpressionsTests.suite ())

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (SexpFunctionsTests.suite ())

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (LittleSchemerTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (LittleMLerTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (TableTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (SchemeInterpreterTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (SeasonedSchemerTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (MutableStorageTests.suite ());

val _ = SMLUnit.TextUITestRunner.runTest
	    {output = TextIO.stdOut}
	    (YcombinatorImperativeTests.suite ());



