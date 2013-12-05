input-from-file:
	sml -Cprint.depth=1000 -Cparser.quotations=true sources.cm < test-input.sml

interactive:
	sml -Cprint.depth=1000 -Cparser.quotations=true sources.cm 

compile:
	sml -m sources.cm < /dev/null
