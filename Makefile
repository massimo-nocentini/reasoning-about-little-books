tests:
	sml -Cprint.depth=1000 -Cparser.quotations=true sources.cm < test-input.sml

scratch:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ctdp.instrument=true   sources.cm < scratch-input.sml

interactive:
	sml -Cprint.depth=1000 -Cparser.quotations=true sources.cm 

compile:
	sml -m sources.cm < /dev/null
