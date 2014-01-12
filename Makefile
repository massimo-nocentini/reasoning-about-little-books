tests:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ccm.debug=true sources.cm < test-input.sml

scratch:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ctdp.instrument=true -Ccm.debug=true sources.cm < scratch-input.sml

interactive:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ccm.debug=true sources.cm 

compile:
	rm -r .cm/
	sml -m sources.cm < /dev/null
