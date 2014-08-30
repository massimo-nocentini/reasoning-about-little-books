tests:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ccm.debug=true sources.cm < test-input.sml

scratch:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ctdp.instrument=true -Ccm.debug=true sources.cm < scratch-input.sml

interactive:
	sml -Cprint.depth=1000 -Cparser.quotations=true -Ccm.debug=true sources.cm 

compile:
	rm -rf .cm/
	sml -m sources.cm < /dev/null

tags:
	rm -f tags
	ctags-exuberant -R --languages=sml
