.PHONY: build
build:
	corebuild -pkgs yojson,ANSITerminal,str -I src/ tim.native

.PHONY: test
test:
	corebuild -pkgs yojson,ANSITerminal,kaputt,str -Is src/,test/ timSummaryTest.native && ./timSummaryTest.native
	corebuild -pkgs yojson,ANSITerminal,kaputt,str -Is src/,test/ timDateTest.native && ./timDateTest.native
