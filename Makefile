.PHONY: build
build:
	corebuild -pkgs yojson,ANSITerminal -I src/ tim.native

.PHONY: test
test:
	corebuild -pkgs yojson,ANSITerminal,kaputt -Is src/,test/ timSummaryTest.native && ./timSummaryTest.native
