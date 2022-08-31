.PHONY: test-chibi, test-gauche

test-chibi:
	chibi-scheme -I . srfi-222-test.scm

test-gauche:
	gosh -I . srfi-222-test.scm
