test:
	stack test --fast --file-watch --ghc-options='-j4 +RTS -A128m -n2m -qg -RTS'

test-accept:
	stack test --fast --test-arguments="--accept" --ghc-options='-j4 +RTS -A128m -n2m -qg -RTS'
