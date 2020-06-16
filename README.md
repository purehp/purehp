# purehp

## How to test for now
- cd into `tests`
- `stack build`
- `spago install`
- edit `src/Main.purs` as needed
- `purs compile src/Main.purs --codegen corefn`
- optional `purs compile src/Main.purs` to check the js output
- `stack exec -- purehp`
- manually check `output/Main/Main.php`
- on successive `Main.purs` edits I usually do `rm -r output/Main` as sometimes files don't get overwritten (?)

This could be done with spago too, but it will also compile all the dependencies and purehp right now is not able to handle them.

e.g.
- `spago build -u '--codegen corefn'`



## TODO

- Fix missing `$` in function arg names
- Find a better implementation for a record?
- Data constructors
- if/else/etc
- add the prelude
- monads
