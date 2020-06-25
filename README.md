# purehp

A pre-alpha PHP backend for PureScript!

## How to test for now
- cd into `tests`
- `stack build`
- `spago install`
- edit `src/Main.purs` as needed
- `purs compile src/Main.purs --codegen corefn`
- optional `purs compile src/Main.purs` to check the js output
- `stack exec -- purehp`
- manually check `output/Main/Main.php`

This could be done with spago too, but it will also compile all the dependencies and purehp right now is not able to handle them.

e.g.
- `spago build -u '--codegen corefn'`



## TODO

- [x] Numers/Strings/Bools
- [x] Variables
- [x] Data constructors
- [x] Functions
- [x] if/else
- [x] case
- [ ] let
- [ ] typeclasses
- [ ] exceptions
- [ ] imports
- [ ] qualified imports
- [ ] working prelude
- [ ] ffi
- [ ] optimization
- [ ] packages
- [ ] tooling

