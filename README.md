# purehp

A pre-alpha PHP backend for PureScript!




## TODO

- [x] Numers/Strings/Bools
- [x] Variables
- [ ] Data constructors
  With the new approach we can't use top level classes anymore. 
- [x] Functions
- [x] if/else
- [x] case
- [x] let
- [ ] typeclasses
  This has just become completely broken.
- [ ] exceptions
- [ ] imports
- [ ] qualified imports
- [ ] working prelude
- [ ] ffi
- [ ] optimization
- [ ] packages
- [ ] tooling

## Notes

- PVar handling it's a bit of a mess right now, and it's probably broken in case of nested values.
  It might make sense to store the type of PVar in an additional field. 
  The main difference is: scope variable, class variable, foreign variable. Some of these things are already handled by the current code.
  
- There's a LOT of duplication and potentially dead code. Wait to have a working final structure and then cleanup or rewrite.

- PIndexer / PArrayIndexer
  Just the PIndexer should be enough, but since there was a problem with matching with indexers and accessors, I decided to create the new data structure. Improve when possible.
  Moreover, an extra set of parentheses is being added right now. E.g. `($this->scope['something'])->foo(1)`;
