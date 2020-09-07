# purehp

A pre-alpha PHP backend for PureScript!




## TODO

- [ ] Literals
  Crashes somewhere with a wrong pattern match.
- [x] Variables
- [ ] Data constructors
  The old implementation is not working anymore since we can't have a class
  inside another class. Can we pull them out? How will imports work then?
- [x] Functions
- [ ] if/else
- [ ] case
- [ ] let
  I'm not sure whether the current implementation makes any sense.
- [ ] typeclasses
- [x] Functions
- [x] if/else
- [x] case
- [x] let
- [ ] typeclasses
  This has just become completely broken.
- [ ] exceptions
- [x] imports
- [x] qualified imports
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
