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
- [ ] exceptions
- [x] imports
- [x] qualified imports
- [ ] working prelude
- [ ] ffi
- [ ] optimization
- [ ] packages
- [ ] tooling

## OPTIMIZATIONS

Moving exported functions out of the constructor as static function would improve performance by ~20%.
