# Helper for Unit test for Bucklescript

This library provides helper functions that are base on Mocha as testing framework and Chai as assertion library.

This inspired [bs-mocha](https://github.com/BuckleScript/bucklescript-addons/tree/master/bindings/bs-mocha), but more simple and append some features.

* Asynchronous test

## Requirements ##

* Mocha
* Chai

Each libraries must bind to ``global`` or ``window`` context. So I recommends karma and karma-mocha, karma-chai when you want to use this library with.

## Example ##

```ocaml
open Bs_testing

let _ = 

  suite "test suite" [
    (* Sync is synchronized test *)
    Sync ("equal numbers", fun _ -> assert_eq 1 1);
    Sync ("greater than", fun _ -> assert_ok (1.0 < 1.5));
  ];
```

## Build ##

```
npm run build
```

## Watch ##

```
npm run watch
```

## Test ##

```
npm run test
```

## License ##
MIT
