# CLAW

Still **BETA** quality. API is subject to change.

**C**ommon **L**isp **a**uto**w**rapping facility for quickly creating
clean&lean bindings to C/C++ libraries.

## Usage for the brave

#### Building

To use `claw` you need:
* [claw-utils](https://github.com/borodust/claw-utils/)
* [cl-resect](https://github.com/borodust/cl-resect)
* [libresect](https://github.com/borodust/libresect)

Steps:
* Clone `claw`, `claw-utils` and `cl-resect` into `~/quicklisp/local-projects`
* Build `libresect.so` by following instructions in `libresect` repo

#### Usage
`claw` (`libclang` actually) has known problems on `SBCL`, so `CCL`
recommended. You only need `CCL` to generate bindings. After that, generated
bindings can be used anywhere `CFFI` works.

In repl:
```common-lisp
;; preload libresect.so
(pushnew :claw-regen-adapter *features*)
(ql:quickload :cffi)
(cffi:load-foreign-library "<path-to>/libresect/build/resect/libresect.so")
```

Now you can tinker with existing wrappers to play with `claw` beta version
E.g. [claw-olm](https://github.com/borodust/claw-olm)
```common-lisp
(ql:quickload :claw-olm/wrapper)
(claw:load-wrapper :claw-olm/wrapper)
```
