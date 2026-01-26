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
* Build `libresect.so` by following instructions in [`libresect`](https://github.com/borodust/libresect) repo

#### Usage
`claw` itself is confirmed to work on `SBCL`, `CCL` and `ECL`. Generated bindings do not depend on `claw` system and can be used anywhere `CFFI` works.

In repl:
```common-lisp
;; preload libresect.so
(pushnew :claw-regen-adapter *features*)
(ql:quickload :cffi)
(cffi:load-foreign-library "<path-to>/libresect/build/resect/libresect.so")
```

Now you can tinker with existing wrappers to play with `claw` beta version
E.g. [claw-git2](https://github.com/borodust/claw-git2)
```common-lisp
(ql:quickload :claw-git2/wrapper)
(claw:load-wrapper :claw-git2)
```
To regen bindings:
```common-lisp
(claw:generate-wrapper :claw-git2)
```
