(cl:defpackage :claw
  (:use :claw.util :claw.wrapper :claw.cffi.c)
  (:export #:defwrapper
           #:include
           #:load-wrapper
           #:generate-wrapper

           #:build-adapter
           #:initialize-adapter))
