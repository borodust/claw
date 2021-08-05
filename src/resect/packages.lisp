(uiop:define-package :claw.resect
  (:use #:cl #:alexandria #:claw.util #:claw.spec)
  (:export #:ignore-functions
           #:ignore-names
           #:ignore-some
           #:ignore-every
           #:ignore-not

           #:declaration-name
           #:declaration-namespace
           #:declaration-location
           #:declaration-template-parameters))
