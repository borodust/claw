(uiop:define-package :claw.resect
  (:use #:cl #:alexandria #:claw.util #:claw.spec)
  (:export #:ignore-functions
           #:ignore-names
           #:declaration-name
           #:declaration-namespace
           #:declaration-location
           #:declaration-template-parameters))
