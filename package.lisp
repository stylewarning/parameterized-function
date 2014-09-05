;;;; package.lisp
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:parameterized-function
  (:use #:cl)
  (:export #:*warn-about-dynamic-dispatch*
                                        ; VARIABLE
           #:*warn-about-non-constant-parameters*
                                        ; VARIABLE
           #:define-dispatch-function   ; MACRO
           #:define-parameterized-function
                                        ; MACRO
           ))

