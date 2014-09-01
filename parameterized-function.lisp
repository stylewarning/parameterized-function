;;;; parameterized-function.lisp
;;;; Copyright (c) 2013-2014 Robert Smith

(in-package #:parameterized-function)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-quoted-list-p (l &optional env)
    (declare (ignoreable env))
    (and (listp l)
         (typep l '(cons (member quote)))
         ;; This will check each symbol for constantness which isn't
         ;; what we want.
         ;;
         ;(every (lambda (x) (constantp x env)) (cadr l))
         ))
  
  (defun dispatch-table-name (name)
    (intern (format nil "%~A-DISPATCH-TABLE" (symbol-name name))))
  
  (defun generate-name-from-parameters (fn params &optional suffix)
    (intern (format nil "%~A-~{~A~^-~}~@[-~A~]"
                    (symbol-name fn)
                    (mapcar #'symbol-name params)
                    suffix)))
  
  (defun undispatch-form (form new-function-name)
    (cons new-function-name (cddr form))))


(defmacro define-dispatch-function (name (&rest parameters) (&rest args))
  (declare (ignore parameters))
  (let ((table-name (dispatch-table-name name))
        (params (gensym "PARAMS-")))
    `(progn
       (defvar ,table-name (make-hash-table :test 'equal))
       
       (define-compiler-macro ,name (&whole form params ,@args &environment env)
         (declare (ignore ,@args))
         (if (not (constant-quoted-list-p params env))
             (progn
               (warn "Non-constant parameters ~S in ~S" params form)
               form)
             (let* ((params (cadr params))    ; remove the QUOTE
                    (dispatch-function (gethash params ,table-name)))
               (if (null dispatch-function)
                   (progn
                     (warn "Unknown dispatch function for ~S with parameters ~S" ',name params)
                     form)
                   (undispatch-form form dispatch-function)))))

       ;; TODO: Get a proper lambda list here so it can be
       ;; introspected. Requires some extra processing.
       (defun ,name (,params ,@args)
         (let ((dispatch-function (gethash ,params ,table-name)))
           (unless dispatch-function
             (error "Parameters ~S are unknown to the dispatch function ~S" ,params ',name))
           (warn "Dynamic dispatch occuring in ~S" ',name)
           ,(intf:calling-form 'dispatch-function (cons params args)))))))

(defmacro define-parameterized-function (name (&rest parameters) (&rest args) &body body)
  "Define a parameterized function named NAME whose parameter variables are PARAMETERS, whose lambda list is ARGS, and body is BODY."
  (assert (every #'symbolp parameters)
          (parameters)
          "Every parameter variable must be a symbol. Given ~S."
          parameters)
  (let ((dispatch-table    (dispatch-table-name name))
        (dispatch-function (generate-name-from-parameters name parameters)))
    `(progn
       (unless (boundp ',dispatch-table)
         (error "Unknown dispatch function ~A." ',name))
       (declaim (inline ,dispatch-function))
       (defun ,dispatch-function (,@args)
         ,@body)
       
       (setf (gethash ',parameters ,dispatch-table) ',dispatch-function)
       
       ',name)))


