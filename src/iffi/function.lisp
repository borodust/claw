(cl:in-package :iffi)


(defvar *doc-table* (make-hash-table))
(defvar *intricate-table* (make-hash-table))
(defvar *function-pointer-extractor-table* (make-hash-table :test 'equal))
(defvar *function-table* (make-hash-table :test 'equal))


;;;
;;; FUNCTION
;;;
(defun form-function-signature (name params-and-qualifiers)
  (multiple-value-bind (qualifiers params)
      (loop for param in params-and-qualifiers
            if (member param '(:const :rvalue :volatile))
              collect param into qualifiers
            else
              collect param into params
            finally (return (values qualifiers params)))
    (list* name (sort qualifiers #'string<) params)))


(defun intricate-function (name &rest param-types)
  (let ((fun-sig (form-function-signature name param-types)))
    (gethash fun-sig *function-table*)))


(defun (setf intricate-function) (value name &rest param-types)
  (let* ((fun-sig (form-function-signature name param-types)))
    (setf (gethash fun-sig *function-table*) value)))


(defun (setf intricate-documentation) (docstring name &rest arg-types)
  (setf (assoc-value (gethash name *doc-table*) arg-types :test #'equal) docstring))


(defun ensure-documentation (name)
  (flet ((format-docs ()
           (with-output-to-string (out)
             (let ((*print-case* :downcase))
               (loop for (types . doc) in (gethash name *doc-table*)
                     do (format out "(")
                        (prin1 name out)
                        (loop for type in types
                              do (format out "~&  '") (prin1 type out))
                        (format out ")")
                        (format out "~&~A~&~%" doc))))))
    (setf (documentation (symbol-function name) t) (format-docs))))


(defun (setf intricate-function-pointer-extractor) (value name &rest arg-types)
  (let ((intricate-function-name (apply #'intricate-function name arg-types)))
    (unless intricate-function-name
      (error "Intricate function uknown: ~A ~A" name arg-types))
    (setf (gethash intricate-function-name *function-pointer-extractor-table*) value)))


(defun intricate-function-pointer (name &rest arg-types)
  (let ((intricate-function-name (apply #'intricate-function name arg-types)))
    (unless intricate-function-name
      (error "Intricate function uknown: ~A ~A" name arg-types))
    (when-let ((extractor (gethash intricate-function-name *function-pointer-extractor-table*)))
      (funcall extractor))))


(define-compiler-macro intricate-function-pointer (&whole whole name &rest arg-types)
  (let* ((unquoted-arg-types (mapcar #'find-quoted arg-types))
         (intricate-function-name (apply #'intricate-function (find-quoted name) unquoted-arg-types)))
    (if (not intricate-function-name)
        whole
        (if-let ((extractor (gethash intricate-function-name *function-pointer-extractor-table*)))
          `(,extractor)
          whole))))


(defun intricate-funcall (name &rest args)
  (let* ((const-p (eq (first args) :const))
         (args (if const-p (rest args) args)))
    (loop for (type . rest) on args by #'cddr
          unless rest
            do (error "Odd number of arguments: ~A" args)
          collect type into arg-types
          collect (first rest) into arg-values
          finally (return (let ((arg-types (append (when const-p
                                                     (list :const))
                                                   arg-types)))
                            (if-let ((fu (apply #'intricate-function name arg-types)))
                              (apply fu arg-values)
                              (error "Intricate function with signature ~A ~A not found" name arg-types)))))))


(define-compiler-macro intricate-funcall (&whole whole name &rest args)
  (let* ((const-p (eq (first args) :const))
         (args (if const-p (rest args) args)))
    (loop for (type . rest) on args by #'cddr
          unless rest
            do (error "Odd number of arguments: ~A" args)
          collect (if-let ((actual (find-quoted type)))
                    actual
                    (progn
                      (warn "Passing argument types dynamically is discouraged. Invocation: ~A " whole)
                      (return whole)))
            into arg-types
          collect (first rest) into arg-values
          finally (return
                    (if-let ((quoted (find-quoted name)))
                      (let ((arg-types (append (when const-p
                                                 (list :const))
                                               arg-types)))
                        (if-let ((function (apply #'intricate-function quoted arg-types)))
                          `(,function ,@arg-values)
                          (progn
                            (warn "Function ~A is not defined for parameters ~A " quoted arg-types)
                            whole)))
                      whole)))))


(defun expand-intricate-function-body (name arguments)
  `(intricate-funcall ',name ,@arguments))


(defun format-defifun-documentation (return-type param-config doc)
  (if (null param-config)
      doc
      (with-output-to-string (out)
        (let ((*print-case* :downcase))
          (format out "Adapted result: ")
          (prin1 return-type out)
          (format out "~&Adapted parameters:")
          (loop for (name type) in param-config
                do (format out "~&")
                   (prin1 type out)
                   (format out " ")
                   (princ name out))
          (format out "~&~%~A" doc)))))


(defmacro defifun (name-and-options return-type &body configuration)
  (destructuring-bind (mangled name &rest opts) (ensure-list name-and-options)
    (let (doc
          param-config
          cffi-opts
          pointer-extractor
          const-p
          (inline-p t))
      (if (stringp (first configuration))
          (setf doc (first configuration)
                param-config (rest configuration))
          (setf param-config configuration))
      (loop for (name value) on opts by #'cddr
            do (case name
                 (:pointer-extractor (setf pointer-extractor value))
                 (:non-mutating (setf const-p value))
                 (:inline (setf inline-p value))
                 (t (setf cffi-opts (list* name value cffi-opts)))))
      (let* ((signed-param-types
               (append
                (when const-p
                  '(:const))
                (loop for param in param-config
                      if (eq param '&rest)
                        collect '&rest
                      else
                        collect
                        (destructuring-bind (cffi-type
                                             &key ((:signed-as iffi-type)) &allow-other-keys)
                            (rest param)
                          (let ((type (or iffi-type cffi-type)))
                            (if (keywordp type)
                                type
                                `(quote ,type)))))))
             (intricately-defined (gethash name *intricate-table*))
             (cfun-name (format-symbol (symbol-package name) "~A~A$~A" 'iffi-cfun$ name mangled)))
        `(progn
           ,@(when inline-p
               `((declaim (inline ,cfun-name))))
           (cffi:defcfun (,mangled ,cfun-name ,@(nreverse cffi-opts)) ,return-type
             ,@(when doc
                 (list doc))
             ,@(loop for (param-name type) in param-config
                     collect `(,param-name ,type)))
           (meta-eval
             (setf (intricate-function ',name ,@signed-param-types) ',cfun-name))
           ,@(when pointer-extractor
               (let ((extractor-cfun-name (symbolicate cfun-name '$pointer-extractor)))
                 `(,@(when inline-p
                       `((declaim (inline ,extractor-cfun-name))))
                   (cffi:defcfun (,pointer-extractor ,extractor-cfun-name) :pointer)
                   (meta-eval
                     (setf
                      (intricate-function-pointer-extractor ',name ,@signed-param-types)
                      ',extractor-cfun-name)))))
           ,@(when (or (not intricately-defined)
                       (equal intricately-defined (or signed-param-types t)))
               (setf (gethash name *intricate-table*) (or signed-param-types t))
               `((defun ,name (&rest args)
                   (apply #'intricate-funcall ',name args))
                 (define-compiler-macro ,name (&rest arguments)
                   (expand-intricate-function-body ',name arguments))))
           ,@(when (and doc
                        (not (member :iffi-skip-documentation *features*)))
               `((meta-eval
                   (setf (intricate-documentation ',name ,@signed-param-types)
                         ,(format-defifun-documentation return-type
                                                        param-config
                                                        doc)))
                 (ensure-documentation ',name))))))))
