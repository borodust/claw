(cl:in-package :claw.iffi.cxx)


(defun adapt-variable-setter (variable)
  (multiple-value-bind (variable-type adapted-p)
      (adapt-type (check-entity-known (claw.spec:foreing-variable-type variable)))
    (let ((unaliased (claw.spec:unalias-foreign-entity variable-type)))
      (unless (or (typep unaliased 'claw.spec:foreign-array)
                  (typep unaliased 'claw.spec:foreign-const-qualifier))
        (make-instance 'adapted-function
                       :name (format nil "set_~A" (mangle-entity-name variable))
                       :namespace (claw.spec:foreign-entity-namespace variable)
                       :parameters (list (parameter "__claw_value_" variable-type))
                       :result-type (void)
                       :body (format nil "~A~%~A = ~@[~A~]__claw_value_;"
                                     (format-location-comment variable)
                                     (claw.spec:format-full-foreign-entity-name variable)
                                     (when adapted-p
                                       "*")))))))


(defun adapt-variable-getter (variable)
  (multiple-value-bind (variable-type adapted-p)
      (adapt-type (check-entity-known (claw.spec:foreing-variable-type variable)))
    (let* ((unaliased (claw.spec:unalias-foreign-entity variable-type))
           (array-p (typep unaliased 'claw.spec:foreign-array))
           (result-type (if array-p
                            (adapt-array-for-result variable-type)
                            variable-type)))
      (make-instance 'adapted-function
                     :name (format nil "get_~A" (mangle-entity-name variable))
                     :namespace (claw.spec:foreign-entity-namespace variable)
                     :parameters nil
                     :result-type result-type
                     :body (format nil "~A~%return ~@[(~A)~]~@[~A~]~A;"
                                   (format-location-comment variable)
                                   (when (or array-p
                                             (typep (claw.spec:unqualify-foreign-entity unaliased)
                                                    'claw.spec:foreign-pointer))
                                     (claw.spec:format-foreign-entity-c-name result-type))
                                   (when adapted-p
                                     "&")
                                   (claw.spec:format-full-foreign-entity-name variable))))))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-variable) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :variable))
         (adapted-getter (adapt-variable-getter entity))
         (getter-cname (register-adapted-function adapted-getter))
         (getter-name (format-symbol (symbol-package name) "~A~A" 'iffi-variable-getter$ name))
         (adapted-setter (adapt-variable-setter entity))
         (decorated-name (format-symbol (symbol-package name) (if adapted-setter "*~A*" "+~A+")
                                        name))
         (accessor-name (format-symbol (symbol-package name) "~A~A" 'iffi-variable$ name)))
    (export-symbol decorated-name)
    `((declaim (inline ,getter-name))
      (cffi:defcfun (,getter-cname ,getter-name)
          ,(entity->cffi-type (adapted-function-result-type adapted-getter))
        ,(claw.spec:format-foreign-location
          (claw.spec:foreign-entity-location entity)))
      (declaim (inline ,accessor-name))
      (defun ,accessor-name ()
        (,getter-name))
      ,@(when adapted-setter
          (multiple-value-bind (setter-cname setter-name)
              (when adapted-setter
                (values (register-adapted-function adapted-setter)
                        (format-symbol (symbol-package name) "~A~A"
                                       'iffi-variable-setter$ name)))
            (with-gensyms (value)
              `((cffi:defcfun (,setter-cname ,setter-name) :void
                  ,(claw.spec:format-foreign-location
                    (claw.spec:foreign-entity-location entity))
                  (,name ,(entity->cffi-type (claw.spec:foreign-enveloped-entity
                                              (first (adapted-function-parameters adapted-setter))))))
                (declaim (inline (setf ,accessor-name)))
                (defun (setf ,accessor-name) (,value)
                  (,setter-name ,value))))))
      (define-symbol-macro ,decorated-name
          (,accessor-name)))))
