(cl:in-package :claw.resect)


(defgeneric parse-instantiation (kind type))


(defun parse-instantiation-by-kind (instantiation-type)
  (parse-instantiation (%resect:type-kind instantiation-type) instantiation-type))


(defun parse-template-instantiations (maybe-root-template-decl)
  (when (and (not (cffi:null-pointer-p maybe-root-template-decl))
             (%resect:declaration-template-p maybe-root-template-decl))
    (resect:docollection (specialization-type
                          (%resect:declaration-template-specializations maybe-root-template-decl))
      (let ((specialization-decl (%resect:type-declaration specialization-type)))
        (when (zerop
               (%resect:collection-size
                (%resect:declaration-template-parameters specialization-decl)))
          (parse-instantiation-by-kind specialization-type))))))


(defun ensure-method-mangled-name (method)
  (let ((mangled (unless-empty (%resect:type-method-mangled-name method))))
    (or mangled (mangle-id (%resect:type-method-id method)))))


(defun parse-instantiated-method-parameters (parameters)
  (let (params)
    (resect:docollection (param-type parameters)
      (push (make-instance 'foreign-parameter
                           :name nil
                           :mangled nil
                           :location (make-instance 'foreign-location
                                                    :path ""
                                                    :line 0
                                                    :column 0)
                           :enveloped (ensure-const-type-if-needed
                                       param-type
                                       (parse-type-by-category param-type)))

            params))
    (nreverse params)))


(defun cast-operator-p (pure-method-name result-type)
  (and (foreign-named-p result-type)
       (starts-with-subseq "operator " pure-method-name)
       (string= pure-method-name
                (string+ "operator "
                         (remove-template-argument-string (foreign-entity-name result-type))))))


(defun parse-instantiated-record-type (record-type)
  (let* ((record-decl (%resect:type-declaration record-type))
         (entity (parse-new-record-declaration (%resect:declaration-kind record-decl) record-decl)))
    (let ((*current-owner* entity))
      (resect:docollection (method (%resect:type-methods record-type))
        (let* ((method-type (%resect:type-method-type method))
               (mangled-name (ensure-method-mangled-name method))
               (pure-method-name (remove-template-argument-string
                                  (%resect:type-method-name method)))
               (pure-entity-name (remove-template-argument-string
                                  (foreign-entity-name entity)))
               (constructor-p (string= pure-method-name pure-entity-name))
               (result-type (ensure-const-type-if-needed
                             (%resect:function-proto-result-type method-type)
                             (parse-type-by-category
                              (%resect:function-proto-result-type method-type)))))
          (multiple-value-bind (method newp)
              (register-entity 'foreign-method
                               :id (%resect:type-method-id method)
                               :source (%resect:type-method-source method)
                               :name (cond
                                       (constructor-p
                                        (foreign-entity-name entity))

                                       ((cast-operator-p pure-method-name
                                                         result-type)
                                        (string+ "operator "
                                                 (foreign-entity-name result-type)))

                                       (t (%resect:type-method-name method)))
                               :owner entity
                               :namespace (claw.spec:foreign-entity-namespace
                                           entity)
                               :mangled mangled-name
                               :location (make-instance 'foreign-location
                                                        :path ""
                                                        :line 0
                                                        :column 0)

                               :result-type result-type
                               :parameters (parse-instantiated-method-parameters (%resect:function-proto-parameters method-type))
                               :variadic (%resect:function-proto-variadic-p method-type)
                               :static (%resect:type-method-static-p method)
                               :const (%resect:type-const-qualified-p method-type)
                               :template nil)
            (when newp
              (setf (gethash mangled-name *mangled-table*) method))))))))


(defmethod parse-instantiation ((kind (eql :record)) type)
  (parse-instantiated-record-type type))
