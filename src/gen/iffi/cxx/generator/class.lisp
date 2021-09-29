(cl:in-package :claw.iffi.cxx)


(defun parameterizedp (entity)
  (if (claw.spec:foreign-envelope-p entity)
      (parameterizedp (claw.spec:foreign-enveloped-entity entity))
      (when (claw.spec:foreign-parameterizable-p entity)
        (not (null (claw.spec:foreign-entity-parameters entity))))))


(defun anonymous-branch-p (entity)
  (or (when (claw.spec:foreign-envelope-p entity)
        (anonymous-branch-p (claw.spec:foreign-enveloped-entity entity)))
      (and (claw.spec:foreign-named-p entity)
           (emptyp (claw.spec:foreign-entity-name entity)))
      (when-let ((owner (claw.spec:foreign-owner entity)))
        (anonymous-branch-p owner))))


(defun mangle-full-record-name (entity)
  (if-let ((mangled (claw.spec:foreign-entity-mangled-name entity)))
    mangled
    (let* ((mangled
             (substitute #\P #\* (claw.spec:format-full-foreign-entity-name entity)))
           (mangled (substitute #\R #\&  mangled))
           (mangled (substitute #\0 #\<  mangled))
           (mangled (substitute #\1 #\>  mangled))
           (mangled (ppcre:regex-replace-all "[:\\(\\),\\s]+" mangled "W")))
      (ppcre:regex-replace-all "\\W" mangled "_"))))


(defun adapt-reporter (entity reporter)
  (make-instance 'adapted-function
                 :name (format nil "~A_~A" reporter (mangle-full-record-name entity))
                 :namespace (claw.spec:foreign-entity-namespace entity)
                 :parameters nil
                 :result-type (unsigned-long-long)
                 :body (format nil "return ~A(~A);"
                               reporter
                               (claw.spec:format-full-foreign-entity-name entity))))


(defun adapt-setter (record field)
  (let* ((field-name (claw.spec:foreign-entity-name field))
         (original-type (claw.spec:foreign-enveloped-entity field))
         (unaliased (claw.spec:unalias-foreign-entity original-type)))
    (multiple-value-bind (field-type adapted-p)
        (adapt-type original-type)
      (unless (or (typep unaliased 'claw.spec:foreign-array)
                  (typep unaliased 'claw.spec:foreign-const-qualifier))
        (make-instance 'adapted-function
                       :name (format nil "set_~A_~A"
                                     (mangle-full-record-name record)
                                     field-name)
                       :namespace (claw.spec:foreign-entity-namespace record)
                       :parameters (list (parameter "__claw_this_" (pointer record))
                                         (parameter "__claw_value_" field-type))
                       :result-type (void)
                       :body (format nil "~A~%__claw_this_->~A = ~@[~A~]__claw_value_;"
                                     (format-location-comment record)
                                     field-name
                                     (when adapted-p
                                       "*")))))))


(defun adapt-array-for-result (result-type)
  (if (typep result-type 'claw.spec:foreign-array)
      (adapt-array-for-result (claw.spec:foreign-enveloped-entity result-type))
      (pointer result-type)))


(defun adapt-getter (record field)
  (let ((field-name (claw.spec:foreign-entity-name field)))
    (multiple-value-bind (field-type adapted-p)
        (adapt-type (claw.spec:foreign-enveloped-entity field))
      (let* ((unaliased (claw.spec:unalias-foreign-entity field-type))
             (array-p (typep unaliased 'claw.spec:foreign-array))
             (result-type (if array-p
                              (adapt-array-for-result field-type)
                              field-type)))
        (make-instance 'adapted-function
                       :name (format nil "get_~A_~A"
                                     (mangle-full-record-name record)
                                     field-name)
                       :namespace (claw.spec:foreign-entity-namespace record)
                       :parameters (list (parameter "__claw_this_" (pointer record)))
                       :result-type result-type
                       :body (format nil "~A~%return ~@[(~A)~]~@[~A~]__claw_this_->~A;"
                                     (format-location-comment record)
                                     (when (or array-p
                                               (typep (claw.spec:unqualify-foreign-entity unaliased)
                                                      'claw.spec:foreign-pointer))
                                       (claw.spec:format-foreign-entity-c-name result-type))
                                     (when adapted-p
                                       "&")
                                     field-name))))))


(defun symbolicate-record-name (entity)
  (let ((formatted (claw.spec:format-full-foreign-entity-name entity)))
    (c-name->lisp (if (emptyp formatted)
                      (claw.spec:foreign-entity-id entity)
                      formatted)
                  :type)))


(defun symbolicate-function-name (entity)
  (let* ((owner-needed-p (and (typep entity 'claw.spec:foreign-method)
                              (claw.spec:foreign-method-static-p entity)))
         (full-name (claw.spec:format-full-foreign-entity-name entity
                                                               :include-method-owner owner-needed-p)))
    (c-name->lisp full-name :type)))


(defun generate-record-binding (define entity &key (with-superclasses t))
  (unless (anonymous-branch-p entity)
    (let ((name (symbolicate-record-name entity))
          (id (claw.spec:foreign-entity-id entity))
          sizeof-cname
          alignof-cname)
      (when (and (not (claw.spec:foreign-entity-private-p entity))
                 (not (anonymous-branch-p entity))
                 (> (claw.spec:foreign-entity-bit-size entity) 0))
        (setf sizeof-cname (register-adapted-function (adapt-reporter entity "sizeof"))
              alignof-cname (register-adapted-function (adapt-reporter entity "alignof"))))
      (export-symbol name)
      `((,define (,name :size-reporter ,sizeof-cname
                        :alignment-reporter ,alignof-cname
                        ,@(when-let ((ctor (find-constructor id)))
                            `(:constructor ,(symbolicate-function-name ctor)))
                        ,@(when-let ((dtor (find-destructor id)))
                            `(:destructor ,(symbolicate-function-name dtor)))
                        ,@(unless *inline-functions*
                            `(:inline nil)))
            ,@(when with-superclasses
                '(()))
          ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))
          ,@(unless (claw.spec:foreign-entity-template-p entity)
              (loop for field in (claw.spec:foreign-record-fields entity)
                    for known-p = (call-shielded-from-unknown
                                   (lambda ()
                                     (check-entity-known field)
                                     t))
                    unless (or (anonymous-branch-p (claw.spec:foreign-enveloped-entity field))
                               (not known-p))
                      collect (let* ((adapted-setter (adapt-setter entity field))
                                     (setter-cname (when adapted-setter
                                                     (register-adapted-function adapted-setter)))
                                     (adapted-getter (adapt-getter entity field))
                                     (getter-cname (when adapted-getter
                                                     (register-adapted-function adapted-getter)))
                                     (field-name (c-name->lisp
                                                  (claw.spec:foreign-entity-name field) :field)))
                                (export-symbol field-name)
                                `(,field-name
                                  ,(entity->cffi-type (adapted-function-result-type adapted-getter))
                                  :setter ,setter-cname
                                  :getter ,getter-cname
                                  :documentation ,(claw.spec:format-foreign-location
                                                   (claw.spec:foreign-entity-location field)))))))))))


(defun adapt-function-class-instance-ctor (entity)
  (let ((function-proto (claw.spec:foreign-entity-value
                         (first (claw.spec:foreign-entity-arguments entity)))))
    (make-instance 'adapted-function
                   :name (string+ (mangle-full-record-name entity) "_ctor")
                   :namespace (claw.spec:foreign-entity-namespace entity)
                   :parameters (list (parameter "__claw_this_"
                                                (pointer entity))
                                     (parameter "__claw_callback_"
                                                (pointer function-proto)))
                   :result-type (void-pointer)
                   :body (format nil "return new (__claw_this_) ~A(__claw_callback_);"
                                 (claw.spec:format-full-foreign-entity-name entity)))))


(defun adapt-function-class-instance-dtor (entity)
  (make-instance 'adapted-function
                 :name (string+ (mangle-full-record-name entity) "_dtor")
                 :namespace (claw.spec:foreign-entity-namespace entity)
                 :parameters (list (parameter "__claw_this_"
                                              (pointer entity)))
                 :result-type (void)
                 :body "__claw_this_->~function();"))


(defun generate-function-class-binding (entity)
  (let* ((name (symbolicate-record-name entity))
         (adapted-ctor (register-adapted-function (adapt-function-class-instance-ctor entity)))
         (adapted-dtor (register-adapted-function (adapt-function-class-instance-dtor entity)))
         (fun-proto (claw.spec:foreign-entity-value (first (claw.spec:foreign-entity-arguments entity))))
         (adapted-proto (adapt-function
                         (make-instance 'claw.spec:foreign-function
                                        :id (string+
                                             (claw.spec:foreign-entity-id entity)
                                             "_claw_fc")
                                        :source (claw.spec:foreign-entity-source fun-proto)
                                        :name (claw.spec:foreign-entity-name entity)
                                        :namespace (claw.spec:foreign-entity-namespace entity)
                                        :location (claw.spec:foreign-entity-location entity)
                                        :mangled (string+
                                                  (claw.spec:foreign-entity-mangled-name entity)
                                                  "_claw_fc")
                                        :result-type (claw.spec:foreign-function-result-type fun-proto)
                                        :parameters (claw.spec:foreign-function-parameters fun-proto))
                         :mode :c++)))
    (export-symbol name)
    `((iffi:define-intricate-function-class (,name
                                             :constructor ,adapted-ctor
                                             :destructor ,adapted-dtor
                                             :size-reporter ,(register-adapted-function
                                                              (adapt-reporter entity "sizeof"))
                                             :alignment-reporter ,(register-adapted-function
                                                                   (adapt-reporter entity "alignof")))
          ,(entity->cffi-type (adapted-function-result-type adapted-proto))
        ,(claw.spec:format-full-foreign-entity-name entity)
        ,@(generate-adapted-parameters adapted-proto)))))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-class) &key)
  (if (and (string= "std" (claw.spec:foreign-entity-namespace entity))
           (starts-with-subseq "function<" (claw.spec:foreign-entity-name entity)))
      (generate-function-class-binding entity)
      (generate-record-binding 'iffi:deficlass entity)))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-struct) &key)
  (generate-record-binding 'iffi:defistruct entity))


(defmethod generate-binding ((generator iffi-generator) (entity claw.spec:foreign-union) &key)
  (generate-record-binding 'iffi:defiunion entity :with-superclasses nil))


(defmethod generate-forward-declaration ((generator iffi-generator)
                                         (entity claw.spec:foreign-record)
                                         &key)
  `((iffi:defirecord ,(symbolicate-record-name entity) ()
      "forward declaration")))
