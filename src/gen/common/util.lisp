(cl:in-package :claw.generator.common)


(declaim (special *spec*
                  *dependency-type-list*
                  *visit-table*
                  *forward-declaration-table*
                  *dependency-list*
                  *trim-enum-prefix-p*
                  *adapter*
                  *export-table*
                  *pointer-type*
                  *recognize-strings*
                  *recognize-bitfields*
                  *recognize-arrays*
                  *use-float-features*
                  *float-features-requested*
                  *inline-functions*
                  *override-table*
                  *entities*
                  *adapted-function-table*
                  *always-generate-adapter*
                  *anonymous-names*))

(defvar *qualify-records* t)

(defvar *use-overriden-types* t)


(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'string=)


(define-constant +adapted-function-prefix+ "__claw_"
  :test #'string=)


(defvar *void*
  (make-instance 'claw.spec:foreign-primitive
                 :id "void"
                 :name "void"
                 :bit-size 0
                 :bit-alignment 0))

(defvar *void-pointer*
  (make-instance 'claw.spec:foreign-pointer
                 :enveloped *void*))

(defvar *unsigned-long-long*
  (make-instance 'claw.spec:foreign-primitive
                 :id "unsigned long long"
                 :name "unsigned long long"
                 :bit-size (* (cffi:foreign-type-size :unsigned-long-long) 8)
                 :bit-alignment (* (cffi:foreign-type-alignment :unsigned-long-long) 8)))


(defgeneric adapted-function-name (function))
(defgeneric adapted-function-namespace (function))
(defgeneric adapted-function-parameters (function))
(defgeneric adapted-function-result-type (function))
(defgeneric adapted-function-body (function))
(defgeneric adapted-function-entity (function))


(defun export-symbol (symbol)
  (setf (gethash symbol *export-table*) symbol))


(defun unexport-symbol (symbol)
  (remhash symbol *export-table*))


(defun get-overriden-type (type)
  (gethash type *override-table* type))


(defun adapter ()
  *adapter*)


(defun expand-constant (name value)
  (let ((name (format-symbol (or (package-name (symbol-package name))
                                 *package*) "+~A+" name)))
    (export-symbol name)
    `((defparameter ,name ,value))))


(defun name->cffi-type (name)
  (switch (name :test #'string=)
    ("bool" :bool)
    ("char" :char)
    ("signed char" :char)
    ("unsigned char" :unsigned-char)
    ("short" :short)
    ("unsigned short" :unsigned-short)
    ("int" :int)
    ("unsigned int" :unsigned-int)
    ("long" :long)
    ("unsigned long" :unsigned-long)
    ("long long" :long-long)
    ("unsigned long long" :unsigned-long-long)
    ("float" :float)
    ("double" :double)
    ("long double" (c-name->lisp :long-double :type))
    ("void" :void)
    (t (c-name->lisp name :type))))


(defun entity->iffi-type (entity &key (qualify-records *qualify-records*) ((:const-qualified const-qualified-p) nil))
  (let ((*qualify-records* qualify-records))
    (labels ((%enveloped-entity ()
               (claw.spec:unalias-foreign-entity (claw.spec:foreign-enveloped-entity entity)))
             (%enveloped-char-p ()
               (let ((unwrapped (claw.spec:unwrap-foreign-entity entity)))
                 (and (typep unwrapped 'claw.spec:foreign-primitive)
                      (string= "char" (claw.spec:foreign-entity-name unwrapped)))))
             (%lisp-name ()
               (name->cffi-type (let ((full-name (and (claw.spec:foreign-named-p entity)
                                                      (claw.spec:format-full-foreign-entity-name entity))))
                                  (if (emptyp full-name)
                                      (claw.spec:foreign-entity-id entity)
                                      full-name)))))
      (typecase entity
        (claw.spec:foreign-pointer (if (and *recognize-strings* (%enveloped-char-p))
                                       :string
                                       (list* :pointer
                                              (entity->iffi-type (%enveloped-entity))
                                              (when const-qualified-p
                                                (list :const)))))
        (claw.spec:foreign-reference `(:reference
                                       ,(entity->iffi-type
                                         (%enveloped-entity))
                                       ,@(when const-qualified-p
                                           (list :const))
                                       ,@(when (claw.spec:foreign-reference-rvalue-p entity)
                                           (list :rvalue))))
        (claw.spec:foreign-array (let ((dimensions (claw.spec:foreign-array-dimensions entity)))
                                   (cond
                                     ((and (= (length dimensions) 1) (%enveloped-char-p))
                                      :string)
                                     ((and dimensions *recognize-arrays*)
                                      (list* :array
                                             (entity->iffi-type (%enveloped-entity))
                                             (apply #'* dimensions)
                                             (when const-qualified-p
                                               (list :const))))
                                     (t
                                      (list* :pointer
                                             (entity->iffi-type (%enveloped-entity))
                                             (when const-qualified-p
                                               (list :const)))))))
        (claw.spec:foreign-struct (cond
                                    (*qualify-records*
                                     `(,:struct ,(%lisp-name) ,@(when const-qualified-p
                                                                  (list :const))))
                                    (const-qualified-p
                                     (list (%lisp-name) :const))
                                    (t (%lisp-name))))
        (claw.spec:foreign-union (cond
                                   (*qualify-records*
                                    `(,:union ,(%lisp-name) ,@(when const-qualified-p
                                                                (list :const))))
                                   (const-qualified-p
                                    (list (%lisp-name) :const))
                                   (t (%lisp-name))))
        (claw.spec:foreign-class (if const-qualified-p
                                     (list (%lisp-name) :const)
                                     (%lisp-name)))
        (claw.spec:foreign-const-qualifier (entity->iffi-type
                                            (%enveloped-entity)
                                            :const-qualified t))
        (claw.spec:foreign-function (%lisp-name))
        (claw.spec:foreign-function-prototype :void)
        (t (if const-qualified-p
               (list (%lisp-name) :const)
               (%lisp-name)))))))


(defun iffi->cffi-type (iffi-type &key ((:use-overriden-types use-overriden-types) *use-overriden-types*))
  (let ((*use-overriden-types* use-overriden-types))
    (flet ((%overtype (type)
             (if *use-overriden-types*
                 (get-overriden-type type)
                 type))
           (%unqualify (iffi-type)
             (remove-if (lambda (value) (member value '(:const :rvalue))) (ensure-list iffi-type))))
      (destructuring-bind (kind &optional type &rest opts) (ensure-list iffi-type)
        (case kind
          (:string :string)
          (:pointer (if type
                        (list (%overtype :pointer)
                              (iffi->cffi-type type))
                        :pointer))
          (:reference (if type
                          (list (%overtype :pointer) (iffi->cffi-type type))
                          :pointer))
          (:array (destructuring-bind (&optional dimensions &rest other) opts
                    (declare (ignore other))
                    (list* (%overtype :array)
                           (iffi->cffi-type type)
                           (when dimensions
                             (list dimensions)))))
          (:struct (list (%overtype :struct) (iffi->cffi-type type)))
          (:union (list (%overtype :union) (iffi->cffi-type type)))
          (:void (%overtype :void))
          (otherwise (let ((unqualified-type (%unqualify iffi-type)))
                       (if (null (rest unqualified-type))
                           (first unqualified-type)
                           unqualified-type))))))))


(defun entity->cffi-type (entity &key (qualify-records *qualify-records*) (use-overriden-types t))
  (iffi->cffi-type
   (entity->iffi-type entity :qualify-records qualify-records)
   :use-overriden-types use-overriden-types))


(defun void ()
  *void*)

(defun void-pointer ()
  *void-pointer*)

(defun unsigned-long-long ()
  *unsigned-long-long*)

(defun pointer (entity)
  (make-instance 'claw.spec:foreign-pointer :enveloped entity))

(defun parameter (name entity)
  (make-instance 'claw.spec:foreign-parameter :name name :enveloped entity))


(defun register-anonymous-name (symbol)
  (setf (gethash symbol *anonymous-names*) t))
