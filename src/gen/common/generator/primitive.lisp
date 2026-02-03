(cl:in-package :claw.generator.common)


(define-constant +emulated-primitives+ '("long double"
                                         "int128"
                                         "uint128"
                                         "float128"
                                         "std::nullptr_t"
                                         "__f16"
                                         "half")
  :test 'equal)


(define-constant +special-primitives+ '("va_list")
  :test 'equal)


(defun generate-primitive-byte-holder (type)
  (let* ((size (ceiling (/ (claw.spec:foreign-entity-bit-size type)
                           +byte-size+)))
         (name (entity->cffi-type type))
         (emulated-name (format-symbol (symbol-package name) "~A~A" 'emulated$ name))
         (this-name (format-symbol (symbol-package name) "~A" 'this)))
    (export-symbol name)
    `((cffi:defcstruct (,name :class ,emulated-name :size ,size)
        (,(format-symbol (symbol-package name) "~A" 'data) :unsigned-char :count ,size))
      (defmethod cffi:foreign-type-alignment ((,this-name ,emulated-name))
        (declare (ignore ,this-name))
        ,(/ (claw.spec:foreign-entity-bit-alignment type) +byte-size+))
      (cffi:defctype ,name (:struct ,name)))))


(defun emulated-primitive-p (entity)
  (and (claw.spec:foreign-named-p entity)
       (member (claw.spec:foreign-entity-name entity) +emulated-primitives+ :test #'string=)))


(defun special-primitive-p (entity)
  (and (claw.spec:foreign-named-p entity)
       (member (claw.spec:foreign-entity-name entity) +special-primitives+ :test #'string=)))


(defmethod generate-binding ((generator generator)
                             (type claw.spec:foreign-primitive)
                             &key)
  (let ((entity-name (claw.spec:foreign-entity-name type)))
    (switch (entity-name :test #'string=)
      ("wchar_t" (let ((name (entity->cffi-type type)))
                   `((cffi:defctype ,name ,(eswitch ((claw.spec:foreign-entity-bit-size type)
                                                     :test #'=)
                                             (8 :char)
                                             (16 :uint16)
                                             (32 :uint32)
                                             (64 :uint64))))))
      ("char16_t" `((cffi:defctype ,(entity->cffi-type type) ,:uint16)))
      ("char32_t" `((cffi:defctype ,(entity->cffi-type type) ,:uint32)))
      (t (cond
           ((emulated-primitive-p type)
            (generate-primitive-byte-holder type))
           ((special-primitive-p type)
            (eswitch (entity-name :test #'string=)
              ("va_list" (let ((name (entity->cffi-type type)))
                           `((cffi:defctype ,name (:pointer :void))))))))))))


(defmethod dependablep ((entity claw.spec:foreign-primitive))
  (declare (ignore entity))
  t)
