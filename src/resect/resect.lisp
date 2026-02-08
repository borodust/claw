(cl:in-package :claw.resect)


(declaim (special *declaration-table*
                  *instantiated-table*
                  *mangled-table*
                  *post-parse-hooks*))

(defvar *parsed-pointers* nil)
(defvar *current-owner* nil)

(define-constant +va-list-id+ "va_list"
  :test #'string=)

(defparameter *const-qualified-source-scanner* (ppcre:create-scanner "\\W*const\\W+"))

(defgeneric parse-declaration (kind declaration &key &allow-other-keys))


(defgeneric parse-type (category kind type))


(defun find-instantiated-type-from-owner (type-entity)
  (let* ((type-owner (foreign-owner type-entity))
         (instantiated (when (and *current-owner*
                                  type-owner
                                  (foreign-entity-template-p type-owner)
                                  (string= (format-full-foreign-entity-name type-owner)
                                           (remove-template-argument-string
                                            (format-full-foreign-entity-name *current-owner*))))
                         (loop for dependent in (dependents-of *current-owner*)
                               when (string= (foreign-entity-name type-entity)
                                             (foreign-entity-name dependent))
                                 return dependent))))
    (or instantiated type-entity)))


(defmethod parse-declaration :around (kind decl &key)
  (declare (ignore kind))
  (if (and (string= +va-list-id+ (%resect:declaration-name decl))
           (emptyp (%resect:declaration-namespace decl)))
      (register-custom-primitive +va-list-id+)
      (call-next-method)))


(defun const (entity)
  (make-instance 'claw.spec:foreign-const-qualifier :enveloped entity))


(defmethod parse-type :around (category kind type)
  (declare (ignorable category kind type))
  (let ((result (if (string= +va-list-id+ (%resect:type-name type))
                    (register-custom-primitive +va-list-id+)
                    (call-next-method))))
    (if (%resect:type-const-qualified-p type)
        (const result)
        result)))


(defun write-uber-header (headers path defines &optional text)
  (alexandria:with-output-to-file (out path :if-exists :supersede)
    (format out "#ifndef  __CLAW_UBERHEADER~%#define __CLAW_UBERHEADER 1~%")
    (loop for (name value) on defines by #'cddr
          do (format out "~%#define ~A~@[ ~A~]" name value))
    (loop for header in headers
          do (format out "~%#include \"~A\"" header))
    (when text
      (format out "~%")
      (format out "~A" text))
    (format out "~%~%~%#endif")))


(defclass foreign-library ()
  ((entities :initarg :entities
             :initform (error ":entities missing")
             :reader claw.wrapper:foreign-library-entities)
   (language :initarg :language
             :initform (error ":language missing")
             :reader claw.wrapper:foreign-library-language)))


(defun parse-declaration-by-kind (decl &optional from-type)
  (when (cffi:null-pointer-p decl)
    (error "Type has no assigned declaration. Was it excluded by libresect?"))
  (parse-declaration (%resect:declaration-kind decl) decl :from-type from-type))


(defun parse-type-by-category (type)
  (parse-type (%resect:type-category type) (%resect:type-kind type) type))


(defclass describing-inspector ()
  ((language :initform nil :reader language-of)))


(defmethod inspect-foreign-library :before ((this describing-inspector)
                                            header-path
                                            includes frameworks
                                            language standard target
                                            intrinsics
                                            &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (with-slots (language) this
    (setf language (case (%resect:translation-unit-language *translation-unit*)
                     (:c :c)
                     (:c++ :c++)
                     (:obj-c :objective-c)))))


(defmethod inspect-foreign-library :around ((this describing-inspector)
                                            header-path
                                            includes frameworks
                                            language standard target
                                            intrinsics
                                            &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (let (*post-parse-hooks*)
    (call-next-method)))


(defmethod inspect-foreign-library :after ((this describing-inspector)
                                           header-path
                                           includes frameworks
                                           language standard target
                                           intrinsics
                                           &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (loop for hook in (reverse *post-parse-hooks*)
        do (funcall hook)))


(defmethod inspect-declaration ((this describing-inspector) kind declaration)
  (parse-declaration kind declaration))


(defmethod claw.wrapper:describe-foreign-library ((parser (eql :claw/resect))
                                                  headers &key
                                                            includes
                                                            frameworks
                                                            language
                                                            standard
                                                            target
                                                            defines
                                                            intrinsics
                                                            instantiation-filter

                                                            include-definitions
                                                            include-sources
                                                            exclude-definitions
                                                            exclude-sources
                                                            enforce-definitions
                                                            enforce-sources
                                                            ignore-definitions
                                                            ignore-sources)
  (declare (ignore parser))
  (with-temporary-directory (:pathname prepared-dir)
    (uiop:with-temporary-file (:pathname uber-path :type "h")
      (write-uber-header headers uber-path defines)
      (multiple-value-bind (prepared-headers macros)
          (prepare-foreign-library uber-path
                                   prepared-dir
                                   includes
                                   frameworks
                                   language
                                   standard
                                   target
                                   intrinsics
                                   instantiation-filter
                                   :include-definitions include-definitions
                                   :include-sources include-sources
                                   :exclude-definitions exclude-definitions
                                   :exclude-sources exclude-sources
                                   :enforce-definitions enforce-definitions
                                   :enforce-sources enforce-sources
                                   :ignore-definitions ignore-definitions
                                   :ignore-sources ignore-sources)
        (let ((*declaration-table* (make-hash-table :test 'equal))
              (*instantiated-table* (make-hash-table :test 'equal))
              (*mangled-table* (make-hash-table :test 'equal))
              (inspector (make-instance 'describing-inspector)))
          (loop for header in prepared-headers
                do (inspect-foreign-library inspector
                                            header
                                            includes
                                            frameworks
                                            language
                                            standard
                                            target
                                            intrinsics
                                            :include-definitions include-definitions
                                            :include-sources include-sources
                                            :exclude-definitions exclude-definitions
                                            :exclude-sources exclude-sources
                                            :enforce-definitions enforce-definitions
                                            :enforce-sources enforce-sources
                                            :ignore-definitions ignore-definitions
                                            :ignore-sources ignore-sources))
          (loop for constant in (prepare-macros-as-constants uber-path
                                                             includes
                                                             frameworks
                                                             target
                                                             macros
                                                             intrinsics)
                do (register-entity-instance constant))
          (make-instance 'foreign-library
                         :entities (hash-table-values *declaration-table*)
                         :language (language-of inspector)))))))


(defmacro on-post-parse (&body body)
  `(push (lambda () ,@body) *post-parse-hooks*))


(defun instantiated-p (decl)
  (and (not (cffi:null-pointer-p (%resect:declaration-template decl)))
       (or (> (%resect:type-size (%resect:declaration-type decl)) 0)
           (%resect:declaration-forward-p decl))))


(defun derive-instantiated-id-from-type (type)
  (flet ((args-for-known ()
           (loop for arg in (extract-type-arguments type)
                 for parsed = (if (cffi:pointerp arg)
                                  (%resect:type-name arg)
                                  (eval-template-argument arg))
                 if (null parsed)
                   do (return nil)
                 else
                   collect parsed))
         (args-for-unknown ()
           (loop for arg in (extract-template-literals type)
                 for evaluated = (eval-template-argument arg)
                 for parsed = (if (cffi:pointerp evaluated)
                                  (%resect:type-name evaluated)
                                  evaluated)
                 if (null parsed)
                   do (return nil)
                 else
                   collect parsed)))
    (let ((args (if (eq :unknown (%resect:type-kind type))
                    (args-for-unknown)
                    (args-for-known)))
          (decl (%resect:type-declaration type)))
      (when (and args (not (cffi:null-pointer-p decl)))
        (format nil "~A~A"
                (%resect:declaration-id (root-template (%resect:type-declaration type)))
                (format-template-argument-string args))))))


(defun eval-instantiated-id-from-decl (decl)
  (let ((args (loop for param in (extract-decl-parameters decl)
                    for arg = (eval-template-argument (%resect:declaration-name param))
                    unless arg
                      return nil
                    collect (if (cffi:pointerp arg)
                                (%resect:type-name arg)
                                arg))))
    (when args
      (format nil "~A~A"
              (%resect:declaration-id (root-template decl))
              (format-template-argument-string args)))))


(defun instantiated-id (decl)
  (if (instantiated-p decl)
      (format nil "~A~A"
              (%resect:declaration-id (%resect:declaration-template decl))
              (reformat-template-argument-string-from-type (%resect:declaration-type decl)))
      (%resect:declaration-id decl)))


(defun register-instantiated (entity decl)
  (when (instantiated-p decl)
    (setf (gethash (instantiated-id decl) *instantiated-table*) entity)))


(defun find-instantiated (name)
  (gethash name *instantiated-table*))


(defun decorate-id-if-instantiated-owner (decl owner)
  (format nil "~A~@[<~A>~]"
          (%resect:declaration-id decl)
          (when (and owner
                     (foreign-entity-arguments owner)
                     (not (foreign-entity-template-p owner))
                     (> (foreign-entity-bit-size owner) 0))
            (format-full-foreign-entity-name owner))))


;;;
;;; UTIL
;;;
(defun find-entity (id)
  (gethash id *declaration-table*))


(defun register-entity (entity-class &rest args &key id &allow-other-keys)
  (if-let ((existing (find-entity id)))
    (values existing nil)
    (values (setf (gethash id *declaration-table*) (apply #'make-instance entity-class args)) t)))


(defun register-entity-instance (entity)
  (let ((id (foreign-entity-id entity)))
    (if-let ((existing (find-entity id)))
      (values existing nil)
      (values (setf (gethash id *declaration-table*) entity) t))))


(defun register-custom-primitive (name)
  (register-entity 'foreign-primitive
                   :id name
                   :name name
                   :bit-size 0
                   :bit-alignment 0))


(defun make-declaration-location (declaration)
  (let ((location (%resect:declaration-location declaration)))
    (make-instance 'foreign-location
                   :path (uiop:ensure-pathname (%resect:location-name location))
                   :line (%resect:location-line location)
                   :column (%resect:location-column location))))


(defun parse-owner (decl)
  (let ((owner (%resect:declaration-owner decl)))
    (unless (cffi:null-pointer-p owner)
      (parse-declaration-by-kind owner))))


(defun specializationp (type)
  (resect:docollection (template-arg (%resect:type-template-arguments type))
    (declare (ignore template-arg))
    (return-from specializationp t)))


(defun reformat-template-argument-string-from-type (type)
  (let ((args (loop for arg in (extract-type-arguments type)
                    collect (if (cffi:pointerp arg)
                                (let ((*tag-types* nil))
                                  (format-foreign-entity-c-name (parse-type-by-category arg)))
                                arg))))
    (format-template-argument-string args)))

;;;
;;; PRIMITIVE
;;;
(defun register-void ()
  (register-custom-primitive "void"))


(defun register-primitive-resect-type (kind type)
  (flet ((register-primitive-type (name)
           (register-entity 'foreign-primitive
                            :id name
                            :name name
                            :plain-old-data-type t
                            :bit-size (%resect:type-size type)
                            :bit-alignment (%resect:type-alignment type))))
    (ecase kind
      (:void (register-void))
      (:bool (register-primitive-type "bool"))
      (:unsigned-char (register-primitive-type "unsigned char"))
      (:signed-char (register-primitive-type "signed char"))
      (:char16 (register-primitive-type "char16_t"))
      (:char32 (register-primitive-type "char32_t"))
      (:unsigned-short (register-primitive-type "unsigned short"))
      (:unsigned-int (register-primitive-type "unsigned int"))
      (:unsigned-long (register-primitive-type "unsigned long"))
      (:unsigned-long-long (register-primitive-type "unsigned long long"))
      (:unsigned-int128 (register-primitive-type "uint128"))
      (:char-s (register-primitive-type "char"))
      (:char-u (register-primitive-type "char"))
      (:wchar (register-primitive-type "wchar_t"))
      (:short (register-primitive-type "short"))
      (:int (register-primitive-type "int"))
      (:long (register-primitive-type "long"))
      (:long-long (register-primitive-type "long long"))
      (:int128 (register-primitive-type "int128"))
      (:float (register-primitive-type "float"))
      (:double (register-primitive-type "double"))
      (:long-double (register-primitive-type "long double"))
      (:nullptr (register-primitive-type "std::nullptr_t"))
      (:float128 (register-primitive-type "float128"))
      (:half (register-primitive-type "half"))
      (:float16 (register-primitive-type "float16"))
      (:auto (register-primitive-type "auto"))
      (:atomic (register-primitive-type "atomic"))
      (:complex (register-primitive-type "std::complex")))))


(defmethod parse-type ((category (eql :arithmetic)) kind type)
  (declare (ignore category))
  (register-primitive-resect-type kind type))


(defmethod parse-type ((category (eql :aux)) kind type)
  (declare (ignore category))
  (register-primitive-resect-type kind type))


;;;
;;; ENUM
;;;
(defmethod parse-declaration ((type (eql :enum)) decl &key)
  (let* ((decl-type (%resect:declaration-type decl))
         (owner (parse-owner decl))
         (value-alist))
    (resect:docollection (decl (%resect:enum-constants decl))
      (push (cons (%resect:declaration-name decl)
                  (if (%resect:enum-constant-unsigned-p decl)
                      (%resect:enum-constant-unsigned-value decl)
                      (%resect:enum-constant-value decl)))
            value-alist))
    (multiple-value-bind (entity registeredp)
        (register-entity 'foreign-enum
                         :id (decorate-id-if-instantiated-owner decl owner)
                         :source (%resect:declaration-source decl)
                         :owner owner
                         :name (%resect:declaration-name decl)
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled (%resect:declaration-mangled-name decl)
                         :location (make-declaration-location decl)
                         :bit-size (%resect:type-alignment decl-type)
                         :bit-alignment (%resect:type-alignment decl-type)
                         :plain-old-data-type (%resect:type-plain-old-data-p decl-type)
                         :type (parse-type-by-category (%resect:enum-type decl))
                         :values (nreverse value-alist))
      (when (and registeredp owner)
        (add-dependent owner entity))
      (find-instantiated-type-from-owner entity))))


(defmethod parse-type (category (kind (eql :enum)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; TEMPLATE PARAMETER
;;;
(defmethod parse-declaration ((kind (eql :template-parameter)) decl &key (inject-arguments t))
  (let* ((name (%resect:declaration-name decl))
         (arg (eval-template-argument name)))
    (if (and inject-arguments arg (cffi:pointerp arg))
        (parse-type-by-category arg)
        (let ((description (list :id (%resect:declaration-id decl)
                                 :source (%resect:declaration-source decl)
                                 :name name
                                 :namespace (unless-empty
                                             (%resect:declaration-namespace decl))
                                 :mangled (%resect:declaration-mangled-name decl)
                                 :location (make-declaration-location decl))))
          (apply #'make-instance
                 (if (eq :non-type (%resect:template-parameter-kind decl))
                     (list* 'foreign-entity-value-parameter
                            :type (parse-type-by-category (%resect:declaration-type decl))
                            description)
                     (list* 'foreign-entity-type-parameter description)))))))


(defmethod parse-type (category (kind (eql :template-parameter)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))

;;;
;;; RECORD
;;;
(defclass resect-record ()
  ((fields :initform nil :accessor fields-of)
   (args :initform nil :accessor arguments-of)
   (params :initform nil :accessor parameters-of)
   (deps :initform nil :accessor dependents-of)
   (parents :initform nil :accessor parents-of)))

(defmethod foreign-record-fields ((this resect-record))
  (slot-value this 'fields))

(defmethod foreign-entity-arguments ((this resect-record))
  (slot-value this 'args))

(defmethod foreign-entity-parameters ((this resect-record))
  (slot-value this 'params))

(defmethod foreign-record-parents ((this resect-record))
  (slot-value this 'parents))

(defmethod foreign-dependent ((this resect-record))
  (slot-value this 'deps))

(defgeneric add-field (record field)
  (:method ((this resect-record) field)
    (with-slots (fields) this
      (unless (member (foreign-entity-name field) fields :key #'foreign-entity-name)
        (nconcf fields (list field))))))

(defgeneric add-dependent (record dependent)
  (:method ((this resect-record) dependent)
    (with-slots (deps) this
      (push dependent deps))))

(defclass resect-struct (resect-record foreign-struct) ())

(defclass resect-union (resect-record foreign-union) ())

(defclass resect-class (resect-record foreign-class) ())


(defun collect-entity-parameters (decl)
  (let (params)
    (resect:docollection (param-decl (%resect:declaration-template-parameters decl))
      (push (parse-declaration (%resect:declaration-kind param-decl) param-decl :inject-arguments nil)
            params))
    (nreverse params)))


(defun %collect-entity-arguments (args params)
  (loop for param in params
        for arg in args
        collect (make-instance 'claw.spec:foreign-entity-argument
                               :parameter param
                               :value (if (cffi:pointerp arg)
                                          (parse-type-by-category arg)
                                          (cond
                                            ((numberp arg) arg)
                                            ((string= "true" arg) t)
                                            ((string= "false" arg) nil)
                                            (t (handler-case
                                                   (parse-number:parse-number arg)
                                                 (t () arg))))))))


(defun collect-record-entity-arguments (decl)
  (when (instantiated-p decl)
    (let ((args (extract-type-arguments (%resect:declaration-type decl)))
          (params (foreign-entity-parameters
                   (parse-declaration-by-kind (root-template decl)))))
      (%collect-entity-arguments args params))))


(defun extract-function-template-arguments (decl)
  (let (template-arguments)
    ;; template-arguments
    (resect:docollection (arg (%resect:declaration-template-arguments decl))
      (push arg template-arguments))
    (nreversef template-arguments)

    (let ((values (loop for arg in template-arguments
                        for value = (ecase (%resect:template-argument-kind arg)
                                      ((:type :declaration :template :template-expansion)
                                       (%resect:type-name (%resect:template-argument-type arg)))
                                      ((:integral :expression :pack)
                                       (%resect:template-argument-value arg)))
                        collect value))
          (parent (%resect:declaration-template decl)))
      (if (cffi:null-pointer-p parent)
          values
          (append values (extract-function-template-arguments parent))))))


(defun collect-function-entity-arguments (decl)
  (unless (cffi:null-pointer-p (%resect:declaration-template decl))
    (let ((args (extract-function-template-arguments decl))
          (params (foreign-entity-parameters
                   (parse-declaration-by-kind (root-template decl)))))
      (%collect-entity-arguments args params))))


(defun collect-entity-arguments (decl)
  (if (eq :function (%resect:declaration-kind decl))
      (collect-function-entity-arguments decl)
      (collect-record-entity-arguments decl)))


(defun nested-pointer-p (type)
  (flet ((%any-pointer-p (type)
           (member (%resect:type-kind type) '(:pointer :rvalue-reference :lvalue-reference))))
    (or (and (eq (%resect:type-kind type) :pointer)
             (%any-pointer-p (%resect:pointer-pointee-type type)))
        (and (member (%resect:type-kind type) '(:rvalue-reference :lvalue-reference))
             (%any-pointer-p (%resect:reference-pointee-type type))))))

(defun ensure-const-type-if-needed (type entity &optional decl)
  (cond
    ((or (typep entity 'foreign-const-qualifier)
         (typep (unwrap-foreign-entity entity) 'foreign-function-prototype))
     entity)
    ((and (not (nested-pointer-p type))
          (or
           ;; crazy, maybe a bug in libclang
           (starts-with-subseq "const " (%resect:type-name type))
           ;; i don't like this at all
           ;; better to use lexer in resect
           (and decl (starts-with-subseq "const " (%resect:declaration-source decl)))))
     (if (foreign-envelope-p entity)
         (claw.spec:rewrap-foreign-envelope entity
                                            (const (claw.spec:foreign-enveloped-entity entity)))
         (const entity)))
    (t entity)))


(defun decorate-if-instantiated-record (decl)
  (let ((name (%resect:declaration-name decl)))
    (format nil "~A~@[~A~]"
            name
            (when (and (not (emptyp name))
                       (instantiated-p decl))
              (reformat-template-argument-string-from-type (%resect:declaration-type decl))))))


(defun postfix-decorate (name postfix)
  (format nil "~A~@[~A~]" name postfix))


(defun ensure-method-mangled-name (type-method postfix)
  (let* ((method-type (%resect:type-method-prototype type-method))
         (method-decl (%resect:type-declaration method-type)))
    (or (unless (cffi:null-pointer-p method-decl)
          (postfix-decorate (ensure-mangled method-decl) postfix))
        (unless-empty (%resect:type-method-mangled-name type-method))
        (mangle-id (%resect:type-method-id type-method)))))


(defun parse-instantiated-method-parameters (parameters parameter-decls)
  (let (params param-names)
    (unless (cffi:null-pointer-p parameter-decls)
      (resect:docollection (param-decl parameter-decls)
        (push (%resect:declaration-name param-decl) param-names))
      (setf param-names (nreverse param-names)))

    (resect:docollection (param-type parameters)
      (let ((param-name (first param-names)))
        (setf param-names (rest param-names))
        (push (make-instance 'foreign-parameter
                             :name param-name
                             :mangled nil
                             :location (make-instance 'foreign-location
                                                      :path ""
                                                      :line 0
                                                      :column 0)
                             :enveloped (ensure-const-type-if-needed
                                         param-type
                                         (parse-type-by-category param-type)))

              params)))
    (nreverse params)))


(defun cast-operator-p (pure-method-name result-type)
  (and (foreign-named-p result-type)
       (starts-with-subseq "operator " pure-method-name)
       (string= pure-method-name
                (string+ "operator "
                         (remove-template-argument-string (foreign-entity-name result-type))))))


(defun parse-methods (entity record-decl &key postfix ((:type record-type)))
  (let ((*current-owner* entity)
        (record-type (or record-type (%resect:declaration-type record-decl))))
    (resect:docollection (type-method (%resect:type-methods record-type))
      (let* ((method-prototype (%resect:type-method-prototype type-method))
             (method-decl (%resect:type-method-declaration type-method))
             (mangled-name (ensure-method-mangled-name type-method postfix))
             (pure-method-name (remove-template-argument-string
                                (%resect:type-method-name type-method)))
             (pure-record-name (remove-template-argument-string
                                (foreign-entity-name entity)))
             (constructor-p (string= pure-method-name pure-record-name))
             (result-type (ensure-const-type-if-needed
                           (%resect:function-proto-result-type method-prototype)
                           (parse-type-by-category
                            (%resect:function-proto-result-type method-prototype)))))
        (multiple-value-bind (method newp)
            (register-entity 'foreign-method
                             :id (%resect:type-method-id type-method)
                             :kind (cond
                                     (constructor-p
                                      :constructor)
                                     ((starts-with #\~ pure-method-name)
                                      :destructor)
                                     ((starts-with-subseq "operator" pure-method-name)
                                      :operator)
                                     (t :regular))
                             :source (if (cffi:null-pointer-p method-decl)
                                         (%resect:type-method-source type-method)
                                         (%resect:declaration-source method-decl))
                             :name (cond
                                     (constructor-p
                                      (string+ pure-method-name
                                               (extract-template-argument-string
                                                (%resect:type-name record-type))))
                                     ((cast-operator-p pure-method-name
                                                       result-type)
                                      (string+ "operator "
                                               (foreign-entity-name result-type)))

                                     (t (%resect:type-method-name type-method)))
                             :owner entity
                             :namespace (unless-empty
                                         (if (cffi:null-pointer-p method-decl)
                                             (claw.spec:foreign-entity-namespace entity)
                                             (%resect:declaration-namespace method-decl)))
                             :mangled mangled-name
                             :location (if (cffi:null-pointer-p method-decl)
                                           (make-instance 'foreign-location
                                                          :path ""
                                                          :line 0
                                                          :column 0)
                                           (make-declaration-location method-decl))
                             :result-type result-type
                             :parameters (parse-instantiated-method-parameters
                                          (%resect:function-proto-parameters method-prototype)
                                          (unless (cffi:null-pointer-p method-decl)
                                            (%resect:method-parameters method-decl)))
                             :variadic (%resect:function-proto-variadic-p method-prototype)
                             :static (%resect:type-method-static-p type-method)
                             :const (%resect:type-method-const-p type-method)
                             :template (if (cffi:null-pointer-p method-decl)
                                           nil
                                           (%resect:declaration-template-p method-decl)))
          (when newp
            (setf (gethash mangled-name *mangled-table*) method)))))))


(defun method-exists-p (decl)
  (resect:docollection (method-decl (%resect:record-methods decl))
    (declare (ignore method-decl))
    (return-from method-exists-p t)))


(defun ensure-inherited-fields (entity)
  (let* ((owner (foreign-owner entity))
         (fields (fields-of entity))
         (owner-has-field (when owner
                            (loop for owner-field in (foreign-record-fields owner)
                                  for unwrapped-type = (unwrap-foreign-entity owner-field)
                                    thereis (and (foreign-identified-p unwrapped-type)
                                                 (string= (foreign-entity-id unwrapped-type)
                                                          (foreign-entity-id entity)))))))
    (when (and (not (foreign-entity-name entity))
               (not owner-has-field)
               fields
               (typep owner 'resect-record))
      (loop for field in fields
            do (add-field owner field))
      ;; propagate further
      (ensure-inherited-fields owner))))


(defun parse-fields (entity decl)
  (let ((*current-owner* entity)
        fields)
    ;; FIXME: migrate to type-fields
    (resect:docollection (field-decl (%resect:record-fields decl))
      (when (publicp field-decl)
        (let ((field-type (%resect:declaration-type field-decl)))
          (push (make-instance 'foreign-record-field
                               :name (%resect:declaration-name field-decl)
                               :location (make-declaration-location field-decl)
                               :enveloped (ensure-const-type-if-needed
                                           field-type
                                           (parse-type-by-category field-type)
                                           field-decl)
                               :bit-size (%resect:type-size field-type)
                               :bit-alignment (%resect:type-alignment field-type)
                               :bit-offset (%resect:field-declaration-offset field-decl)
                               :bitfield-p (%resect:field-declaration-bitfield-p field-decl)
                               :bit-width (%resect:field-declaration-width field-decl))
                fields))))
    (setf (fields-of entity) (nreverse fields))
    (ensure-inherited-fields entity)))


(defun parse-new-record-declaration (record-kind decl &key ((:type record-type)))
  (labels ((collect-parents ()
             (let (parents)
               (resect:docollection (parent-type (%resect:record-parents decl))
                 (push (parse-type-by-category parent-type) parents))
               (nreverse parents))))
    (let ((owner (parse-owner decl)))
      (multiple-value-bind (entity registeredp)
          (let ((decl-type (%resect:declaration-type decl)))
            (register-entity (ecase record-kind
                               (:struct 'resect-struct)
                               (:union 'resect-union)
                               (:class 'resect-class))
                             :id (%resect:declaration-id decl)
                             :source (%resect:declaration-source decl)
                             :owner owner
                             :name (decorate-if-instantiated-record decl)
                             :namespace (unless-empty
                                         (%resect:declaration-namespace decl))
                             :mangled (%resect:declaration-mangled-name decl)
                             :location (make-declaration-location decl)
                             :bit-size (%resect:type-size decl-type)
                             :bit-alignment (%resect:type-alignment decl-type)
                             :plain-old-data-type (%resect:type-plain-old-data-p decl-type)
                             :abstract (%resect:record-abstract-p decl)
                             :private (or (foreign-entity-private-p owner)
                                          (not (publicp decl))
                                          (not (template-arguments-public-p decl)))
                             :forward (%resect:declaration-forward-p decl)
                             :template (%resect:declaration-template-p decl)))
        (when registeredp
          (when owner
            (add-dependent owner entity))
          (setf (parents-of entity) (collect-parents)
                (arguments-of entity) (collect-entity-arguments decl)
                (parameters-of entity) (collect-entity-parameters decl))
          (unless (foreign-entity-private-p entity)
            (unless (zerop (foreign-entity-bit-size entity))
              (on-post-parse
                (parse-fields entity decl)))
            (parse-template-instantiations decl)
            (register-instantiated entity decl)
            (on-post-parse
              (parse-methods entity decl :type record-type))))
        (find-instantiated-type-from-owner entity)))))


(defun find-instantiated-from-type (from-type)
  (and from-type (find-instantiated (derive-instantiated-id-from-type from-type))))


(defun find-instantiated-from-decl (decl)
  (find-instantiated (eval-instantiated-id-from-decl decl)))


(defun parse-record-declaration (record-kind decl from-type)
  (if-let ((instantiated (or (find-instantiated-from-type from-type)
                             (find-instantiated-from-decl decl))))
    instantiated
    (parse-new-record-declaration record-kind decl)))


(defmethod parse-declaration ((type (eql :struct)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :union)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-declaration ((type (eql :class)) decl &key from-type)
  (parse-record-declaration type decl from-type))


(defmethod parse-type (category (kind (eql :struct)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :union)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


(defmethod parse-type (category (kind (eql :class)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; FUNCTION
;;;
(defun parse-parameters (parameters)
  (let (params
        parsed-param-names
        (repeat-idx 0))
    (resect:docollection (param parameters)
      (let ((name (unless-empty (%resect:declaration-name param)))
            (param-type (%resect:declaration-type param)))
        (when name
          (when (member name parsed-param-names :test #'string=)
            (setf name (format nil "~A~A" name (incf repeat-idx))))
          (push name parsed-param-names))
        (push (make-instance 'foreign-parameter
                             :name name
                             :mangled (%resect:declaration-mangled-name param)
                             :location (make-declaration-location param)
                             :enveloped (ensure-const-type-if-needed
                                         param-type
                                         (parse-type-by-category param-type)))

              params)))
    (nreverse params)))


(defun parse-result-type (decl)
  (let ((type (if (eq :function (%resect:declaration-kind decl))
                  (%resect:function-result-type decl)
                  (%resect:method-result-type decl))))
    (ensure-const-type-if-needed
     type
     (parse-type-by-category type))))


(defun register-function (decl)
  (let ((id (%resect:declaration-id decl))
        (name (%resect:declaration-name decl))
        (mangled-name (ensure-mangled decl))
        (params (parse-parameters (%resect:function-parameters decl))))
    (multiple-value-bind (entity newp)
        (register-entity 'foreign-function
                         :id id
                         :source (%resect:declaration-source decl)
                         :name name
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled mangled-name
                         :location (make-declaration-location decl)
                         :result-type (parse-result-type decl)
                         :parameters params
                         :variadic (%resect:function-variadic-p decl)
                         :inlined (%resect:function-inlined-p decl)
                         :template (%resect:declaration-template-p decl)
                         :entity-parameters (collect-entity-parameters decl)
                         :entity-arguments (collect-entity-arguments decl))
      (when newp
        (parse-template-instantiations decl)
        (setf (gethash mangled-name *mangled-table*) entity))
      entity)))


(defun extract-function-instantiated-arguments (template result-type parameters)
  (declare (ignore parameters))
  ;; TODO: figure out all template arguments from instantiated function
  ;; but for now result-type will suffice (it might be non-deducible when
  ;; we create adapted function for it)


  (let ((template-parameters (foreign-entity-parameters template))
        (template-result-type (unwrap-foreign-entity (foreign-function-result-type template))))
    ;; TODO: only simplest case here, but we really should dig into type and
    ;; recreate the whole thing
    (when (and (= 1 (length template-parameters))
               (or (typep template-result-type 'foreign-entity-parameter)
                   (and (typep template-result-type 'unrecognized-type)
                        (string= (foreign-entity-name (first template-parameters)) (foreign-entity-name template-result-type)))))
      (list (make-instance 'foreign-entity-argument
                           :parameter (first template-parameters)
                           :value result-type)))))


(defun register-instantiated-function (template decl)
  (let ((result-type (parse-result-type decl))
        (params (parse-parameters (%resect:function-parameters decl))))
    (if (typep template 'foreign-method)
        (register-entity 'foreign-method
                         :id (%resect:declaration-id decl)
                         :name (foreign-entity-name template)
                         :namespace (foreign-entity-namespace template)
                         :source (foreign-entity-source template)
                         :mangled (ensure-mangled decl)
                         :location (foreign-entity-location template)
                         :result-type result-type
                         :parameters (rest params)
                         :owner (foreign-enveloped-entity (first params))
                         :variadic (foreign-function-variadic-p template)
                         :entity-parameters nil
                         :entity-arguments (extract-function-instantiated-arguments template
                                                                                    result-type
                                                                                    params))
        (register-entity 'foreign-function
                         :id (%resect:declaration-id decl)
                         :name (foreign-entity-name template)
                         :namespace (foreign-entity-namespace template)
                         :source (foreign-entity-source template)
                         :mangled (ensure-mangled decl)
                         :location (foreign-entity-location template)
                         :result-type result-type
                         :parameters params
                         :variadic (foreign-function-variadic-p template)
                         :entity-parameters nil
                         :entity-arguments (extract-function-instantiated-arguments template
                                                                                    result-type
                                                                                    params)))))


(defmethod parse-declaration ((type (eql :function)) decl &key)
  (unless (eq :static (%resect:function-storage-class decl))
    (if (starts-with-subseq +instantiation-prefix+ (%resect:declaration-name decl))
        (on-post-parse
          (let ((template-mangled-name (subseq (%resect:declaration-name decl)
                                               (length +instantiation-prefix+))))
            (if-let ((template (gethash template-mangled-name *mangled-table*)))
              (register-instantiated-function template decl)
              (warn "Template with mangled name ~A not found" template-mangled-name))))
        (register-function decl))))


(defmethod parse-type (category (kind (eql :function-prototype)) type)
  (declare (ignorable category kind))
  (let (params)
    (resect:docollection (param-type (%resect:function-proto-parameters type))
      (push (make-instance 'foreign-parameter
                           :enveloped (parse-type-by-category param-type))

            params))
    (make-instance 'foreign-function-prototype
                   :result-type (parse-type-by-category
                                 (%resect:function-proto-result-type type))
                   :parameters (nreverse params)
                   :variadic (%resect:function-proto-variadic-p type))))


(defmethod parse-type (category (kind (eql :function-no-prototype)) type)
  (declare (ignorable category kind))
  (make-instance 'foreign-function-prototype
                 :result-type (parse-type-by-category
                               (%resect:function-proto-result-type type))
                 :parameters nil ;; noproto functions have no parameters
                 :variadic nil))


;;;
;;; TYPEDEF
;;;
(defmethod parse-declaration ((kind (eql :typedef)) decl &key)
  (let* ((owner (parse-owner decl))
         (id (decorate-id-if-instantiated-owner decl owner))
         (enveloped (let ((aliased-type (%resect:typedef-aliased-type decl)))
                      (if-let ((instantiated (find-instantiated-from-type aliased-type)))
                        instantiated
                        (parse-type-by-category aliased-type)))))
    (multiple-value-bind (entity registeredp)
        (register-entity 'foreign-alias
                         :id id
                         :owner owner
                         :name (%resect:declaration-name decl)
                         :namespace (unless-empty
                                     (%resect:declaration-namespace decl))
                         :mangled (%resect:declaration-mangled-name decl)
                         :location (make-declaration-location decl)
                         :enveloped enveloped)
      (when (and registeredp owner)
        (add-dependent owner entity))
      (find-instantiated-type-from-owner entity))))


(defmethod parse-type (category (kind (eql :typedef)) type)
  (declare (ignore category kind))
  (parse-declaration-by-kind (%resect:type-declaration type) type))


;;;
;;; ARRAY
;;;
(defmethod parse-type ((category (eql :array)) kind type)
  (declare (ignore category kind))
  (make-instance 'foreign-array
                 :enveloped (parse-type-by-category (%resect:array-element-type type))
                 :size (let ((size (%resect:array-size type)))
                         (when (>= size 0)
                           (list size)))))

;;;
;;; POINTER
;;;
(defmethod parse-type ((category (eql :pointer)) kind type)
  (declare (ignorable category kind))
  (let ((pointee-type (%resect:pointer-pointee-type type)))
    (make-instance 'foreign-pointer
                   :owner (when (eql kind :member-pointer)
                            (let ((owner (%resect:member-pointer-owning-type type))
)
                              ;; libclang cannot handle templated member-pointers
                              (unless (cffi:null-pointer-p owner)
                                (parse-type-by-category owner))))
                   :enveloped (if (member (cffi:pointer-address type) *parsed-pointers*
                                          :test #'=)
                                  (register-void)
                                  (let ((*parsed-pointers* (cons (cffi:pointer-address type)
                                                                 *parsed-pointers*)))
                                    (parse-type-by-category pointee-type))))))


;;;
;;; REFERENCE
;;;
(defmethod parse-type ((category (eql :reference)) kind type)
  (declare (ignorable category kind))
  (let ((pointee-type (%resect:reference-pointee-type type)))
    (make-instance 'foreign-reference
                   :enveloped (parse-type-by-category pointee-type)
                   :rvalue (not (%resect:reference-lvalue-p type)))))


(defun const-qualified-source-p (source)
  (ppcre:all-matches *const-qualified-source-scanner* source))
;;;
;;; VARIABLE
;;;
(defmethod parse-declaration ((kind (eql :variable)) declaration &key)
  (let ((type (parse-type-by-category (%resect:variable-type declaration)))
        (name (%resect:declaration-name declaration))
        (location (make-declaration-location declaration))
        (source (%resect:declaration-source declaration)))
    (unless (or (starts-with-subseq +instantiation-prefix+ name)
                (eq :static (%resect:variable-storage-class declaration)))
      (let ((value (case (%resect:variable-kind declaration)
                     (:int (%resect:variable-to-int declaration))
                     (:float (%resect:variable-to-float declaration))
                     (:string (%resect:variable-to-string declaration))
                     (t nil))))
        (if (or (%resect:type-const-qualified-p
                 (%resect:variable-type declaration))
                (const-qualified-source-p source))
            (register-entity 'foreign-constant
                             :id name
                             :name name
                             :namespace (unless-empty
                                         (%resect:declaration-namespace declaration))
                             :source source
                             :value value
                             :location location)
            (register-entity 'foreign-variable
                             :id (%resect:declaration-id declaration)
                             :name name
                             :namespace (unless-empty
                                         (%resect:declaration-namespace declaration))
                             :location location
                             :source source
                             :value value
                             :type type
                             :external (eq :external (%resect:declaration-linkage declaration))))))))


;;;
;;; MACRO
;;;
;;; Macros are parsed as auto variables
;;;
(defmethod parse-declaration ((kind (eql :macro)) declaration &key)
  (declare (ignore kind declaration)))


;;;
;;; UNRECOGNIZED
;;;
(defclass unrecognized-entity (foreign-entity)
  ((name :initarg :name :reader foreign-entity-name)
   (kind :initarg :kind)))


(defmethod foreign-entity-unknown-p ((this unrecognized-entity))
  (declare (ignore this))
  t)

(defmethod format-foreign-entity-c-name ((this unrecognized-entity) &key &allow-other-keys)
  (foreign-entity-name this))

;;;
;;; UNRECOGNIZED TYPE
;;;
(defclass unrecognized-type (unrecognized-entity)
  ((category :initarg :category)
   (declaration :initarg :declaration)))


(defmethod print-object ((o unrecognized-type) s)
  (with-slots (name kind category) o
    (print-unreadable-object (o s :type t :identity nil)
      (format s "~A ~A ~A" category kind name))))


(defun make-unrecognized-type (type category kind)
  (let ((decl (%resect:type-declaration type)))
    (make-instance 'unrecognized-type :name (%resect:type-name type)
                                      :category category
                                      :kind kind
                                      :source (unless (cffi:null-pointer-p decl)
                                                (%resect:declaration-source decl))
                                      :declaration (unless (cffi:null-pointer-p decl)
                                                     (parse-declaration-by-kind decl)))))


(defun notice-unrecognized-type (category kind type)
  (warn "Failed to recognize type of ~A kind from ~A category: ~A"
        kind category (%resect:type-name type))
  (make-unrecognized-type type category kind))


(defmethod parse-type (category kind type)
  (let* ((decl (%resect:type-declaration type))
         (entity (unless (cffi:null-pointer-p decl)
                   (parse-declaration-by-kind decl type))))
    (if (or (not entity) (foreign-entity-unknown-p entity))
        (if (and (eq category :unknown)
                 (eq kind :unknown))
            (make-unrecognized-type type category kind)
            (notice-unrecognized-type category kind type))
        entity)))


(defmethod parse-type ((category (eql :aux)) (kind (eql :dependent)) type)
  (make-unrecognized-type type category kind))


;;;
;;; UNRECOGNIZED DECL
;;;
(defclass unrecognized-declaration (unrecognized-entity)
  ((location :initarg :location)))


(defmethod print-object ((o unrecognized-declaration) s)
  (with-slots (name kind location) o
    (print-unreadable-object (o s :type t :identity nil)
      (format s "~A ~A ~A" kind name location))))


(defun make-unrecognized-declaration (decl kind)
  (make-instance 'unrecognized-declaration
                 :name (%resect:declaration-name decl)
                 :location (make-declaration-location decl)
                 :source (%resect:declaration-source decl)
                 :kind kind))


(defmethod parse-declaration ((kind (eql :unknown)) declaration &key)
  (make-unrecognized-declaration declaration kind))


(defmethod parse-declaration (kind declaration &key)
  (warn "Failed to recognize declaration of ~A kind: ~A" kind (%resect:declaration-name declaration))
  (make-unrecognized-declaration declaration kind))
