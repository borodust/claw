(cl:in-package :iffi)

(defvar *record-table* (make-hash-table))


;;;
;;; RECORD
;;;
(defclass intricate-field ()
  ((name :initarg :name :reader name-of)
   (getter :initarg :getter :reader getter-of)
   (setter :initarg :setter :reader setter-of)))


(defclass intricate-record ()
  ((name :initarg :name :reader name-of)
   (size-reporter :initarg :size-reporter :initform nil  :reader size-reporter-of)
   (alignment-reporter :initarg :alignment-reporter :initform nil :reader alignment-reporter-of)
   (constructor :initarg :constructor :initform nil :reader constructor-of)
   (destructor :initarg :destructor :initform nil :reader destructor-of)
   (field-map :initarg :field-map :initform (make-hash-table))))


(defun serialize-intricate-record (record)
  (with-slots (field-map) record
    (list :name (name-of record)
          :size (size-reporter-of record)
          :alignment (alignment-reporter-of record)
          :constructor (constructor-of record)
          :destructor (destructor-of record)
          :fields (loop for field being the hash-value of field-map
                        collect (list :name (name-of field)
                                      :getter (getter-of field)
                                      :setter (setter-of field))))))


(defun deserialize-intricate-record (record-data)
  (destructuring-bind (&key name size alignment constructor destructor fields)
      record-data
    (let ((field-map (loop with map = (make-hash-table)
                           for field in fields
                           do (destructuring-bind (&key name setter getter)
                                  field
                                (setf (gethash name map)
                                      (make-instance 'intricate-field :name name
                                                                      :getter getter
                                                                      :setter setter)))
                           finally (return map))))
      (make-instance 'intricate-record :name name
                                       :size-reporter size
                                       :alignment-reporter alignment
                                       :constructor constructor
                                       :destructor destructor
                                       :field-map field-map))))


(defun find-intricate-field (record field-name)
  (with-slots (field-map) record
    (gethash field-name field-map)))


(defun register-intricate-record (record-data)
  (let ((record (deserialize-intricate-record record-data)))
    (setf (gethash (name-of record) *record-table*) record)))


(defun find-intricate-record (name)
  (loop for alias in (list* name (find-intricate-aliases name))
          thereis (gethash alias *record-table*)))


(defun expand-record-field (record inline field)
  (destructuring-bind (field-name type &key
                                         setter
                                         getter
                       &allow-other-keys)
      field
    (when-let ((record-field (find-intricate-field record field-name)))
      (append
       (when setter
         (let ((cffi-setter (setter-of record-field)))
           `(,@(when inline
                 `((declaim (inline ,cffi-setter))))
             (cffi:defcfun (,setter ,cffi-setter) :void
               (this :pointer)
               (value ,type)))))
       (when getter
         (let ((cffi-getter (getter-of record-field)))
           `(,@(when inline
                 `((declaim (inline ,cffi-getter))))
             (cffi:defcfun (,getter ,cffi-getter) ,type
               (this :pointer)))))))))


(defun expand-record (record size-reporter alignment-reporter inline fields)
  (let ((cffi-size-reporter (size-reporter-of record))
        (cffi-alignment-reporter (alignment-reporter-of record)))
    `(,@(when inline
          `((declaim (inline ,cffi-size-reporter ,cffi-alignment-reporter))))
      (cffi:defcfun (,size-reporter ,cffi-size-reporter)
          :unsigned-long-long)
      (cffi:defcfun (,alignment-reporter ,cffi-alignment-reporter)
          :unsigned-long-long)
      ,@(loop for field in fields
              append (expand-record-field record inline field)))))


(defun make-field-map (record-name fields)
  (loop with table = (make-hash-table)
        for field in fields
        do (destructuring-bind (field-name type &key
                                                  setter
                                                  getter
                                                  documentation)
               field
             (declare (ignore type documentation))
             (let ((record-field (make-instance
                                  'intricate-field
                                  :name field-name
                                  :setter (when setter
                                            (format-symbol
                                             (symbol-package field-name)
                                             "~A$~A$~A"
                                             'iffi-set record-name field-name))
                                  :getter (when getter
                                            (format-symbol
                                             (symbol-package field-name)
                                             "~A$~A$~A"
                                             'iffi-get record-name field-name)))))
               (setf (gethash field-name table) record-field)))
        finally (return table)))


(defmacro defirecord (name-and-opts superclasses &body fields)
  (declare (ignore superclasses))
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (multiple-value-bind (doc fields)
        (if (stringp (first fields))
            (values (first fields) (rest fields))
            (values nil fields))
      (destructuring-bind (&key size-reporter alignment-reporter constructor destructor (inline t))
          opts
        (let ((record (make-instance 'intricate-record
                                     :name name
                                     :constructor constructor
                                     :destructor destructor
                                     :size-reporter (when size-reporter
                                                      (format-symbol (symbol-package name)
                                                                     "~A$~A" 'iffi-sizeof name))
                                     :alignment-reporter (when alignment-reporter
                                                           (format-symbol (symbol-package name)
                                                                          "~A$~A" 'iffi-alignof name))
                                     :field-map (make-field-map name fields))))
          `(progn
             (cffi:defctype ,name :void ,doc)
             ,@(when (and size-reporter alignment-reporter)
                 (expand-record record size-reporter alignment-reporter inline fields))
             (meta-eval
               (register-intricate-record ',(serialize-intricate-record record)))))))))


(defmacro defiunion (name-and-opts &body fields)
  `(defirecord ,name-and-opts nil ,@fields))


(defmacro defistruct (name-and-opts superclasses &body fields)
  `(defirecord ,name-and-opts ,superclasses ,@fields))


(defmacro deficlass (name-and-opts superclasses &body fields)
  `(defirecord ,name-and-opts ,superclasses ,@fields))


(defun intricate-size (name)
  (if-let ((intricate (find-intricate-record name)))
    (funcall (size-reporter-of intricate))
    (cffi:foreign-type-size name)))


(define-compiler-macro intricate-size (&whole whole name)
  (if-let (quoted (find-quoted name))
    (if-let ((intricate (find-intricate-record quoted)))
      `(,(size-reporter-of intricate))
      whole)
    whole))


(defun intricate-alignment (name)
  (if-let ((intricate (find-intricate-record name)))
    (funcall (alignment-reporter-of intricate))
    (cffi:foreign-type-size name)))


(define-compiler-macro intricate-alignment (&whole whole name)
  (if-let ((quoted (find-quoted name)))
    (if-let ((intricate (find-intricate-record quoted)))
      `(,(alignment-reporter-of intricate))
      whole)
    whole))


(defmacro with-field-setter ((field-setter type-name slot-name) &body body)
  (with-gensyms (intricate field)
    `(if-let ((,intricate (find-intricate-record ,type-name)))
       (if-let ((,field (find-intricate-field ,intricate ,slot-name)))
         (if-let ((,field-setter (setter-of ,field)))
           (progn ,@body)
           (error "Writer not found for slot ~A in type ~A" ,slot-name ,type-name))
         (error "Field ~A not found in type ~A" ,slot-name ,type-name))
       (error "Metadata for intricate record ~A not found" ,type-name))))


(defun set-intricate-slot-value (instance type-name slot-name value)
  (with-field-setter (field-setter type-name slot-name)
    (funcall field-setter instance value))
  value)


(define-compiler-macro set-intricate-slot-value (&whole whole instance type-name slot-name value)
  (let ((quoted-type-name (find-quoted type-name))
        (quoted-slot-name (find-quoted slot-name)))
    (if (and quoted-type-name quoted-slot-name)
        (with-field-setter (field-setter quoted-type-name quoted-slot-name)
          (once-only (value)
            `(prog1 ,value (,field-setter ,instance ,value))))
        whole)))


(define-setf-expander intricate-slot-value (instance type-name slot-name &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion instance env)
    (declare (ignore setter newval))
    (with-gensyms (store)
      (let ((quoted-type-name (find-quoted type-name))
            (quoted-slot-name (find-quoted slot-name)))
        (values dummies
                vals
                `(,store)
                (if (and quoted-type-name quoted-slot-name)
                    `(set-intricate-slot-value ,instance ',quoted-type-name ',quoted-slot-name ,store)
                    `(set-intricate-slot-value ,instance ,type-name ,slot-name ,store))
                `(intricate-slot-value ,getter ,type-name ,slot-name))))))


(defun intricate-slot-value (instance type-name slot-name)
  (if-let ((intricate (find-intricate-record type-name)))
    (if-let ((field (find-intricate-field intricate slot-name)))
      (if-let ((getter (getter-of field)))
        (funcall getter instance)
        (error "Reader not found for slot ~A in type ~A" slot-name type-name))
      (error "Field ~A not found in type ~A" slot-name type-name))
    (error "Metadata for intricate record ~A not found" type-name)))


(define-compiler-macro intricate-slot-value (&whole whole instance type-name slot-name)
  (let ((quoted-type-name (find-quoted type-name))
        (quoted-slot-name (find-quoted slot-name)))
    (if (and quoted-type-name quoted-slot-name)
        (if-let ((intricate (find-intricate-record quoted-type-name)))
          (if-let ((field (find-intricate-field intricate quoted-slot-name)))
            (if-let ((getter (getter-of field)))
              `(,getter ,instance)
              (error "Reader not found for slot ~A in type ~A" slot-name type-name))
            (error "Field ~A not found in type ~A" slot-name type-name))
          (error "Metadata for intricate record ~A not found" type-name))
        whole)))


(defmacro with-intricate-slots (type (&rest slots) instance &body body)
  (if slots
      (once-only (instance)
        `(symbol-macrolet (,@(loop for slot-def in slots
                                   collect (destructuring-bind (var-name &optional slot-name) slot-def
                                             (let ((slot-name (or slot-name var-name)))
                                               `(,var-name
                                                 (intricate-slot-value ,instance ',type ',slot-name))))))
           ,@body))
      `(progn ,@body)))
