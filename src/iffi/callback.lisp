(cl:in-package :iffi)

(defvar *callback-table* (make-hash-table))
(defvar *function-class-table* (make-hash-table))
;;;
;;; CALLBACK
;;;
(defun register-intricate-function-class (name data)
  (setf (gethash name *function-class-table*) data))


(defun find-intricate-function-class (name)
  (gethash name *function-class-table*))


(defun function-class-constructor (data)
  (getf data :constructor))

(defun function-class-destructor (data)
  (getf data :destructor))

(defun function-class-return-type (data)
  (getf data :return-type))

(defun function-class-parameters (data)
  (getf data :parameters))


(defmacro define-intricate-function-class (name-and-opts return-type &body config)
  (destructuring-bind (name &key (constructor (error ":constructor missing"))
                              (destructor (error ":destructor missing"))
                              (size-reporter (error ":size-reporter missing"))
                              (alignment-reporter (error ":alignment-reporter missing"))
                              (inline t))
      (ensure-list name-and-opts)
    (let (doc
          params
          (cffi-fctor-name (format-symbol (symbol-package name) "~A~A" 'iffi-fctor$ constructor))
          (cffi-fdtor-name (format-symbol (symbol-package name) "~A~A" 'iffi-fdtor$ destructor)))
      (if (stringp (first config))
          (setf doc (first config)
                params (rest config))
          (setf params config))
      `(progn
         (iffi:deficlass (,name
                          :size-reporter ,size-reporter
                          :alignment-reporter ,alignment-reporter
                          :inline ,inline)
             nil
           ,@(when doc
               (list doc)))
         ,@(when inline
             `((declaim (inline ,cffi-fctor-name))))
         (cffi:defcfun (,constructor ,cffi-fctor-name) (:pointer ,name)
           (mem (:pointer ,name))
           (cffi-callback :pointer))
         ,@(when inline
             `((declaim (inline ,cffi-fdtor-name))))
         (cffi:defcfun (,destructor ,cffi-fdtor-name) :void
           (callback (:pointer ,name)))

         (meta-eval
           (register-intricate-function-class
            ',name
            ',(list :constructor cffi-fctor-name
                    :destructor cffi-fdtor-name
                    :return-type return-type
                    :parameters params)))))))


(defun register-intricate-callback-function-class (name fun-class-name)
  (setf (gethash name *callback-table*) fun-class-name))


(defun find-intricate-callback-function-class (name)
  (gethash name *callback-table*))


(defmacro deficallback (name (&rest parameter-names) function-class-name &body body)
  (let* ((base-function-class-name (find-base-type function-class-name))
         (fun-class (find-intricate-function-class base-function-class-name)))
    (unless fun-class
      (error "Cannot define ~A callback: function class ~A not found" name function-class-name))
    (let ((parameters (function-class-parameters fun-class))
          (return-type (function-class-return-type fun-class)))
      (unless (= (length parameters) (length parameter-names))
        (error "Cannot define ~A callback: ~A parameters required, but got ~A"
               name (length parameters) (length parameter-names)))
      `(progn
         (cffi:defcallback ,name ,return-type (,@(loop for (nil type) in parameters
                                                       for param-name in parameter-names
                                                       collect `(,param-name ,type)))
           ,@body)
         (meta-eval
           (register-intricate-callback-function-class ',name ',base-function-class-name))))))


(defun make-intricate-callback (name)
  (if-let (fun-class-name (find-intricate-callback-function-class name))
    (let ((fun-class (find-intricate-function-class fun-class-name)))
      (funcall (function-class-constructor fun-class)
               (intricate-alloc fun-class-name) (cffi:get-callback name)))
    (error "Function class not found for callback ~A" name)))


(define-compiler-macro make-intricate-callback (&whole whole name)
  (let ((cb-name (find-quoted name)))
    (if-let (fun-class-name (find-intricate-callback-function-class cb-name))
      (let ((fun-class (find-intricate-function-class fun-class-name)))
        `(,(function-class-constructor fun-class)
          (intricate-alloc ',fun-class-name)
          (cffi:callback ,cb-name)))
      whole)))


(defun destroy-intricate-callback (name callback)
  (if-let (fun-class-name (find-intricate-callback-function-class name))
    (let ((fun-class (find-intricate-function-class fun-class-name)))
      (funcall (function-class-destructor fun-class) callback)
      (intricate-free callback))
    (error "Function class not found for callback ~A" name)))


(define-compiler-macro destroy-intricate-callback (&whole whole name callback)
  (let ((cb-name (find-quoted name)))
    (if-let (fun-class-name (find-intricate-callback-function-class cb-name))
      (let ((fun-class (find-intricate-function-class fun-class-name)))
        `(progn
           (,(function-class-destructor fun-class) ,callback)
           (intricate-free ,callback)))
      whole)))
