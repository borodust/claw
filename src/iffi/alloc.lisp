(cl:in-package :iffi)


(defun intricate-alloc (name &optional (count 1))
  (if-let ((intricate (find-intricate-record name)))
    (aligned-alloc (intricate-alignment name) (* (intricate-size name) count))
    (aligned-alloc (cffi:foreign-type-size name) (* (cffi:foreign-type-alignment name) count))))


(define-compiler-macro intricate-alloc (&whole whole name &optional (count 1))
  (if-let ((quoted (find-quoted name)))
    (if-let ((intricate (find-intricate-record quoted)))
      `(aligned-alloc (intricate-alignment ,name) (* (intricate-size ,name) ,count))
      (if-let ((type-alignment (ignore-errors (cffi:foreign-type-alignment quoted)))
               (type-size (ignore-errors (cffi:foreign-type-size quoted))))
        `(aligned-alloc ,type-alignment (* ,type-size ,count))
        whole))
    whole))


(defmacro with-intricate-alloc (bindings &body body)
  (let* ((bindings (if (listp (first bindings))
                       bindings
                       (list bindings))))
    `(let (,@(loop for binding in bindings
                   collect (destructuring-bind (name type &optional count) binding
                             `(,name (intricate-alloc ',type ,@(when count `(,count)))))))
       (unwind-protect
            (progn ,@body)
         ,@(loop for name in (mapcar #'first bindings)
                 collect `(intricate-free ,name))))))


(defmacro with-intricate-allocs (bindings &body body)
  `(with-intricate-alloc ,bindings ,@body))


(declaim (inline intricate-free))
(defun intricate-free (ptr)
  (aligned-free ptr))


(define-compiler-macro intricate-free (ptr)
  `(aligned-free ,ptr))






;;;
;;; INSTANCE
;;;
(defun make-intricate-instance (name &rest args)
  (let* ((record (find-intricate-record name))
         (ptr (intricate-alloc name)))
    (unless record
      (error "Record with name ~A not found" name))
    (if-let ((ctor (constructor-of record)))
      (handler-case
          (apply (constructor-of record) `(:pointer ,name) ptr args)
        (serious-condition (condi) (intricate-free ptr) (error condi)))
      (error "Constructor not found for record ~A" name))
    ptr))


(define-compiler-macro make-intricate-instance (&whole whole name &rest args)
  (let* ((quoted-name (find-quoted name))
         (record (find-intricate-record quoted-name))
         (ctor (and record (constructor-of record))))
    (when quoted-name
      (cond
        ((not record) (warn "Record with name ~A not found" quoted-name))
        ((not ctor) (warn "Constructor not found for record ~A" quoted-name))))
    (if ctor
        (with-gensyms (ptr condi)
          `(let ((,ptr (intricate-alloc ',quoted-name)))
             (handler-case
                 ;; FIXME: here we actually a break funcall protocol a bit
                 ;; because if during args evaluation condition is raised it is
                 ;; going to be consumed here with stack being unwound
                 (,ctor '(:pointer ,quoted-name) ,ptr ,@args)
               (serious-condition (,condi) (intricate-free ,ptr) (error ,condi)))
             ,ptr))
        whole)))


(defun destroy-intricate-instance (name instance)
  (let ((record (find-intricate-record name)))
    (unless record
      (error "Record with name ~A not found" name))
    (if-let ((dtor (destructor-of record)))
      (funcall dtor `(:pointer ,name) instance)
      (error "Destructor for record ~A not found" name)))
  (intricate-free instance))


(define-compiler-macro destroy-intricate-instance (&whole whole name instance)
  (let* ((quoted-name (find-quoted name))
         (record (find-intricate-record quoted-name))
         (dtor (and record (destructor-of record))))
    (if dtor
        (once-only (instance)
          `(progn
             (,dtor '(:pointer ,quoted-name) ,instance)
             (intricate-free ,instance)))
        whole)))


(defmacro with-intricate-instance ((var name &rest constructor-args) &body body)
  `(let ((,var (make-intricate-instance ',name ,@constructor-args)))
     (unwind-protect
          (progn ,@body)
       (destroy-intricate-instance ',name ,var))))


(defmacro with-intricate-instances ((&rest declarations) &body body)
  (labels ((expand-with-intricate-instances (declarations body)
             (if declarations
                 `((with-intricate-instance ,(first declarations)
                     ,@(expand-with-intricate-instances (rest declarations) body)))
                 `(,@body))))
    (first (expand-with-intricate-instances declarations body))))
