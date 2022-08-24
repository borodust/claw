(cl:in-package :iffi)


(declaim (special *allocator* *extricator*))


(defvar *allocator-expander* nil
  "Can be bound via CLtL2 compiler-let to allow optimizing codegen in aligned-alloc")

(defvar *extricator-expander* nil
  "Can be bound via CLtL2 compiler-let to allow optimizing codegen in aligned-free")

(defun find-quoted (value)
  (cond
    ((keywordp value) value)
    ((and (listp value) (eq 'quote (first value))) (second value))))


(defmacro initialize-iffi ()
  `(progn
     ,@(cond
         ((alexandria:featurep :iffi-custom-allocation)
          `((defun iffi::aligned-alloc (byte-alignment byte-size)
              (funcall *allocator* byte-alignment byte-size))

            (define-compiler-macro iffi::aligned-alloc (byte-alignment byte-size)
              (if *allocator-expander*
                  (funcall *allocator-expander* byte-alignment byte-size)
                  `(funcall *allocator* ,byte-alignment ,byte-size)))

            (defun iffi::aligned-free (ptr)
              (funcall *extricator* ptr))

            (define-compiler-macro iffi::aligned-free (ptr)
              (if *extricator-expander*
                  (funcall *extricator-expander* ptr)
                  `(funcall *extricator* ,ptr)))))
         ((cffi:foreign-symbol-pointer "aligned_alloc")
          `((declaim (inline iffi::aligned-alloc iffi::aligned-free))
            (cffi:defcfun ("aligned_alloc" iffi::aligned-alloc) :pointer
              (byte-alignment :size)
              (byte-size :size))

            (defun iffi::aligned-free (ptr)
              (cffi:foreign-free ptr))))

         ((cffi:foreign-symbol-pointer "_aligned_malloc")
          `((declaim (inline iffi::%aligned-malloc
                             iffi::aligned-alloc
                             iffi::aligned-free))
            (cffi:defcfun ("_aligned_malloc" iffi::%aligned-malloc) :pointer
              (byte-size :size)
              (byte-alignment :size))

            (defun iffi::aligned-alloc (alignment size)
              (iffi::%aligned-malloc size alignment))

            (cffi:defcfun ("_aligned_free" iffi::aligned-free) :pointer
              (memory :pointer))))

         (t (error "Aligned memory allocation function not found. No C std library linked?")))))


(defmacro meta-eval (&body body)
  `(eval-when (:compile-toplevel
               :execute
               ,@(unless (member :iffi-ephemeral-metadata *features*) '(:load-toplevel)))
     ,@body))
