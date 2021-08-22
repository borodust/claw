(cl:in-package :iffi)


(defun find-quoted (value)
  (cond
    ((keywordp value) value)
    ((and (listp value) (eq 'quote (first value))) (second value))))


(defmacro initialize-iffi ()
  `(progn
     (declaim (inline iffi::aligned-alloc iffi::aligned-free))
     ,@(cond
         ((cffi:foreign-symbol-pointer "aligned_alloc")
          `((cffi:defcfun ("aligned_alloc" iffi::aligned-alloc) :pointer
              (byte-alignment :size)
              (byte-size :size))

            (defun iffi::aligned-free (ptr)
              (cffi:foreign-free ptr))))

         ((cffi:foreign-symbol-pointer "_aligned_malloc")
          `((declaim (inline iffi::%aligned-malloc))
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
