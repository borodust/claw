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


(defun parse-instantiated-record-type (record-type)
  (let ((record-decl (%resect:type-declaration record-type)))
    (parse-new-record-declaration (%resect:declaration-kind record-decl) record-decl :type record-type)))


(defmethod parse-instantiation ((kind (eql :record)) type)
  (parse-instantiated-record-type type))
