(cl:in-package :iffi)


(defvar *alias-table* (make-hash-table :test 'equal))


(defun find-alias-node (name)
  (gethash name *alias-table*))


(defun register-alias-node (name parent children)
  (setf
   (gethash name *alias-table*)
   (list* parent children)))


(defun alias-node-parent (node)
  (first node))


(defun alias-node-children (node)
  (rest node))


(defun add-alias-node-child (node child)
  (pushnew child (rest node) :test #'equal)
  node)


(defun update-alias-node-parent (node new-parent)
  (setf (first node) new-parent)
  node)


(defun ensure-canonical-pointer (type)
  (cond
    ((and (atom type)
          (or (eq type :const)
              (string= "&rest" (string-downcase type))))
     type)
    ((and (listp type) (eq :pointer (cffi::canonicalize-foreign-type type)))
     (list* :pointer (rest type)))
    ((eq :pointer (cffi::canonicalize-foreign-type type))
     :pointer)
    (t type)))


(defun find-base-type (type)
  (let ((type (ensure-canonical-pointer type)))
    (if (and (listp type)
             (eq :pointer (first type)))
        (list :pointer (find-base-type (second type)))
        (let* ((node (find-alias-node type))
               (parent (and node (alias-node-parent node))))
          (if parent
              (find-base-type parent)
              type)))))


(defun find-intricate-aliases (name)
  (labels ((%collect-aliases (id)
             (let ((node (find-alias-node id)))
               (list* id (loop for child in (alias-node-children node)
                               append (%collect-aliases child))))))
    (%collect-aliases (find-base-type name))))


(defun register-intricate-alias (base alias)
  (labels ((%register-alias (name base)
             (register-alias-node name base nil))
           (%register-base (name &rest children)
             (register-alias-node name nil children)))
    (let ((base-node (find-alias-node base))
          (alias-node (find-alias-node alias)))
      (cond
        ((not (or base-node alias-node))
         (%register-base base alias)
         (%register-alias alias base))
        ((and base-node alias-node)
         (unless (and (equal (alias-node-parent alias-node) base)
                      (member alias (alias-node-children base-node) :test #'equal))
           (error "Alias ~A exists: current base ~A" alias (find-base-type alias))))
        (base-node
         (%register-alias alias base)
         (add-alias-node-child base-node alias))
        (alias-node
         (if (alias-node-parent alias-node)
             (error "Alias ~A exists: current base ~A" alias (find-base-type alias))
             (progn
               (%register-base base alias)
               (update-alias-node-parent alias-node base))))))))


;;;
;;; ALIAS
;;;
(defmacro defitype (alias origin &optional documentation)
  `(progn
     (cffi:defctype ,alias ,origin ,@(when documentation `(,documentation)))))
