(in-package :claw.util)


(defun test-template-handling ()
  (macrolet ((test (form)
               `(unless ,form
                  (cerror "proceed" "Test failed: ~S" ',form))))
    (test (equal (split-template-name-into-groups "A")
                 '("A")))
    (test (equal (split-template-name-into-groups "A<int>")
                 '("A" ("int"))))
    (test (equal (split-template-name-into-groups "A<B<int>, C>")
                 '("A" ("B" ("int") "C"))))
    (test (equal (split-template-name-into-groups "A<B, C<int>>")
                 '("A" ("B" "C" ("int")))))
    (test (equal (split-template-name-into-groups "A<B, C<int>, D>")
                 '("A" ("B" "C" ("int") "D"))))

    ;; Previous result: ""
    (test (equal (remove-template-argument-string "A<int>")
                 "A"))
    ;; Previous result: "<1,1, B<int>, C>"
    (test (equal (extract-template-argument-string "A<1, B<int>, C>")
                 "<1, B<int>, C>"))

    (test (equal (extract-template-argument-string "A")
                 nil))
    (test (equal (extract-template-argument-string "A<int>")
                 "<int>"))
    (test (equal (extract-template-argument-string "A<B<int>, C>")
                 "<B<int>, C>"))
    (test (equal (extract-template-argument-string "A<B, C<int>>")
                 "<B, C<int>>"))
    (test (equal (extract-template-argument-string "A<B, C<int>, D>")
                 "<B, C<int>, D>"))
    "Passed"))
