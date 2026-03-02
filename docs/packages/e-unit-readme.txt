E-Unit is a minitest/JUnit-style testing framework for Emacs Lisp.
It provides test definition, assertions, extensibility, and output.

Basic usage:

  (deftest test-basic-math ()
    "Test basic arithmetic"
    (assert-equal 4 (+ 2 2)))

  (e-unit-run-buffer)

Suggested keybindings (add to your init.el):

  (global-set-key (kbd "C-c C-t b") #'e-unit-run-buffer-compilation)
  (global-set-key (kbd "C-c C-t d") #'e-unit-run-directory-compilation)
