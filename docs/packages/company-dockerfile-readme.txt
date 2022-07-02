To use this package with company-mode run;
(add-hook 'dockerfile-mode-hook
          (lambda ()
            (add-to-list 'company-backends #'company-dockerfile))

To use this package, you must be in dockerfile major mode.
