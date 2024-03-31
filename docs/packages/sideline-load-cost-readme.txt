
Display load/require module size with sideline.

1) Add sideline-load-cost to sideline backends list,

  (setq sideline-backends-right '(sideline-load-cost))

2) Then enable sideline-mode in the target buffer,

  M-x sideline-mode
