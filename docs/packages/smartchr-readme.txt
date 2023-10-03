(global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))

substitute `!!' with cursor
(global-set-key (kbd "{")
             (smartchr '("{ `!!' }" "{ \"`!!'\" }" "{")))
