;; Lua
(add-to-list 'load-path "/usr/share/emacs/site-lisp/lua-mode")
(autoload 'lua-mode "lua-mode" "Mode for editing Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(setq lua-default-application "/usr/bin/lua")

(provide 'dss-lua)
