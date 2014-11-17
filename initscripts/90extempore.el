(setq user-extempore-directory "/usr/local/opt/extempore/")

(autoload 'extempore-mode (concat user-extempore-directory "extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(setq extempore-tab-completion nil)

(setq extempore-program-args "--indevice 0 --device 1 --frames 1024")

(defun pdc/extempore-mode-hook ()
  (turn-on-eldoc-mode)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  (yas-minor-mode-on))

(add-hook 'extempore-mode-hook 'pdc/extempore-mode-hook)

(autoload #'llvm-mode (concat user-extempore-directory "extras/llvm-mode.el")
  "Major mode for editing LLVM IR files" t)
(add-to-list 'auto-mode-alist '("\\.ir$" . llvm-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode))


(defun extempore-create-template-file (base-path filename &optional header)
  (let ((full-path (format "%s/%s" base-path filename)))
    (unless (file-exists-p full-path)
      (progn
        (find-file full-path)
        (if header (insert header))
        (save-buffer)
        (kill-buffer)))))

(defun extempore-create-template (name)
  "Set up the directory structure and files for a new extempore session/gig."
  (interactive "sSession name: ")
  (let* ((xtm-dir (expand-file-name "~/Projects/xtm/"))
         (base-path (concat xtm-dir "sessions/" name))
         (setup-header
          (concat ";;; setup.xtm --- setup file for " name "\n"
                  "(sys:load \"libs/external/instruments_ext.xtm\")\n"
                  ";; (sys:load \"" xtm-dir "lib/ben-lib-scheme.xtm\")\n"
                  ";; (ipc:load \"utility\" \"" xtm-dir "\"lib/ben-lib-scheme.xtm\")\n"
                  ";; (sys:load \"" xtm-dir "lib/sampler-maps.xtm\")\n"
                  ";; (ipc:load \"utility\" \"" xtm-dir "\"lib/sampler-maps.xtm\")\n"
                  "(bind-func dsp:DSP\n"
                  "  (lambda (in time chan dat)\n"
                  "    0.0))\n\n"
                  ";; (ipc:bind-func \"utility\" 'instname)\n"
                  "(ipc:audio-setup \"utility\")\n"
                  "(dsp:set! dsp)")))
    (if (file-exists-p base-path)
        (error "Cannot create xtm session: Directory already exists"))
    (make-directory base-path t)
    ;; Practice files
    (extempore-create-template-file
     base-path "prac-utility.xtm" "headeru")
    (extempore-create-template-file
     base-path "prac-primary.xtm" "headerp")
    ;; Gig files
    (extempore-create-template-file
     base-path "gig-utility.xtm" "headeru")

    (extempore-create-template-file
     base-path "gig-primary.xtm" "headerp")

    ;; Setup file
    (extempore-create-template-file
     base-path "setup.xtm" setup-header)
    (dired base-path)))

;; yasnippet helpers


;; used in extempore-mode's print-line-debug snippet
(defun extempore-yas-println-debug-expander (pl-str format-str)
  (if (not (string= pl-str ""))
      (mapconcat (lambda (name) (format format-str name name))
                 (cl-remove-if (lambda (x) (or (string-match "^'.*:$" x)
                                               (string-match "^\".*:\"$" x)))
                               (split-string pl-str " "))
                 " ")
    pl-str))

(defvar extempore-yas-expansion-list-oscmc_c
  '(("osc" "0." "chan amp freq")
    ("square" "0." "chan amp freq n")
    ("triangle" "0." "chan amp freq n")
    ("rect" "0." "chan amp freq duty")
    ("saw" "0." "chan amp freq")
    ("pulse" "" "chan amp freq width")
    ("fade" "" "chan initial final dur")
    ("delay" "2 max_delay_samps" "chan in wet fb")
    ("comb" "2 max_delay_samps" "chan in delay wet fb")
    ("flanger" "2 delay mod_phase mod_range mod_rate" "chan in wet fb")
    ("chorus" "2 phase" "chan in wet fb")
    ("tap_delay" "2 max_delay_samps ntaps" "chan in")
    ("allpass" "2 delay_sec" "chan in wet")
    ("reverb" "2 size_ms" "chan in wet fb")
    ("hold" "2" "chan in h")
    ("lpf" "2" "chan in freq res")
    ("hpf" "2" "chan in freq res")
    ("bpf" "2" "chan in freq bw")
    ("notch" "2" "chan in freq bw")
    ("vcf" "type chan" "chan in freq res")
    ("hann" "" "chan width")
    ("linear" "start end dur" "chan inc")))

(defun extempore-yas-oscmc_c-expander (type construct-p)
  (let ((res (cl-find-if (lambda (x) (string= (car x) type))
                         extempore-yas-expansion-list-oscmc_c)))
    (if res
        (if construct-p
            (cadr res)
          (caddr res))
      "")))



