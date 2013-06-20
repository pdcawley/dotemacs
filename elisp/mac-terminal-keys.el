;; Provides some key mappings so that the Mac terminal app navigation
;; keys etc will do the right thing when talking to terminal mode
;; emacs. This is mapped from the 'xterm defaults' preset in iTerm2.

(define-key key-translation-map [\e] [\M])
;; Arrows + Meta/Control/Shift
(define-key input-decode-map  "\e[1;9A"        [M-up])
(define-key input-decode-map "\e[1;10A"      [S-M-up])
(define-key input-decode-map "\e[1;14A"      [C-M-up])
(define-key input-decode-map "\e[1;15A"    [S-C-M-up])
(define-key input-decode-map  "\e[1;9B"      [M-down])
(define-key input-decode-map "\e[1;10B"    [S-M-down])
(define-key input-decode-map "\e[1;14B"    [C-M-down])
(define-key input-decode-map "\e[1;15B"  [S-C-M-down])
(define-key input-decode-map  "\e[1;9D"      [M-left])
(define-key input-decode-map "\e[1;10D"    [S-M-left])
(define-key input-decode-map "\e[1;14D"    [C-M-left])
(define-key input-decode-map "\e[1;15D"  [S-C-M-left])
(define-key input-decode-map  "\e[1;9C"     [M-right])
(define-key input-decode-map "\e[1;10C"   [S-M-right])
(define-key input-decode-map "\e[1;14C"   [C-M-right])
(define-key input-decode-map "\e[1;15C" [S-C-M-right])

(provide 'mac-terminal-keys)
