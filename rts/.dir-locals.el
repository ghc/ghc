;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

;; Default mode settings: no tabs, 80 column, UTF8
((nil
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (buffer-file-coding-system . utf-8-unix))

 ;; c-mode settings: 'Allman' BSD style, 4 space indents
 (c-mode
  (c-file-style . "BSD")
  (c-basic-offset . 4)))
