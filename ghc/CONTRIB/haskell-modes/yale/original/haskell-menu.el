;;; haskell-menu.el -- support for Haskell menubar functions
;;;
;;; author :  Sandra Loosemore
;;; date   :  15 Jun 1994
;;;


;;; Add an entry to the main menu bar

(defvar menu-bar-haskell-menu (make-sparse-keymap "Haskell"))
(define-key haskell-mode-map [menu-bar haskell]
  (cons "Haskell" menu-bar-haskell-menu))
(define-key inferior-haskell-mode-map [menu-bar haskell]
  (cons "Haskell" menu-bar-haskell-menu))
(define-key ht-mode-map [menu-bar haskell]
  (cons "Haskell" menu-bar-haskell-menu))


;;; Define the functions.  They get listed on the menu in the reverse
;;; order that they're defined.

(define-key menu-bar-haskell-menu [haskell-tutorial]
  '("Tutorial" . haskell-tutorial))
(define-key menu-bar-haskell-menu [haskell-optimizers]
  '("Optimizers..." . haskell-optimizers))
(define-key menu-bar-haskell-menu [haskell-printers]
  '("Printers..." . haskell-printers))
(define-key menu-bar-haskell-menu [haskell-get-pad]
  '("Scratch Pad" . haskell-get-pad))
(define-key menu-bar-haskell-menu [haskell-compile]
  '("Compile File..." . haskell-compile))
(define-key menu-bar-haskell-menu [haskell-run-file]
  '("Run File..." . haskell-run-file))
(define-key menu-bar-haskell-menu [haskell-load]
  '("Load File..." . haskell-load))
(define-key menu-bar-haskell-menu [haskell-report-type]
  '("Type Check Expression..." . haskell-report-type))
(define-key menu-bar-haskell-menu [haskell-run]
  '("Run Dialogue..." . haskell-run))
(define-key menu-bar-haskell-menu [haskell-eval]
  '("Eval Expression..." . haskell-eval))

(provide 'haskell-menu)
