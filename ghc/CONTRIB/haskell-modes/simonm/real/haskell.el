;;; Haskell mode for emacs (c) Simon Marlow 11/1/92

(defvar haskell-mode-map ()
  "Keymap used in Haskell mode.")

(defvar haskell-literate-mode-map ()
  "Keymap used in Haskell literate script mode.")

(defvar haskell-mode-syntax-table ()
  "Syntax table for haskell mode.")

(if haskell-mode-map
    ()
  (setq haskell-mode-map (make-sparse-keymap))
  (define-key haskell-mode-map "\C-j" 'haskell-newline-and-indent))

(if haskell-literate-mode-map
    ()
  (setq haskell-literate-mode-map (make-sparse-keymap))
  (define-key haskell-literate-mode-map "\C-j" 
    'haskell-literate-newline-and-indent)
  (define-key haskell-literate-mode-map "\M-\C-i" 
    'haskell-literate-toggle-bird-track-line)
  (define-key haskell-literate-mode-map "\M-m" 
    'haskell-literate-back-to-indentation))


(if haskell-mode-syntax-table
    ()
  (let ((i 0))
    (setq haskell-mode-syntax-table (make-syntax-table))
;    (while (< i ?0)
;      (modify-syntax-entry i "." haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i (1+ ?9))
;      (modify-syntax-entry i "_" haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i ?A)
;      (modify-syntax-entry i "." haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i (1+ ?Z))
;      (modify-syntax-entry i "w" haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i ?a)
;      (modify-syntax-entry i "." haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i (1+ ?z))
;      (modify-syntax-entry i "w" haskell-mode-syntax-table)
;      (setq i (1+ i)))
;    (while (< i 128)
;      (modify-syntax-entry i "." haskell-mode-syntax-table)
;      (setq i (1+ i)))
    (modify-syntax-entry ?   " " haskell-mode-syntax-table)
    (modify-syntax-entry ?\t " " haskell-mode-syntax-table)
    (modify-syntax-entry ?\f "> b"    haskell-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    haskell-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" haskell-mode-syntax-table)
    (modify-syntax-entry ?\' "w" haskell-mode-syntax-table)
    (modify-syntax-entry ?_  "w" haskell-mode-syntax-table)
    (modify-syntax-entry ?\\ "." haskell-mode-syntax-table)
    (modify-syntax-entry ?\( "()" haskell-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" haskell-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" haskell-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" haskell-mode-syntax-table)
    (modify-syntax-entry ?{  "(}1" haskell-mode-syntax-table)
    (modify-syntax-entry ?}  "){4" haskell-mode-syntax-table)
    (modify-syntax-entry ?-  ". 12b" haskell-mode-syntax-table)
    ))

(defun haskell-vars ()
  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--[^a-zA-Z0-9]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'haskell-comment-indent)
  )

(defun haskell-mode ()
  "Major mode for editing Haskell programs.
Blank lines separate paragraphs, Comments start with '--'. 
Use Linefeed to do a newline and indent to the level of the previous line.
Tab simply inserts a TAB character.
Entry to this mode calls the value of haskell-mode-hook if non-nil."
  (interactive)
  (haskell-vars)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell")
  (use-local-map haskell-mode-map)
  (set-syntax-table haskell-mode-syntax-table)
  (run-hooks 'haskell-mode-hook))

(defun haskell-literate-mode ()
  "Major mode for editing haskell programs in literate script form.
Linefeed produces a newline, indented maybe with a bird track on it.
M-TAB toggles the state of the bird track on the current-line.
Entry to this mode calls haskell-mode-hook and haskell-literate-mode-hook."
  (interactive)
  (haskell-vars)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords haskell-literate-font-lock-keywords)
  (setq major-mode 'haskell-literate-mode)
  (setq mode-name "Literate Haskell")
  (use-local-map haskell-literate-mode-map)
  (set-syntax-table haskell-mode-syntax-table)
  (run-hooks 'haskell-mode-hook)
  (run-hooks 'haskell-literate-mode-hook))

;; Find the indentation level for a comment..
(defun haskell-comment-indent ()
  (skip-chars-backward " \t")
  ;; if the line is blank, put the comment at the beginning,
  ;; else at comment-column
  (if (bolp) 0 (max (1+ (current-column)) comment-column)))

;; Newline, and indent according to the previous line's indentation.
;; Don't forget to use 'indent-tabs-mode' if you require tabs to be used
;; for indentation.
(defun haskell-newline-and-indent ()
  (interactive)
  (newline)
  (let ((c 0))
    (save-excursion
      (forward-line -1)
      (back-to-indentation)
      (setq c (if (eolp) 0 (current-column))))
    (indent-to c)))			;ident new line to this level

;;; Functions for literate scripts

;; Newline and maybe add a bird track, indent
(defun haskell-literate-newline-and-indent ()
  (interactive)
  (newline)
  (let ((bird-track nil) (indent-column 0))
    (save-excursion
      (forward-line -1)
      (if (= (following-char) ?>) (setq bird-track t))
      (skip-chars-forward "^ \t")
      (skip-chars-forward " \t")
      (setq indent-column (if (eolp) 0 (current-column))))
    (if bird-track (insert-char ?> 1))
    (indent-to indent-column)))

;; Toggle bird-track ][ 
(defun haskell-literate-toggle-bird-track-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (= (following-char) ? )
 	(progn (delete-char 1) (insert-char ?> 1))
      (if (= (following-char) ?>)
 	  (progn (delete-char 1) (insert-char ?  1))
  	(progn (insert-char ?> 1) (insert-char ?  1))))))

(defun haskell-literate-toggle-bird-track-region (start end)
  (interactive "r") 
  (save-excursion 
    (goto-char start) 
    (while (<= (point) end) 
      (beginning-of-line)
      (haskell-literate-toggle-bird-track-line)
      (forward-line 1))))

(defun haskell-literate-back-to-indentation ()
  (interactive)
  (beginning-of-line)
  (if (= (following-char) ?>) 
      (forward-char 1))
  (skip-chars-forward " \t"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; keywords for jwz's font-look-mode (lemacs 19)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar haskell-literate-font-lock-keywords ()
  "Font definitions for Literate Haskell files.")

(setq haskell-literate-font-lock-keywords
      (list
       '("^[^>\n].*$" . font-lock-comment-face)
       (concat "\\b\\("
	       (mapconcat 'identity 
			  '("case" "class" "data" "default" "deriving" "else" 
			    "hiding" "if" "import" "in" "infix" "infixl" 
			    "infixr" "instance" "interface" "let" "module" 
			    "of" "renaming" "then" "to" "type" "where")
			  "\\|")
	       "\\)\\b")
;       '("(\\|)\\|\\[\\|\\]\\|,\\|[\\\\!$#^%&*@~?=-+<>.:]+" . font-lock-function-name-face)
       ))

