;; haskell-mode.el. Major mode for editing Haskell.
;; Copyright (C) 1989, Free Software Foundation, Inc., Lars Bo Nielsen
;; and Lennart Augustsson
;; modified by Peter Thiemann, March 1994

;; This file is not officially part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell Mode. A major mode for editing and running Haskell. (Version 0.0)
;; =================================================================
;;
;; This is a mode for editing and running Haskell.
;; It is very much based on the sml mode for GNU Emacs. It
;; features:
;;
;;      - Inferior shell running Haskell. No need to leave emacs, just
;;        keep right on editing while Haskell runs in another window.
;;
;;      - Automatic "load file" in inferior shell. Send regions of code
;;        to the Haskell program.
;;
;;
;; 1. HOW TO USE THE Haskell-MODE
;; ==========================
;;
;; Here is a short introduction to the mode.
;;
;; 1.1 GETTING STARTED
;; -------------------
;;
;; If you are an experienced user of Emacs, just skip this section.
;;
;; To use the haskell-mode, insert this in your "~/.emacs" file (Or ask your
;; emacs-administrator to help you.):
;;
;;    (setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) (cons '("\\.lhs$" . haskell-mode)
;;                           auto-mode-alist)))
;;    (autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell." t)
;;
;; Now every time a file with the extension `.hs' or `.lhs' is found, it is
;; automatically started up in haskell-mode.
;;
;; You will also have to specify the path to this file, so you will have
;; to add this as well:
;;
;;    (setq load-path (cons "/usr/me/emacs" load-path))
;;
;; where "/usr/me/emacs" is the directory where this file is.
;;
;; You may also want to compile the this file (M-x byte-compile-file)
;; for speed.
;;
;; You are now ready to start using haskell-mode. If you have tried other
;; language modes (like lisp-mode or C-mode), you should have no
;; problems. There are only a few extra functions in this mode.
;;
;; 1.2. EDITING COMMANDS.
;; ----------------------
;;
;; The following editing and inferior-shell commands can ONLY be issued
;; from within a buffer in haskell-mode.
;;
;; LFD (haskell-newline-and-indent).  
;;     This is probably the function you will be using the most (press
;;     CTRL while you press Return, press C-j or press Newline). It
;;     makes a new line and performs indentation based on the last 
;;     preceding non-comment line.
;;
;; M-; (indent-for-comment).
;;     Like in other language modes, this command will give you a comment
;;     at the of the current line. The column where the comment starts is
;;     determined by the variable comment-column (default: 40).
;;    
;; C-c C-v (haskell-mode-version). 
;;     Get the version of the haskell-mode.
;;
;;
;; 1.3. COMMANDS RELATED TO THE INFERIOR SHELL
;; -------------------------------------------
;;
;; C-c C-s (haskell-pop-to-shell).
;;     This command starts up an inferior shell running haskell. If the shell
;;     is running, it will just pop up the shell window.
;;
;; C-c C-u (haskell-save-buffer-use-file).
;;     This command will save the current buffer and send a "load file",
;;     where file is the file visited by the current buffer, to the
;;     inferior shell running haskell.
;;
;; C-c C-f (haskell-run-on-file).
;;     Will send a "load file" to the inferior shell running haskell,
;;     prompting you for the file name.
;;    
;; C-c C-r (haskell-send-region). 
;;     Will send region, from point to mark, to the inferior shell
;;     running haskell.
;;
;; C-c C-b (haskell-send-buffer). 
;;     Will send whole buffer to inferior shell running haskell.
;;
;; 2. INDENTATION
;; ================
;; 
;; The first indentation command (using C-j or TAB) on a given line
;; indents like the last preceding non-comment line. The next TAB
;; indents to the indentation of the innermost enclosing scope. Further
;; TABs get you to further enclosing scopes. After indentation has
;; reached the first column, the process restarts using the indentation
;; of the preceding non-comment line, again.
;;
;; 3. INFERIOR SHELL.
;; ==================
;;
;; The mode for Standard ML also contains a mode for an inferior shell
;; running haskell. The mode is the same as the shell-mode, with just one
;; extra command.
;;
;; 3.1. INFERIOR SHELL COMMANDS
;; ----------------------------
;;
;; C-c C-f (haskell-run-on-file).  Send a `load file' to the process running
;; haskell.
;;
;; 3.2. CONSTANTS CONTROLLING THE INFERIOR SHELL MODE
;; --------------------------------------------------
;;
;; Because haskell is called differently on various machines, and the
;; haskell-systems have their own command for reading in a file, a set of
;; constants controls the behavior of the inferior shell running haskell (to
;; change these constants: See CUSTOMIZING YOUR Haskell-MODE below).
;;
;; haskell-prog-name (default "hbi").
;;     This constant is a string, containing the command to invoke
;;     Standard ML on your system. 
;;
;; haskell-use-right-delim (default "\"")
;; haskell-use-left-delim  (default "\"")
;;     The left and right delimiter used by your version of haskell, for
;;     `use file-name'.
;;
;; haskell-process-name (default "Haskell"). 
;;     The name of the process running haskell. (This will be the name
;;     appearing on the mode line of the buffer)
;;
;; NOTE: The haskell-mode functions: haskell-send-buffer, haskell-send-function and
;; haskell-send-region, creates temporary files (I could not figure out how
;; to send large amounts of data to a process). These files will be
;; removed when you leave emacs.
;;
;; 4. FONTIFICATION
;;
;; There is support for Jamie Zawinski's font-lock-mode through the
;; variable "haskell-font-lock-keywords".
;;
;; 5. CUSTOMIZING YOUR Haskell-MODE
;; ============================
;;
;; If you have to change some of the constants, you will have to add a
;; `hook' to the haskell-mode. Insert this in your "~/.emacs" file.
;;
;;    (setq haskell-mode-hook 'my-haskell-constants)
;;
;; Your function "my-haskell-constants" will then be executed every time
;; "haskell-mode" is invoked.  Now you only have to write the emacs-lisp
;; function "my-haskell-constants", and put it in your "~/.emacs" file.
;;
;; Say you are running a version of haskell that uses the syntax `load
;; ["file"]', is invoked by the command "OurHaskell" and you don't want the
;; indentation algorithm to indent according to open parenthesis, your
;; function should look like this:
;;
;;    (defun my-haskell-constants ()
;;       (setq haskell-prog-name "OurHaskell")
;;       (setq haskell-use-left-delim "[\"")
;;       (setq haskell-use-right-delim "\"]")
;;       (setq haskell-paren-lookback nil))
;;
;; The haskell-shell also runs a `hook' (haskell-shell-hook) when it is invoked.
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; ORIGINAL AUTHOR
;;         Lars Bo Nielsen
;;         Aalborg University
;;         Computer Science Dept.
;;         9000 Aalborg
;;         Denmark
;;
;;         lbn@iesd.dk
;;         or: ...!mcvax!diku!iesd!lbn
;;         or: mcvax!diku!iesd!lbn@uunet.uu.net
;;
;; MODIFIED FOR Haskell BY
;;	   Lennart Augustsson
;;	   indentation stuff by Peter Thiemann
;;
;;
;; Please let me know if you come up with any ideas, bugs, or fixes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst haskell-mode-version-string
  "HASKELL-MODE, Version 0.2, PJT indentation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change
;;; 

;; The command used to start up the haskell-program.
(defconst haskell-prog-name "hbi" "*Name of program to run as haskell.")

;; The left delimmitter for `load file'
(defconst haskell-use-left-delim "\""
  "*The left delimiter for the filename when using \"load\".")

;; The right delimmitter for `load file'
(defconst haskell-use-right-delim "\""
  "*The right delimiter for the filename when using \"load\".")

;; A regular expression matching the prompt pattern in the inferior
;; shell
(defconst haskell-shell-prompt-pattern "^> *"
  "*The prompt pattern for the inferion shell running haskell.")

;; The template used for temporary files, created when a region is
;; send to the inferior process running haskell.
(defconst haskell-tmp-template "/tmp/haskell.tmp."
  "*Template for the temporary file, created by haskell-simulate-send-region.")

;; The name of the process running haskell (This will also be the name of
;; the buffer).
(defconst haskell-process-name "Haskell" "*The name of the Haskell-process")

;;;
;;; END OF CONSTANTS CONTROLLING THE MODE.
;;;
;;; If you change anything below, you are on your own.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar haskell-mode-syntax-table nil "The syntax table used in haskell-mode.")

(defvar haskell-mode-map nil "The mode map used in haskell-mode.")

(defvar haskell-mode-abbrev-table nil "The abbrev-table used in haskell-mode.")

(defvar haskell-old-kill-emacs-hook nil "Old value of kill-emacs-hook")

(defun haskell-mode ()
  "Major mode for editing Haskell code.
Tab indents for Haskell code.
Comments are delimited with --
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Key bindings:
=============

\\[haskell-pop-to-shell]\t  Pop to the haskell window.
\\[haskell-save-buffer-use-file]\t  Save the buffer, and send a \"load file\".
\\[haskell-send-region]\t  Send region (point and mark) to haskell.
\\[haskell-run-on-file]\t  Send a \"load file\" to haskell.
\\[haskell-send-buffer]\t  Send whole buffer to haskell.
\\[haskell-mode-version]\t  Get the version of haskell-mode.
\\[haskell-evaluate-expression]\t  Prompt for an expression and evalute it.


Mode map
========
\\{haskell-mode-map}
Runs haskell-mode-hook if non nil."
  (interactive)
  (kill-all-local-variables)
  (if haskell-mode-map
      ()
    (setq haskell-mode-map (make-sparse-keymap))
    (define-key haskell-mode-map "\C-c\C-v" 'haskell-mode-version)
    (define-key haskell-mode-map "\C-c\C-u" 'haskell-save-buffer-use-file)
    (define-key haskell-mode-map "\C-c\C-s" 'haskell-pop-to-shell)
    (define-key haskell-mode-map "\C-c\C-r" 'haskell-send-region)
    (define-key haskell-mode-map "\C-c\C-m" 'haskell-region)
    (define-key haskell-mode-map "\C-c\C-f" 'haskell-run-on-file)
    (define-key haskell-mode-map "\C-c\C-b" 'haskell-send-buffer)
    (define-key haskell-mode-map "\C-ce"    'haskell-evaluate-expression)
    (define-key haskell-mode-map "\C-j"     'haskell-newline-and-indent)
    (define-key haskell-mode-map "\177"     'backward-delete-char-untabify))
  (use-local-map haskell-mode-map)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell")
  (define-abbrev-table 'haskell-mode-abbrev-table ())
  (setq local-abbrev-table haskell-mode-abbrev-table)
  (if haskell-mode-syntax-table
      ()
    (setq haskell-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?{  "(}1"    haskell-mode-syntax-table)
    (modify-syntax-entry ?}  "){4"    haskell-mode-syntax-table)
; partain: out
;    (modify-syntax-entry ?-  "_ 2356" haskell-mode-syntax-table)
;    (modify-syntax-entry ?\f "> b"    haskell-mode-syntax-table)
;    (modify-syntax-entry ?\n "> b"    haskell-mode-syntax-table)
; partain: end out
; partain: in
    (modify-syntax-entry ?-  "_ 23" haskell-mode-syntax-table)
;    (modify-syntax-entry ?\f "> b"    haskell-mode-syntax-table)
;    (modify-syntax-entry ?\n "> b"    haskell-mode-syntax-table)
; partain: end in
    (modify-syntax-entry ?\\ "\\"     haskell-mode-syntax-table)
    (modify-syntax-entry ?*  "_"      haskell-mode-syntax-table)
    (modify-syntax-entry ?_  "_"      haskell-mode-syntax-table)
    (modify-syntax-entry ?'  "_"      haskell-mode-syntax-table)
    (modify-syntax-entry ?:  "_"      haskell-mode-syntax-table)
    (modify-syntax-entry ?|  "."      haskell-mode-syntax-table)
    )
  (set-syntax-table haskell-mode-syntax-table)
  (make-local-variable 'require-final-newline) ; Always put a new-line
  (setq require-final-newline t)	; in the end of file
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'haskell-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "-- ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 60)		; Start of comment in this column
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--[^a-zA-Z0-9]*") ; This matches a start of comment
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'haskell-comment-indent)
  ;;
  ;; Adding these will fool the matching of parens. I really don't
  ;; know why. It would be nice to have comments treated as
  ;; white-space
  ;; 
  ;; (make-local-variable 'parse-sexp-ignore-comments)
  ;; (setq parse-sexp-ignore-comments t)
  ;; 
  (run-hooks 'haskell-mode-hook))		; Run the hook

(defun haskell-mode-version ()
  (interactive)
  (message haskell-mode-version-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some variables for later use

(defvar haskell-open-comment "{-")
(defvar haskell-close-comment "-}")
(defvar haskell-indentation-counter 0
  "count repeated invocations of indent-for-tab-command")
(defvar haskell-literate-flag nil
  "used to guide literate/illiterate behavior, set automagically")

(defun haskell-newline-and-indent ()
  (interactive)
  (setq haskell-literate-flag
	(save-excursion
	  (beginning-of-line)
	  (= (following-char) ?>)))
  (newline)
  (if haskell-literate-flag (insert ">"))
  (haskell-indent-line))

(defun haskell-indent-line ()
  "Indent current line of ordinary or literate Haskell code."
  (interactive)
  (let ((indent (haskell-calculate-indentation-pjt-2)))
    (if (/= (current-indentation) indent)
	(let ((beg (progn
		     (beginning-of-line)
		     (if (= (following-char) ?>) (forward-char 1)) ;LITERATE
		     (point))))
	  (skip-chars-forward "\t ")
	  (delete-region beg (point))
	  (indent-to indent))
      ;; If point is before indentation, move point to indentation
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward "\t ")))))

(defun haskell-calculate-indentation ()
  (save-excursion
    (let ((col (current-column)))
      (while (and (not (bobp))		;skip over empty and comment-only lines
		  (= col (current-column)))
	(previous-line 1)
	(beginning-of-line)			; Go to first non whitespace
	(if (= (following-char) ?>)	;LITERATE
	    (forward-char 1)
	  (if haskell-literate-flag	;ignore illiterate lines
	      (end-of-line)))
	(skip-chars-forward "\t ")		; on the line.
	(setq col (current-column))
	(search-forward-regexp (concat haskell-open-comment "\\|--\\|\n") nil 0)
	(goto-char (match-beginning 0)))
      (search-backward-regexp "\\b\\(where\\|let\\|of\\|in\\)\\b\\|\n" nil 0)
      (if (looking-at "\n")
	  ()
	(setq col (current-column))
	(forward-word 1)
	(skip-chars-forward "\t ")
	(if (looking-at "\\w")
	    (setq col (current-column))
	  (setq col (+ 2 col))))
      col)))

(defun haskell-calculate-indentation-pjt-2 ()
  "Calculate indentation for Haskell program code, versatile version"
  (save-excursion
    (if (eq last-command 'haskell-indentation)
	(setq haskell-indentation-counter (1+ haskell-indentation-counter))
      (setq haskell-indentation-counter -1))
    (setq this-command 'haskell-indentation)
    (let* ((simple-indent (haskell-calculate-indentation))
	   (count haskell-indentation-counter)
	   (min-indent simple-indent)	; minimum indentation found in a non-comment line
	   (last-indent simple-indent)	; indentation of the following non-comment line
	   (return-indent nil)		; computed indentation
	   (comment-depth 0))
      (previous-line 1)
      (if (< haskell-indentation-counter 0) ; 1st tab gives simple indentation
	  (setq return-indent simple-indent))
      (while (not return-indent)
	(if (search-backward-regexp "\\b\\(where\\|let\\|of\\)\\b\\|\n\\|{-\\|-}" nil t 1)
	    (cond
	     ((looking-at haskell-open-comment)
	      (setq comment-depth (1- comment-depth)))
	     ((looking-at haskell-close-comment)
	      (setq comment-depth (1+ comment-depth)))
	     ((= 0 comment-depth)
	      (cond
	       ((looking-at "\n")
		(save-excursion
		  (forward-char 1)
		  (if (= (following-char) ?>)
		      (forward-char 1)
		    (if haskell-literate-flag
			(end-of-line))) ;LITERATE: ignore lines w/o >
		  (skip-chars-forward "\t ")
		  (if (looking-at (concat haskell-open-comment "\\|--\\|\n"))
		      ()
		    (setq last-indent (current-column))
		    (if (< last-indent min-indent)
			(setq min-indent last-indent)))))
	       (t			; looking at a keyword
		(save-excursion
		  (forward-word 1)
		  (skip-chars-forward " \t")
		  (if (and haskell-literate-flag ;LITERATE: ignore lines w/o >
			   (save-excursion
			     (beginning-of-line)
			     (/= (following-char) ?>)))
		      (end-of-line))
		  (if (looking-at (concat haskell-open-comment "\\|--\\|\n"))
		      ()
		    (setq last-indent (current-column)))
		  (if (<= last-indent min-indent)
		      (if (> count 0)
			  (setq count (1- count))
			(setq return-indent last-indent)))
		  (if (< last-indent min-indent)
		      (setq min-indent last-indent)))))))
	  (setq return-indent simple-indent)
	  (setq haskell-indentation-counter -1)))
      return-indent)))

(defun haskell-skip-nested-comment ()
  ;; point looks at opening {-, move over closing -}
  ;; todo: specify what happens on failure, bounds check ...
  (forward-char 2)
  (let ((comment-depth 1))
    (while (> comment-depth 0)
      (search-forward-regexp "{-\\|-}")
      (goto-char (match-beginning 0))
      (setq comment-depth
	    (if (= (following-char) 123) ; code for opening brace
		(1+ comment-depth)
	      (1- comment-depth)))
      (goto-char (match-end 0)))))


;;;seemingly obsolete functions
(defun haskell-inside-of-inline-comment ()
  (let ((bolp (save-excursion
	       (beginning-of-line)
	       (point))))
    (search-backward comment-start bolp t 1)))

(defun haskell-inside-of-nested-comment ()
  (save-excursion
    (let ((count 0))
      (while
	  (search-backward-regexp "\\({-\\|-}\\)" 0 t 1)
	(if (haskell-inside-of-inline-comment)
	    ()
	  (if (looking-at haskell-open-comment)
	      (setq count (1+ count))
	    (setq count (1- count)))))
      (> count 0))))

(defun haskell-inside-of-comment ()
  (or (haskell-inside-of-inline-comment)
      (haskell-inside-of-nested-comment)))

;;;stolen from sml-mode.el
(defun haskell-comment-indent ()
  "Compute indentation for Haskell comments"
  (if (looking-at "^--")
      0
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))
	   comment-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INFERIOR SHELL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar haskell-shell-map nil "The mode map for haskell-shell.")

(defun haskell-shell ()
  "Inferior shell invoking Haskell.
It is not possible to have more than one shell running Haskell.
Like the shell mode with the additional command:

\\[haskell-run-on-file]\t Runs haskell on the file.
\\{haskell-shell-map}
Variables controlling the mode:

haskell-prog-name (default \"hbi\")
    The string used to invoke the haskell program.

haskell-use-right-delim (default \"\\\"\")
haskell-use-left-delim  (default \"\\\"\")
    The left and right delimiter used by your version of haskell, for
    \"load file-name\".

haskell-process-name (default \"Haskell\")
    The name of the process running haskell.

haskell-shell-prompt-pattern (default \"^> *\")
    The prompt pattern.

Runs haskell-shell-hook if not nil."
  (interactive)
  (if (not (process-status haskell-process-name))
      (save-excursion			; Process is not running
	(message "Starting Haskell...")	; start up a new process
	(require 'shell)
	(set-buffer (make-comint haskell-process-name haskell-prog-name))
	(erase-buffer)			; Erase the buffer if a previous
	(if haskell-shell-map		; process died in there
	    ()
	  (setq haskell-shell-map (copy-keymap shell-mode-map))
	  (define-key haskell-shell-map "\C-c\C-f" 'haskell-run-on-file))
	(use-local-map haskell-shell-map)
	(make-local-variable 'shell-prompt-pattern)
	(setq shell-prompt-pattern haskell-shell-prompt-pattern)
	(setq major-mode 'haskell-shell)
	(setq mode-name "Haskell Shell")
	(setq mode-line-format 
	      "-----Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
	(set-process-filter (get-process haskell-process-name) 'haskell-process-filter)
	(message "Starting Haskell...done.")
	(run-hooks 'haskell-shell-hook))))

(defun haskell-process-filter (proc str)
  (let ((cur (current-buffer))
	(pop-up-windows t))
    (pop-to-buffer (concat "*" haskell-process-name "*"))
    (goto-char (point-max))
    (if (string= str "\b\b\b  \b\b\b")
	(backward-delete-char 4)
      (insert str))
    (set-marker (process-mark proc) (point-max))
    (pop-to-buffer cur)))

(defun haskell-pop-to-shell ()
  (interactive)
  (haskell-shell)
  (pop-to-buffer (concat "*" haskell-process-name "*")))

(defun haskell-run-on-file (fil)
  (interactive "FRun Haskell on : ")
  (haskell-shell)
  (save-some-buffers)
  (process-send-string haskell-process-name
	       (concat "load " haskell-use-left-delim (expand-file-name fil)
		       haskell-use-right-delim ";\n")))

(defun haskell-save-buffer-use-file ()
  "Save the buffer, and send a `use file' to the inferior shell
running Haskell."
  (interactive)
  (let (file)
    (if (setq file (buffer-file-name))	; Is the buffer associated
	(progn				; with file ?
	  (save-buffer)
	  (haskell-shell)
	  (process-send-string haskell-process-name
		       (concat "load " haskell-use-left-delim
			       (expand-file-name file)
			       haskell-use-right-delim ";\n")))
      (error "Buffer not associated with file."))))

(defvar haskell-tmp-files-list nil
  "List of all temporary files created by haskell-simulate-send-region.
Each element in the list is a list with the format:

      (\"tmp-filename\"  buffer  start-line)")

(defvar haskell-simulate-send-region-called-p nil
  "Has haskell-simulate-send-region been called previously.")

(defun haskell-make-temp-name (pre)
  (concat (make-temp-name pre) ".m"))

(defun haskell-simulate-send-region (point1 point2)
  "Simulate send region. As send-region only can handle what ever the
system sets as the default, we have to make a temporary file.
Updates the list of temporary files (haskell-tmp-files-list)."
  (let ((file (expand-file-name (haskell-make-temp-name haskell-tmp-template))))
    ;; Remove temporary files when we leave emacs
    (if (not haskell-simulate-send-region-called-p)
	(progn
	  (setq haskell-old-kill-emacs-hook kill-emacs-hook)
	  (setq kill-emacs-hook 'haskell-remove-tmp-files)
	  (setq haskell-simulate-send-region-called-p t)))
    (save-excursion
      (goto-char point1)
      (setq haskell-tmp-files-list
	    (cons (list file
			(current-buffer)
			(save-excursion	; Calculate line no.
			  (beginning-of-line)
			  (1+ (count-lines 1 (point)))))
		  haskell-tmp-files-list)))
    (write-region point1 point2 file nil 'dummy)
    (haskell-shell)
    (message "Using temporary file: %s" file)
    (process-send-string
     haskell-process-name
     ;; string to send: load file;
     (concat "load " haskell-use-left-delim file haskell-use-right-delim ";\n"))))

(defun haskell-remove-tmp-files ()
  "Remove the temporary files, created by haskell-simulate-send-region, if
they still exist. Only files recorded in haskell-tmp-files-list are removed."
  (message "Removing temporary files created by haskell-mode...")
  (while haskell-tmp-files-list
    (condition-case ()
 	(delete-file (car (car haskell-tmp-files-list)))
      (error ()))
    (setq haskell-tmp-files-list (cdr haskell-tmp-files-list)))
  (message "Removing temporary files created by haskell-mode...done.")
  (run-hooks 'haskell-old-kill-emacs-hook))

(defun haskell-send-region ()
  "Send region."
  (interactive)
  (let (start end)
    (save-excursion
      (setq end (point))
      (exchange-point-and-mark)
      (setq start (point)))
    (haskell-simulate-send-region start end)))

(defun haskell-send-buffer ()
  "Send the buffer."
  (interactive)
  (haskell-simulate-send-region (point-min) (point-max)))

(defun haskell-evaluate-expression (h-expr)
  "Prompt for and evaluate an expression"
  (interactive "sExpression: ")
  (let ((str (concat h-expr ";\n"))
	(buf (current-buffer)))
    (haskell-pop-to-shell)
    (insert str)
    (process-send-string haskell-process-name str)
    (pop-to-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; keywords for jwz's font-look-mode (lemacs 19)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq haskell-font-lock-keywords
      (list (concat "\\b\\("
		    (mapconcat 'identity 
			       '("case" "class" "data" "default" "deriving" "else" "hiding"
				 "if" "import" "in" "infix" "infixl" "infixr" "instance"
				 "interface" "let" "module" "of" "renaming" "then" "to"
				 "type" "where")
			       "\\|")
		    "\\)\\b")
	    (list "^\\(#[ \t]*\\(if\\|ifdef\\|ifndef\\|else\\|endif\\|include\\)\\)")
	    (list "\\(^>?\\|\\bwhere\\b\\|\\blet\\b\\)[ \t]*\\(\\(\\w\\|\\s_\\)+\\)\\(\\([^=\n]*\\S.\\)?=\\(\\S.\\|$\\)\\|[ \t]*::\\S.\\).*$"
		    2 'font-lock-function-name-face)
	    (list "\\b\\(data\\|type\\)\\b[ \t]+\\(\\(\\w\\|\\s_\\)+\\)"
		     2 'font-lock-type-face)
	    (list (concat "'\\([^\\]\\|\\\\\\([0-9]+\\|"
			  (mapconcat 'identity
				     '("a" "b" "f" "n" "r" "t" "v" "\\\\" "\"" "'" "&")
				     "\\|")
			  "\\|\\^\\([][_^A-Z@\\\\]\\)"
			  "\\)\\)'") 1 'font-lock-string-face)))

;;; font-lock-keywords for literate style files

(setq haskell-font-lock-keywords-2
      (list (concat "^>.*\\b\\("
		    (mapconcat 'identity 
			       '("case" "class" "data" "default" "deriving" "else" "hiding"
				 "if" "import" "in" "infix" "infixl" "infixr" "instance"
				 "interface" "let" "module" "of" "renaming" "then" "to"
				 "type" "where")
			       "\\|")
		    "\\)\\b")
	    (list "^>\\(.*\\(\\bwhere\\b\\|\\blet\\b\\)\\|\\)[ \t]*\\(\\(\\w\\|\\s_\\)+\\)\\(\\([^=\n]*\\S.\\)?=\\(\\S.\\|$\\)\\|[ \t]*::\\S.\\).*$"
		    3 'font-lock-function-name-face)
	    (list "^>.*\\b\\(data\\|type\\)\\b[ \t]+\\(\\(\\w\\|\\s_\\)+\\)"
		     2 'font-lock-type-face)
	    (list (concat "^>.*'\\([^\\]\\|\\\\\\([0-9]+\\|"
			  (mapconcat 'identity
				     '("a" "b" "f" "n" "r" "t" "v" "\\\\" "\"" "'" "&")
				     "\\|")
			  "\\|\\^\\([][_^A-Z@\\\\]\\)"
			  "\\)\\)'") 1 'font-lock-string-face)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; END OF Haskell-MODE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'haskell-mode)
