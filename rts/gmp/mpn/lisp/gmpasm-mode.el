;;; gmpasm-mode.el -- GNU MP asm and m4 editing mode.


;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.
;;
;; This file is part of the GNU MP Library.
;;
;; The GNU MP Library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or (at your
;; option) any later version.
;;
;; The GNU MP Library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; Commentary:
;;
;; gmpasm-mode is an editing mode for m4 processed assembler code and m4
;; macro files in GMP.  It's similar to m4-mode, but has a number of
;; settings better suited to GMP.
;;
;;
;; Install
;; -------
;;
;; To make M-x gmpasm-mode available, put gmpasm-mode.el somewhere in the
;; load-path and the following in .emacs
;;
;;	(autoload 'gmpasm-mode "gmpasm-mode" nil t)
;;
;; To use gmpasm-mode automatically on all .asm and .m4 files, put the
;; following in .emacs
;;
;;	(add-to-list 'auto-mode-alist '("\\.asm\\'" . gmpasm-mode))
;;	(add-to-list 'auto-mode-alist '("\\.m4\\'" . gmpasm-mode))
;;
;; To have gmpasm-mode only on gmp files, try instead something like the
;; following, which uses it only in a directory starting with "gmp", or a
;; sub-directory of such.
;;
;;	(add-to-list 'auto-mode-alist
;;	             '("/gmp.*/.*\\.\\(asm\\|m4\\)\\'" . gmpasm-mode))
;;
;; Byte compiling will slightly speed up loading.  If you want a docstring
;; in the autoload you can use M-x update-file-autoloads if you set it up
;; right.
;;
;;
;; Emacsen
;; -------
;;
;; FSF Emacs 20.x - gmpasm-mode is designed for this.
;; XEmacs 20.x - seems to work.
;;
;; FSF Emacs 19.x - should work if replacements for some 20.x-isms are
;;    available.  comment-region with "C" won't really do the right thing
;;    though.


;;; Code:

(defgroup gmpasm nil
  "GNU MP m4 and asm editing."
  :prefix "gmpasm-"
  :group 'languages)

(defcustom gmpasm-mode-hook nil
  "*Hook called by `gmpasm-mode'."
  :type 'hook
  :group 'gmpasm)

(defcustom gmpasm-comment-start-regexp "[#;!@C]"
  "*Regexp matching possible comment styles.
See `gmpasm-mode' docstring for how this is used."
  :type 'regexp
  :group 'gmpasm)


(defun gmpasm-add-to-list-second (list-var element)
  "(gmpasm-add-to-list-second LIST-VAR ELEMENT)

Add ELEMENT to LIST-VAR as the second element in the list, if it isn't
already in the list.  If LIST-VAR is nil, then ELEMENT is just added as the
sole element in the list.

This is like `add-to-list', but it puts the new value second in the list.

The first cons cell is copied rather than changed in-place, so references to
the list elsewhere won't be affected."

  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
	 (if (symbol-value list-var)
	     (cons (car (symbol-value list-var))
		   (cons element
			 (cdr (symbol-value list-var))))
	   (list element)))))


(defun gmpasm-delete-from-list (list-var element)
  "(gmpasm-delete-from-list LIST-VAR ELEMENT)

Delete ELEMENT from LIST-VAR, using `delete'.
This is like `add-to-list', but the element is deleted from the list.
The list is copied rather than changed in-place, so references to it elsewhere
won't be affected."

  (set list-var (delete element (copy-sequence (symbol-value list-var)))))


(defvar gmpasm-mode-map
  (let ((map (make-sparse-keymap)))
    
    ;; assembler and dnl commenting
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "\C-c\C-d" 'gmpasm-comment-region-dnl)
    
    ;; kill an M-x compile, since it's not hard to put m4 into an infinite
    ;; loop
    (define-key map "\C-c\C-k" 'kill-compilation)
    
    map)
  "Keymap for `gmpasm-mode'.")


(defvar gmpasm-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; underscore left as a symbol char, like C mode
    
    ;; m4 quotes
    (modify-syntax-entry ?`  "('"  table)
    (modify-syntax-entry ?'  ")`"  table)

    table)
  "Syntax table used in `gmpasm-mode'.

m4 ignores quote marks in # comments at the top level, but inside quotes #
isn't special and all quotes are active.  There seems no easy way to express
this in the syntax table, so nothing is done for comments.  Usually this is
best, since it picks up invalid apostrophes in comments inside quotes.")


(defvar gmpasm-font-lock-keywords
  (eval-when-compile
    (list
     (cons
      (concat
       "\\b"
       (regexp-opt
	'("deflit" "defreg" "defframe" "defframe_pushl"
	  "define_not_for_expansion"
	  "ASM_START" "ASM_END" "PROLOGUE" "EPILOGUE"
	  "forloop"
	  "TEXT" "DATA" "ALIGN" "W32"
	  "builtin" "changecom" "changequote" "changeword" "debugfile"
	  "debugmode" "decr" "define" "defn" "divert" "divnum" "dumpdef"
	  "errprint" "esyscmd" "eval" "__file__" "format" "gnu" "ifdef"
	  "ifelse" "include" "incr" "index" "indir" "len" "__line__"
	  "m4exit" "m4wrap" "maketemp" "patsubst" "popdef" "pushdef"
	  "regexp" "shift" "sinclude" "substr" "syscmd" "sysval"
	  "traceoff" "traceon" "translit" "undefine" "undivert" "unix")
	t)
       "\\b") 'font-lock-keyword-face)))

  "`font-lock-keywords' for `gmpasm-mode'.

The keywords are m4 builtins and some of the GMP macros used in asm files.
L and LF don't look good fontified, so they're omitted.

The right assembler comment regexp is added dynamically buffer-local (with
dnl too).")


;; Initialized if gmpasm-mode finds filladapt loaded.
(defvar gmpasm-filladapt-token-table nil
  "Filladapt token table used in `gmpasm-mode'.")
(defvar gmpasm-filladapt-token-match-table nil
  "Filladapt token match table used in `gmpasm-mode'.")
(defvar gmpasm-filladapt-token-conversion-table nil
  "Filladapt token conversion table used in `gmpasm-mode'.")


;;;###autoload
(defun gmpasm-mode ()
  "A major mode for editing GNU MP asm and m4 files.

\\{gmpasm-mode-map}
`comment-start' and `comment-end' are set buffer-local to assembler
commenting appropriate for the CPU by looking for something matching
`gmpasm-comment-start-regexp' at the start of a line, or \"#\" is used if
there's no match (if \"#\" isn't what you want, type in a desired comment
and do \\[gmpasm-mode] to reinitialize).

`adaptive-fill-regexp' is set buffer-local to the standard regexp with
`comment-start' and dnl added.  If filladapt.el has been loaded it similarly
gets `comment-start' and dnl added as buffer-local fill prefixes.

Font locking has the m4 builtins, some of the GMP macros, m4 dnl commenting,
and assembler commenting (based on the `comment-start' determined).

Note that `gmpasm-comment-start-regexp' is only matched as a whole word, so
the `C' in it is only matched as a whole word, not on something that happens
to start with `C'.  Also it's only the particular `comment-start' determined
that's added for filling etc, not the whole `gmpasm-comment-start-regexp'.

`gmpasm-mode-hook' is run after initializations are complete.
"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gmpasm-mode
        mode-name  "gmpasm")
  (use-local-map gmpasm-mode-map)
  (set-syntax-table gmpasm-mode-syntax-table)
  (setq fill-column 76)

  ;; Short instructions might fit with 32, but anything with labels or
  ;; expressions soon needs the comments pushed out to column 40.
  (setq comment-column 40)

  ;; Don't want to find out the hard way which dumb assemblers don't like a
  ;; missing final newline.
  (set (make-local-variable 'require-final-newline) t)

  ;; The first match of gmpasm-comment-start-regexp at the start of a line
  ;; determines comment-start, or "#" if no match.
  (set (make-local-variable 'comment-start)
       (save-excursion
	 (goto-char (point-min))
	 (if (re-search-forward
	      (concat "^\\(" gmpasm-comment-start-regexp "\\)\\(\\s-\\|$\\)")
	      nil t)
	     (match-string 1)
	   "#")))
  (set (make-local-variable 'comment-end) "")

  ;; If comment-start ends in an alphanumeric then \b is used to match it
  ;; only as a separate word.  The test is for an alphanumeric rather than
  ;; \w since we might try # or ! as \w characters but without wanting \b.
  (let ((comment-regexp
	 (concat (regexp-quote comment-start)
		 (if (string-match "[a-zA-Z0-9]\\'" comment-start) "\\b"))))
    
    ;; Whitespace is required before a comment-start so m4 $# doesn't match
    ;; when comment-start is "#".
    ;; Only spaces or tabs match after, so newline isn't included in the
    ;; font lock below.
    (set (make-local-variable 'comment-start-skip)
	 (concat "\\(^\\|\\s-\\)" comment-regexp "[ \t]*"))

    ;; Comment fontification based on comment-start, matching through to the
    ;; end of the line.
    (add-to-list (make-local-variable 'gmpasm-font-lock-keywords)
		 (cons (concat
			"\\(\\bdnl\\b\\|" comment-start-skip "\\).*$")
		       'font-lock-comment-face))

    (set (make-local-variable 'font-lock-defaults)
	 '(gmpasm-font-lock-keywords
	   t	         ; no syntactic fontification (of strings etc)
	   nil           ; no case-fold
	   ((?_ . "w"))  ; _ part of a word while fontifying
	   ))

    ;; Paragraphs are separated by blank lines, or lines with only dnl or
    ;; comment-start.
    (set (make-local-variable 'paragraph-separate)
	 (concat "[ \t\f]*\\(\\(" comment-regexp "\\|dnl\\)[ \t]*\\)*$"))
    (set (make-local-variable 'paragraph-start)
	 (concat "\f\\|" paragraph-separate))
 
    ;; Adaptive fill gets dnl and comment-start as comment style prefixes on
    ;; top of the standard regexp (which has # and ; already actually).
    (set (make-local-variable 'adaptive-fill-regexp)
	 (concat "[ \t]*\\(\\("
		 comment-regexp
		 "\\|dnl\\|[-|#;>*]+\\|(?[0-9]+[.)]\\)[ \t]*\\)*"))
    (set (make-local-variable 'adaptive-fill-first-line-regexp)
	 "\\`\\([ \t]*dnl\\)?[ \t]*\\'")

    (when (fboundp 'filladapt-mode)
      (when (not gmpasm-filladapt-token-table)
	(setq gmpasm-filladapt-token-table
	      filladapt-token-table)
	(setq gmpasm-filladapt-token-match-table
	      filladapt-token-match-table)
	(setq gmpasm-filladapt-token-conversion-table
	      filladapt-token-conversion-table)
	
	;; Numbered bullet points like "2.1" get matched at the start of a
	;; line when it's really something like "2.1 cycles/limb", so delete
	;; this from the list.  The regexp for "1.", "2." etc is left
	;; though.
	(gmpasm-delete-from-list 'gmpasm-filladapt-token-table
				 '("[0-9]+\\(\\.[0-9]+\\)+[ \t]"
				   bullet))
	  
	;; "%" as a comment prefix interferes with x86 register names
	;; like %eax, so delete this.
	(gmpasm-delete-from-list 'gmpasm-filladapt-token-table
				 '("%+" postscript-comment))
	
	(add-to-list 'gmpasm-filladapt-token-match-table
		     '(gmpasm-comment gmpasm-comment))
	(add-to-list 'gmpasm-filladapt-token-conversion-table
		     '(gmpasm-comment . exact))
	)
      
      (set (make-local-variable 'filladapt-token-table)
	   gmpasm-filladapt-token-table)
      (set (make-local-variable 'filladapt-token-match-table)
	   gmpasm-filladapt-token-match-table)
      (set (make-local-variable 'filladapt-token-conversion-table)
	   gmpasm-filladapt-token-conversion-table)
    
      ;; Add dnl and comment-start as fill prefixes.
      ;; Comments in filladapt.el say filladapt-token-table must begin
      ;; with ("^" beginning-of-line), so put our addition second.
      (gmpasm-add-to-list-second 'filladapt-token-table
				 (list (concat "dnl[ \t]\\|" comment-regexp)
				       'gmpasm-comment))
      ))
  
  (run-hooks 'gmpasm-mode-hook))


(defun gmpasm-comment-region-dnl (beg end &optional arg)
  "(gmpasm-comment-region BEG END &option ARG)

Comment or uncomment each line in the region using `dnl'.
With \\[universal-argument] prefix arg, uncomment each line in region.
This is `comment-region', but using \"dnl\"."

  (interactive "r\nP")
  (let ((comment-start "dnl")
	(comment-end ""))
    (comment-region beg end arg)))


(provide 'gmpasm-mode)

;;; gmpasm-mode.el ends here
