;; Haskell major mode
;; (c) Copyright, Richard McPhee et al. 
;; University of Glasgow, February 1993



;; if .hs is not recognised then put the extension in auto-mode-list

(if (assoc "\\.hs" auto-mode-alist)
    nil
  (nconc auto-mode-alist '(("\\.hs". haskell-mode))))

(if (assoc "\\.hi" auto-mode-alist)
    nil
  (nconc auto-mode-alist '(("\\.hi". haskell-mode))))

(if (assoc "\\.gs" auto-mode-alist)
    nil
  (nconc auto-mode-alist '(("\\.gs". haskell-mode))))

(defvar haskell-mode-syntax-table nil
  "Syntax table for haskell-mode buffers.")

(defvar haskell-mode-abbrev-table nil
  "Abbrev table for haskell-mode buffers.")

(defvar haskell-mode-map (make-sparse-keymap)
  "Keymap for haskell-mode-buffers.")



;;; Here are the keymaps used in haskell-mode

(define-key haskell-mode-map "\M-;"  'haskell-insert-comment)
(define-key haskell-mode-map "\C-c=" 'haskell-insert-concat)
(define-key haskell-mode-map "\C-c;" 'set-haskell-comment-column)
(define-key haskell-mode-map "\C-c+" 'set-haskell-concat-column)
(define-key haskell-mode-map "\C-cn" 'set-haskell-indent-offset)
(define-key haskell-mode-map "\C-cl" 'set-haskell-list-offset)
(define-key haskell-mode-map "\C-ci" 'set-haskell-if-offset)
(define-key haskell-mode-map "\C-ce" 'set-haskell-let-offset)
(define-key haskell-mode-map "\C-cc" 'set-haskell-case-offset)
(define-key haskell-mode-map "\C-ct" 'set-haskell-then-offset)
(define-key haskell-mode-map "\C-co" 'set-haskell-comp-offset)
(define-key haskell-mode-map "\C-cw" 'set-haskell-where-offset)
(define-key haskell-mode-map "\C-cg" 'goto-line)
(define-key haskell-mode-map "\C-j"  'haskell-reindent-then-newline-and-indent)
(define-key haskell-mode-map "\t"    'haskell-indent-line)
(define-key haskell-mode-map "}"     'electric-haskell-brace)
(define-key haskell-mode-map "]"     'electric-haskell-brace)
(define-key haskell-mode-map ")"     'haskell-insert-round-paren)
(define-key haskell-mode-map "\C-cr" 'haskell-indent-region)
(define-key haskell-mode-map "\C-cf" 'haskell-further-indent)
(define-key haskell-mode-map "\C-cb" 'haskell-lesser-indent)
(define-key haskell-mode-map "\177"  'backward-delete-char-untabify)
(define-key haskell-mode-map "\M-\C-\177" 'delete-horizontal-space)
					
(defun haskell-set-local-vars ()
  "Set the local variables for haskell-mode."
  (kill-all-local-variables)

  (setq indent-line-function 'haskell-indent-line)

  (make-local-variable 'haskell-std-list-indent)
  ;;Non-nil means indent to the offset, 'haskell-list-offset' in a bracket rather than
  ;; moving to the next word afer a function name
  (setq haskell-std-list-indent t)

  (make-local-variable 'haskell-nest-ifs)
  ;;Non-nil means that 'if' statements are nested ie. lined up with `if' not `else'.
  (setq haskell-nest-ifs nil)

  (make-local-variable 'haskell-align-else-with-then)
  ;;Non-nil means align an `else' under it's corresponding `then'
  (setq haskell-align-else-with-then nil)


  ;;The local vars for 'where' indentation

  (make-local-variable 'haskell-align-where-with-eq)
  ;;Non-nil means align a 'where' under it's corresponding equals sign
  (setq haskell-align-where-with-eq t)

  (make-local-variable 'haskell-align-where-after-eq)
  ;;Non-nil means align a 'where' after it's corresponding equals sign
  (setq haskell-align-where-after-eq nil)

  (make-local-variable 'haskell-std-indent-where)
  ;;put the 'where' the standard offset ie. 'haskell-indent-offset'
  (setq haskell-std-indent-where nil)	


  (make-local-variable 'haskell-always-fixup-comment-space)
  ;;Non-nil means always insert a (single) space after a comment, even
  ;; if there is more or less than one.
  (setq haskell-always-fixup-comment-space t)

  
  (make-local-variable 'haskell-indent-offset)
  ;;Extra indentation for a line continued after a keyword.
  (setq haskell-indent-offset 4)

  (make-local-variable 'haskell-list-offset)
  ;;Extra indentation for continuing a list.
  (setq haskell-list-offset 4)
  
  (make-local-variable 'haskell-comp-offset)
  ;;Extra indentation for a list comprehension.
  (setq haskell-comp-offset 4)
  
  (make-local-variable 'haskell-case-offset)
  (setq haskell-case-offset 4)

  (make-local-variable 'haskell-where-offset)
  (setq haskell-where-offset 4)

  (make-local-variable 'haskell-let-offset)
  (setq haskell-let-offset 4)

  (make-local-variable 'haskell-then-offset)
  (setq haskell-then-offset 0)

  (make-local-variable 'haskell-if-offset)
  (setq haskell-if-offset 4)

  (make-local-variable 'haskell-comment-column)
  (setq haskell-comment-column 35)
  
  (make-local-variable 'haskell-concat-column)
  (setq haskell-concat-column 69)
  
  (make-local-variable 'haskell-where-threshold)
  (setq haskell-where-threshold 35)
  
  (make-local-variable 'line-comment)
  (setq line-comment "-- ")

  (make-local-variable 'haskell-indent-style)
  (setq haskell-indent-style "none"))


(defun haskell-set-syntax-table ()
  "Set the syntax table for Haskell-mode."
  (setq haskell-mode-syntax-table (make-syntax-table))
  (set-syntax-table haskell-mode-syntax-table)
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\\ "\\")
  (modify-syntax-entry ?\' "w")
  (modify-syntax-entry ?_  "w")
  (modify-syntax-entry ?#  "_")
  (modify-syntax-entry ?$  "_")
  (modify-syntax-entry ?%  "_")
  (modify-syntax-entry ?:  "_")
  (modify-syntax-entry ??  "_")
  (modify-syntax-entry ?@  "_")
  (modify-syntax-entry ?!  "_")
  (modify-syntax-entry ?^  "_")
  (modify-syntax-entry ?~  "_")
  (modify-syntax-entry ?-  "_ 12")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?{  "(}")
  (modify-syntax-entry ?}  "){")
  (set-syntax-table haskell-mode-syntax-table))



(defun haskell-mode ()
  "Major mode for editing Haskell code.
Linefeed reindents current line, takes newline and indents.
Tab indents current line for Haskell code.
Functions are seperated by blank lines.
Delete converts tabs to spaces as it moves back.
\\{haskell-mode-map}
Variables controlling indentation style:
 haskell-indent-offset
    Standard extra indentation for continuing Haskell
    code under the scope of an expression.  The default is 4.

 haskell-list-offset
    Extra indentation for indenting in a list.  Used if variable
    haskell-std-list-indent is non-nil.  The default is 4.

 haskell-comp-offset
    Extra indentation for continuing a list comprehension.  
    The default is 4.

 haskell-case-offset
    Standard extra indentation for continuing Haskell
    code under the scope of an expression.  The default is 4.

 haskell-where-offset
    Standard extra indentation for continuing Haskell
    code under the scope of a `where'.  The default is 4.

 haskell-let-offset
    Standard extra indentation for continuing Haskell
    code under the scope of a `let'.  The default is 4.

 haskell-then-offset
    Standard extra indentation for a `then' beyond
    its corresponding `if'.  The default is 0.

 haskell-if-offset
    Standard extra indentation for continuing Haskell
    code under the scope of an `if'.  The default is 4.

 haskell-comment-column
    Column to which line comments `--' will be inserted.
    The default is 35.

 haskell-concat-column
    Column to which concatenation operator `++' will be inserted.
    The default is 69.

 haskell-where-threshold
    Column beyond which a `where' will be indented to the
    start of a line (to avoid spilling over lines).
    The default is 35.

 set-haskell-indent-offset (C-c i)
    Changes the default value of the local variable,
    haskell-indent-offset.  May be a number from 0-10.

 set-haskell-list-indent (C-c l)
    Change the value of the local variable, 
    haskell-list-offset.  May be a number from 0-100.

 set-haskell-comment-column (C-x ;)
    Changes the value of the local variable,
    haskell-comment-column.  May be any number from 0-100."

  (interactive)
  (haskell-set-local-vars)
  (haskell-set-syntax-table)
  (use-local-map haskell-mode-map)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell") 
  (define-abbrev-table 'haskell-mode-abbrev-table ()))




;;; Returns the indentation column for a comment on this line.
;;; The point is positioned at the last char of any code on the line.

(defun haskell-comment-indent ()
  "Returns the indentation for a comment on the given line.
If the line has code on it or the point is not at the beginning of the line,
then indent to indent-column.
Otherwise, don't indent."
  (cond ((or (haskell-code-on-linep)	
	     (not (bolp)))		 
	 ;;There is code before the haskell-comment-column
	 ;; or not at the beginning of the line
	 ;;Return the largest of
	 ;; the current column +1 and the haskell-comment-column
	 (max (1+ (current-column))	
	      haskell-comment-column))		
	(t
	 ;;Otherwise, return 0
	 0)))



;;; Returns whether a comment is on the current line
;;; Search from bol, and beware of "--", {-- etc!
;;; DOES NOT RECOGNISE {- COMMENTS YET or -- within a string

(defun haskell-comment-on-linep ()
  "Returns the truth value of whether there is a '--' comment on the current line."
  (save-excursion
    (beginning-of-line)		
    (looking-at ".*--")))


;;; This doesn't account for comments '{-'.  Test explicitly if you use this function!

(defun haskell-code-on-linep ()
  "Returns a truth value as to whether there is code on the current line."
  (save-excursion
    (beginning-of-line)
    (not
     ;; Code on line if not looking at a comment directly
     ;; and the line is not blank
     (or
	  (looking-at "^[ \t]*--")	
	  (looking-at "^[ \t]*$")))))	


;;; Insert a Haskell "--" comment on the current line.
;;; Move to the comment position if there's already a comment here.
;;; Otherwise, the comment is inserted either at the comment column
;;; or one column after the last non-space character, whichever is further
;;; to the right.
;;; This function is executed by M-;

(defun haskell-insert-comment ()
  "Inserts a '--' comment on the given line."
  (interactive)
  (cond ((haskell-comment-on-linep)
	 ;;There is a comment on the line
	 ;;Just reindent existing comment
	 (haskell-reindent-comment))	
	(t
	 (if (haskell-code-on-linep)
	     ;;There is code on the line
	     ;; and guarenteed that a comment
	     ;; does not already exist.
	     ;;Move to the last nonspace char
	     ;; (there may be spaces after the last char)
	     (progn
	       (end-of-line)			
	       (skip-chars-backward " \t")))
	 ;;Indent to required level
	 ;; and insert the line comment '--'
	 (indent-to (haskell-comment-indent)) 
	 (insert line-comment))))		


;;; Reindents a comment.
;;; The comment is indented according to the normal rules.
;;; Skips over ---- and following spaces or tabs

(defun haskell-reindent-comment ()
  "Indents a comment on a line to keep it at haskell-comment-column,
if possible.
It is guaranteed that a comment exists on the current line."
    (beginning-of-line)
    ;;Go back to beginning of comment 
    (re-search-forward "--")		
    (forward-char -2)
    ;;Delete all spaces and reindent to
    ;; the correct location.
    (delete-horizontal-space)		
    (indent-to (haskell-comment-indent)) 
    ;;Move past the comment and insert
    ;; only one space between it and the text.
    ;;Leave point just after comment.
    (skip-chars-forward "- \t")
    (if haskell-always-fixup-comment-space
	(progn
	(fixup-whitespace)			
	(forward-char 1))))



;;; Inserts a haskell concatenation operator, `++', at the
;;; column dictated by haskell-concat-column

(defun haskell-insert-concat()
  "Inserts a `++' operator on the given line."
  (interactive)
  (end-of-line)			
  (skip-chars-backward " \t")
  ;;Indent to required level
  ;; and insert the concat operator `++'
  (indent-to (haskell-concat-indent)) 
  (insert "++"))



;;; Returns the indentation column for a concatenation operator on this line.
;;; The point is positioned at the last char of any code on the line.

(defun haskell-concat-indent ()
  "Returns the indentation for a concat operator on the given line."
  (max (1+ (current-column))	
       haskell-concat-column))	



;;; Returns the indentation of the current line of haskell code.
;;; A blank line has ZERO indentation

(defun haskell-current-indentation ()
  "Returns the indentation for the current haskell line. A blank line has 
indentation zero."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*$")
	;;The line is empty
	;; so the indentation is zero
	0
      ;;Otherwise find the normal value of indentation
      (current-indentation))))		



;;; Returns the indentation of the previous line of haskell code.
;;; A blank line has ZERO indentation

(defun haskell-previous-indentation ()
  "Returns the previous line's indentation as Haskell indentation."
  (save-excursion
    (if (not (bobp))
	;;Not at the start of the buffer
	;; so get the previous lines indentation
	(progn
	  (forward-line -1)
	  (haskell-current-indentation))
      ;;We are at the start of buffer
      ;;There is no previous line; Indent is zero
      0)))				



;;; Move back to the last line which is aligned in the left column.
;;; Ignores comments and blank lines.
;;; The point is left at the beginning of the line.

(defun haskell-back-to-zero-indent ()
  "Moves point to last line which has zero as indentation."
  ;;Not at the beginning of buffer.
  ;;Continue to go to the previous line until
  ;; we find a line whose indentation is non-zero.
  ;;Blank lines and lines containing only comments
  ;; are ignored.
  (beginning-of-line)
  (while (and
	  (or (not (zerop (haskell-current-indentation)))	
	      (looking-at "^[ \t]*\\($\\|--\\)"))
	    (not (bobp)))
    (haskell-backward-to-noncomment)
    (beginning-of-line)))



;;; Find the last symbol, usually an equality.

;;; Note: we check for "=" as a complete WORD (and ignore
;;; comments) when searching for this.  Ie. an `=' may be
;;; surrounded only by a letter, digit, or whitespace .
;;; Strings are not considered.
;;; Don't go beyond the first character in the (possibly narrowed) buffer.
;;;   From the beginning of the line,
;;;     find the comment position (or end-of-line)
;;;     search forward to this position, looking for a "where"
;;;     If one's found, then search forward for "\b=\b"
;;;        If there's no equality sign then
;;;		search forward from the start of the line for an equals
;;;	   Otherwise we found it.
;;;    If there's no where then search forward for an equals, as above.
				 
(defun haskell-back-to-symbol (exp)
  "Goes backward from point until a symbol, EXP, is found.
The point is left at the first symbol matching the context 
of the haskell code."
  (let* ((found nil)
	 (symbol (concat "[ \ta-z0-9A-Z]" exp "[ \t\na-z0-9A-Z]"))
	 eol-limit
	 bol-limit
	 (zero-indent (save-excursion
			(haskell-back-to-zero-indent)
			(point)))
	 (initial-depth (car (parse-partial-sexp
			      (point)
			      zero-indent))))
	
    (while (and (not found)
		(> (point) zero-indent))
      ;;Not found and point > point min
      ;;Record the limit of search for the beginning and
      ;; end of the line.
      (setq eol-limit (point))	
      (beginning-of-line)
      (setq bol-limit (point))	
      (goto-char eol-limit)		
      (re-search-backward "\\bwhere\\b" bol-limit 't)
      ;;Search back from the end of the line
      ;; to find the most recent 'where'.

      (cond ((and (re-search-backward symbol bol-limit 't)
		  (= initial-depth
		     (car (parse-partial-sexp
			   (point)
			   zero-indent))))
	     ;;Found a symbol sign surrounded by
	     ;; a letter, digit or space only, or at the
	     ;; beginning of the buffer and they are at
	     ;; the same depth level
	     (setq found 't))
	    ((and (re-search-backward symbol bol-limit 't)
		  (zerop
		   (car (parse-partial-sexp
			 (point)
			 zero-indent))))
	     ;; Found a symbol and it is not in any parens
	     (setq found 't))
	    ;;Otherwise, go back a line.
	    (t (haskell-backward-to-noncomment))))
    (if found
	(forward-char 1))))


;;; Goes back to the last keyword.  The point is left at the
;;; beginning of the keyword.
;;; The words recognised are:
;;;   `case',`of',`where',`let',`in',`if',`then',`else'

(defun haskell-back-to-keyword ()
  "Goes backward from point until a keyword is found.
The point is left after the first keyword."
  (let* ((found nil)
	 eol-limit
	 bol-limit
	 (zero-indent (save-excursion
			(haskell-back-to-zero-indent)
			(point)))
	 (initial-depth (car (parse-partial-sexp
			      (point)
			      zero-indent))))

    (while (and (not found)
		(>= (point) zero-indent))
      ;;Not found and point > point min
      ;;Go back past any comment.
      ;;Record the limit of search for the beginning and
      ;; end of the line.
      (setq eol-limit (point))	
      (beginning-of-line)
      (setq bol-limit (point))	
      (goto-char eol-limit)		
      (if (and (re-search-backward
		"\\b\\(case\\|of\\|where\\|let\\|in\\|if\\|then\\|else\\)\\b"
		bol-limit 't)
	       (= initial-depth
		  (car (parse-partial-sexp
			(point)
			zero-indent))))
	  ;;Found a keyword and it is at the same level as the initial position
	  (progn
	    (setq found 't)
	    (forward-word 1))
	;;Otherwise, go back a line.
	(haskell-backward-to-noncomment)))))



;;; Returns the end of line (point) of the current line, excluding any
;;; line comments on it.

(defun haskell-eol ()
  "Returns the end (point) of the current line, excluding any line comments."
  (save-excursion
    (end-of-line)
    (let ((eol-limit (point)))	
      (beginning-of-line)
      (if (search-forward "--" eol-limit 'move-to-eol)
	  ;;Found a '--' 
	  ;;So move to the beginning of the comment
	  ;;If fail then move to end of line
	  (forward-char -2)))
    (point)))



;;; Returns whether or not the current line contains an equality outwith a
;;; comment.  The equality may only be surrounded by a letter, digit or
;;; whitespace. 

(defun haskell-looking-at-eqp ()
  "Returns whether or not the current line contains an equality outwith a
comment."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[ \ta-z0-9A-Z]=[ \t\na-z0-9A-Z]" (1+ (haskell-eol)) 't)))
	  
	  
;;; This function does not require all keywords, just those which
;;; may have a bracket before them.
(defun haskell-looking-at-keywordp ()
  "Returns whether or not there is a keyword after the point outwith a
comment."
  (save-excursion
    (re-search-forward 
     "\\(\\(=>\\|=\\|++\\|->\\|<-\\|::\\)\\|\\b\\(case\\|of\\|if\\|then\\|else\\|let\\|in\\)\\b\\)"
     (haskell-eol) 't)))
	  
	  
;;; This function returns whether or not there is a keyword contained in
;;; the region START END.  START < END.

(defun haskell-keyword-in-regionp (start end)
  "Returns whether or not there is a keyword between START and END."
  (save-excursion
    (goto-char start)
    (let ((found nil)
	  (eol-limit (haskell-eol)))
      (while (and (not found) (< (point) end))
	(if (> eol-limit end)
	    (setq eol-limit end))
	(if (re-search-forward
	     "\\b\\(case\\|of\\|if\\|then\\|else\\|let\\|in\\)\\b"
	     eol-limit 'move)
	    (setq found t)
	  ;;Otherwise, have not found a keyword.  Now at haskell-eol.
	  (if (< (point) end)
	      ;;We still have an area to search
	      ;; so go forward one line
	      (progn
		(beginning-of-line)
		(forward-line 1)
		(setq eol-limit (haskell-eol))))))
      ;;found is `t' or point >= end
      found)))
	 

;;;  Goes back to the last line which is not entirely commented out.
;;;  The point is left just before the comment.  
	  
(defun haskell-backward-to-noncomment ()
  "Sets the point to the last char on the line of Haskell code before a comment."
  (let ((comment 't)
	(limit (point-min)))
      (while (and comment (> (point) limit))
	;; comment is true and point > limit
	(beginning-of-line)
	(if (< (forward-line -1) 0)
	    ;;This was the first line in the buffer
	    (setq comment nil)
	  ;;Otherwise, this was not the first line
	  (if (not (looking-at "^[ \t]*\\($\\|--\\)"))
	      ;;There is not a comment at the beginning of the line
	      ;; and the line is not blank
	      (progn
		;;The line is either blank or has code on it.
		(setq comment nil)
		(goto-char (haskell-eol))))))

      ;;return point
      (point)))



;;; Indents a region (by applying "tab" to each line).
;;; The marker upper-marker is set to the end of the region.
;;; We indent from the beginning of the region to this marker.
;;; Implements C-c r.
		      
(defun haskell-indent-region ()
  "Indents the region between the point and mark."
  (interactive)
  (let ((lower-limit (min (point) (mark)))
	(upper-limit (max (point) (mark))))
    (indent-region lower-limit upper-limit 'nil)))
      


;;; Implements TAB.
;;; This actually indents a line.
;;; Eventually it will handle a line split at any point,

(defun haskell-indent-line ()
  "Indent current line as Haskell code.
Keeps the point at the same position on the line unless the 
point is less then the current indentation, in which case the 
point is moved to the first char."
  (interactive)
  (save-excursion
    (let ((indent (haskell-calculate-indentation)))
      (beginning-of-line)
      (delete-horizontal-space)
      ;;Kill any spaces that may preceed the code
      ;; and reindent to the correct level.
      (indent-to indent)))
  (if (< (current-column) (current-indentation))
      ;;The point is in the indentation
      ;; so move to the first char on the line
      (move-to-column (current-indentation))))



;;; This is the haskell version of the Emacs function
;;; reindent-then-newline-and-indent.  It was necessary
;;; to write this because the Emacs version has the 
;;; terrible property of deleting whitespace BEFORE 
;;; reindenting the original line.

(defun haskell-reindent-then-newline-and-indent ()
  "Reidents the current line of Haskell code then takes a
newline and indents this new line."
  (interactive)
  (skip-chars-backward " \t")
  (haskell-indent-line)
  (newline)
  (delete-horizontal-space)
  (haskell-indent-line))



;;; Returns whether the first word of the last line with zero indentation 
;;; is the same as the first word of the current line.
;;; This function is based on the (reasonable?) assumption that 
;;; a function definition occurs on the left hand margin.
;;; This is not quit reasonable since recusive functions are not
;;; recognised.

(defun haskell-continued-fn-defp ()
  "Returns whether the first word on the last line with zero indentation
matches the first word on the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;;Goto the first non space char 
    (haskell-word-eq (point)
		     (save-excursion
		       (forward-line -1)
		       (haskell-back-to-zero-indent)
		       (point)))))


;;; Returns whether two words are the same.
;;; The beginning of both words are given as their
;;; respective points in the buffer.  

(defun haskell-word-eq (current-pos previous-pos)
  (let ((OK 't))
    (goto-char previous-pos)
    ;;We shall compare the two words starting
    ;; at previous-pos and current-pos.
      (while (and OK (looking-at "\\S-"))
	;;OK and looking at a word constituent
	(if (eq (char-after current-pos) 
		(char-after previous-pos))
	    ;;The two chars are the same
	    (progn
	      ;;Increment the two postions
	      ;; and update location of point
	      (setq current-pos (1+ current-pos))
	      (setq previous-pos (1+ previous-pos))
	      (goto-char previous-pos))
	  ;;The two chars are different
	  ;; so set OK to be false
	  (setq OK 'nil)))

      ;;Return the value of OK
      OK))				
      



;;; This function returns the column of the last unbalanced
;;; expression.
;;; It is called when an keyword is found.  The point is 
;;; initially placed before the corresponding keyword.
;;; The function looks at every word to see if it is a
;;; `let' or `in'.  Each word must be outwith a comment.

(defun haskell-last-unbalanced-key-column (open close)
  "Returns the column of the last unbalanced keyword, open."
  (save-excursion
    (let ((original-pos (point))
	  (bol-limit (save-excursion
		       (beginning-of-line)
		       (setq bol-limit (point))))
	  (depth  1))
      (setq open (concat "\\b" open "\\b"))
      (setq close (concat "\\b" close "\\b"))
      (while (and
	      (> depth 0)
	      (> (point) (point-min)))
	(forward-word -1)
	(if (< (point) bol-limit)
	    ;;Moved past the beginning of line limit
	    ;; so go back to the previous line past
	    ;; any comments.
	    (progn
	      (goto-char original-pos)
	      (haskell-backward-to-noncomment)
	      (setq original-pos (point))
	      (setq bol-limit (save-excursion
				(beginning-of-line)
				(point))))
	  ;;Otherwise, still on the same line
	  (if (looking-at open)
	      ;;This word is an open keyword
	      (setq depth (1- depth))
	    ;;Otherwise,
	    (if (looking-at close)
		;;This word is a close keyword
		(setq depth (1+ depth))))))

      (if (string= open "\\bif\\b")
	  ;;The argument is `if'
	  (if (not (save-excursion (skip-chars-backward " \t") (bolp)))
	      ;;There is something before the `if'
	      (if (and (save-excursion
			 (forward-word -1)
			 (looking-at "\\belse\\b"))
		       (not haskell-nest-ifs))
		  ;;There is an `else' before the 'if'
		  (forward-word -1))))
      
      
      (current-column))))
	
	  

;;; Return the indentation for a line given that we expect a `where'.
;;; The point lies on the corresponding symbol 
;;; that the `where' scopes over.

(defun haskell-indent-where ()
  "Return the indentation for a line, given that we expect a `where'
clause."
  (let ((symbol (if (looking-at "=")
		    "="
		  "->")))
    
    (cond ((or haskell-std-indent-where
	       (> (current-column) haskell-where-threshold))
	   ;;Set indentation as the sum of the previous
	   ;; line's layout column and the standard offset
	   ;; (ie. 'haskell-where-offset)
	   (save-excursion
	     (beginning-of-line)
	     (cond ((looking-at (concat "^[ \t]*" symbol))
		    ;;The line starts with the symbol
		    (setq indent (current-indentation)))
		   ((looking-at "^[ \t]*where\\b")
		    ;;The line starts with a 'where'
		    (forward-word 1)
		    (skip-chars-forward " \t")
		    (setq indent (+ (current-column) haskell-where-offset)))
		   (t
		    ;;The line begins on the layout column
		    (setq indent (+ (current-indentation) 
				    haskell-indent-offset))))))
	  ((or haskell-align-where-with-eq
	       haskell-align-where-after-eq)
	   (if (looking-at (concat symbol "[ \t]*$"))
	       ;;The symbol is at the end of the line
	       (setq indent (+ (current-indentation)
			       haskell-where-offset))
	     (save-excursion
	       ;;Set the indentation as required
	       (if haskell-align-where-after-eq
		   (skip-chars-forward (concat symbol " \t")))
	       (setq indent (current-column))))))))



;;; Calculates the indentation for the current line.
;;; When we come here, we are in a line which we want to indent.
;;; We should leave the point at the same relative position it
;;; was in before we called the function, that is, if a line
;;; is already correctly indented, nothing happens!

;;; The main problems are handling "where" definitions
;;; and the syntax of expressions when these are continued
;;; over multiple lines (e.g. tuples, lists, or just plain
;;; bracketed expressions).  Watch out for let ... in, too!

;;; For example, think about the following tricky cases:

;;;    f x = x + <NL>
      
;;;    f x = [  x + y, <NL>

;;;    f x = [  <NL>

;;;    f x = [  -- start of a large list
;;;             -- which I'm commenting in as I go
;;;  <TAB>

(defun haskell-calculate-indentation ()
  "Returns the indentation level for the current line of haskell code."
  (save-excursion
    (let ((indent 0)
	  (eol-position (point)))
      (beginning-of-line)
      (cond ((bobp)
	     ;;We are at the beginning of the buffer so do nothing at all
	     (setq indent 0))	

	    ((looking-at "^[ \t]*--")
	     ;;There is a comment on the line by itself
	     ;;Leave it the way it is
	     (setq indent (current-indentation)))

	    ((looking-at "^[ \t]*\\(data\\|type\\|module\\|import\\|instance\\)\\b")
	     ;;There is a 'data', 'type', 'module' or 'import' at start of line
	     (setq indent 0))

	    ((haskell-continued-fn-defp)
	     ;;This is clearly same function
	     ;; so set indent to be 0
	     (setq indent 0))
		  
	    ((looking-at "^[ \t]*[]}]")
	     ;;There is a "]" or "}" at the start of the line
	     (let ((state (parse-partial-sexp (match-end 0)
					      (save-excursion
						(haskell-back-to-zero-indent)
						(point)))))
	       (if (>= (car state) 0)
		   ;;Since the point is just after a parenthesis
		   ;; it has a match if the depth is >= 0
		   (save-excursion
		     (goto-char (nth 2 state))
		     ;;Move to the match.
		     (if (not
			  (save-excursion
			    (skip-chars-backward " \t")
			    (bolp)))
			 ;;There is something before the brace.
			 (progn
			   (let ((initial-pos (point)))
			     (forward-word -1)
			     (if (not (looking-at
				       "\\(let\\|where\\)"))
				 ;;The word is not `where' or `let'
				 ;; so go back.
				 (progn
				   (goto-char initial-pos)
				   (skip-chars-forward " \t"))))))
		     (setq indent (current-column)))
		 (setq indent 0))))

	    ((looking-at "^[ \t]*\\(->\\|=>\\)")
	     ;; '->' or '=>' at start of line
	     (save-excursion
	       (haskell-backward-to-noncomment)
	       ;;Go back to previous line
	       (let ((eol-limit (point)))
		 (beginning-of-line)
		 (if (re-search-forward "::" eol-limit 't)
		     ;;There is a '::' on this (previous) line
		     ;; set indent to be at the start of it
		     (setq indent (- (current-column) 2))
		   ;;Otherwise copy this (previous) line's indentation
		   (setq indent (current-indentation)))))) 
			     
	    ((looking-at "^[ \t]*where\\b")
	     ;;There is a 'where' at the start of the line
	     ;;Look for the equality (which will not
	     ;; be on this line).
	     (haskell-backward-to-noncomment)
	     (goto-char (max (save-excursion
			       (haskell-back-to-symbol "=")
			       (point))
			      (save-excursion
			       (haskell-back-to-symbol "->")
			       (point))))
	     (setq indent (haskell-indent-where)))

	    ((looking-at "^[ \t]*then\\b")
	     ;;The first thing on the line is a `then'
	     (setq indent (+ (haskell-last-unbalanced-key-column "if" "then")
			     haskell-then-offset)))

	    ((looking-at "^[ \t]*else\\b")
	     ;;The first thing on the line is a `else'
	     (if haskell-align-else-with-then
		 (setq indent (haskell-last-unbalanced-key-column "then" "else"))
	       (setq indent (haskell-last-unbalanced-key-column "if" "else"))))
			     
	    ((looking-at "^[ \t]*|")
	     ;;There is a `|' at beginning of line
	     (save-excursion
	       (let ((state
		     (parse-partial-sexp (save-excursion
					   (haskell-back-to-zero-indent)
					   (point))
					 (point))))
		 (if (not (or (nth 3 state) (nth 4 state)))
		     ;;Not in a comment or string
		     (if (> (car state) 0)
			 ;;In an unbalanced parenthesis.
			 (progn
			   (goto-char (nth 1 state))
			   ;;Move to the beginning of the unbalanced parentheses
			   (if (and (looking-at "\\[")
				    (search-forward "|" (haskell-eol) 't))
			       ;;It is a list comprehension
			       (setq indent (1- (current-column)))
			     (setq indent (+ (current-column)
					     haskell-comp-offset))))
		       ;;Otherwise, not in an unbalanced parenthesis
		       (setq indent (save-excursion
				      (haskell-back-to-symbol "=")
				      (cond ((not (looking-at "="))
					     ;;Did not find an equals
					     (+ (haskell-previous-indentation)
						haskell-indent-offset))
					    ((save-excursion
					       (beginning-of-line)
					       (looking-at "^[ \t]*data\\b"))
					     ;;There is a `data' at beginning
					     (setq indent (current-column)))
					    ((save-excursion
					       (beginning-of-line)
					       (search-forward
						"|" (haskell-eol) 't))
					     ;;There is a `|' on this line
					     ;; so set this to be the indent
					     (save-excursion
					       (goto-char (match-beginning 0))
					       (current-column)))
					    (t
					     ;;Otherwise, set `=' as indent
					     (current-column))))))))))
		   
	    ((looking-at "^[ \t]*=")
	     ;;There is an equals at the start of the line
	     ;;Set the indentation to be the previous line's 
	     ;; indentation plus the standard offset
	     (setq indent (+ haskell-indent-offset
			     (haskell-previous-indentation))))

	    ((looking-at "^[ \t]*in\\b")
	     ;;The line starts with 'in'
	     (beginning-of-line)
	     (setq indent (haskell-last-unbalanced-key-column "let" "in")))

	    ((looking-at "^[ \t]*of\\b")
	     ;;The line starts with `of'
	     (beginning-of-line)
	     (setq indent (haskell-last-unbalanced-key-column "case" "of")))

	    ((looking-at "^.*::")
	     ;;There is a '::' in the line
	     ;;There are several possibilities for indentation
	     (if (looking-at "[ \t]*::")
		 ;;The '::' is the first thing on the line
		 ;; so set indent to be the previous line's
		 ;; indentation plus the standard offset
		 (setq indent (+ (haskell-previous-indentation)
				 haskell-indent-offset))
	       (save-excursion
		 ;;Otherwise, the '::' is contained in the line somewhere
		 ;; so use contextual indentation
		 (setq indent (haskell-context-indent)))))
	   
	    (t
	     ;;Do not recognise the first word on the line.
	     (setq indent (haskell-context-indent))))
      
      indent)))				;return indent as indentation value



;;; Returns the indentation for the current line by looking at the 
;;; previous line to give clues to the indentation.

(defun haskell-context-indent ()
  "Returns the indentation for the current line by looking at 
the previous line to dictate the indentation."
  (save-excursion
    (let ((original-position (point))
	  indent)
      (beginning-of-line)
      (if (bobp)
	  ;;At the beginning of the buffer
	  (setq indent 0)
	;;Otherwise, we are not at the beginning of the buffer
	(haskell-backward-to-noncomment)
	(let ((eol-limit (point))
	      ;;Record the (upper) limit for any search on this line
	      bol-limit
	      (paren-indent 'nil))
	  ;;`paren-indent' flags whether we are indenting a list or not
	  (beginning-of-line)
	  (setq bol-limit (point))
	  ;;Record the (lower) limit for any search on this line
	  (goto-char eol-limit) ;goto the end of the line
	  (flag)
	  (if (save-excursion
		(goto-char eol-limit)
		(and (re-search-backward
		      "[])][^][()]*" bol-limit 't)
		     (save-excursion
		       (goto-char (match-beginning 0))
		       (not (haskell-looking-at-keywordp)))))
			
	      ;;There is a close parenthesis at the end of the line
	      ;; followed by anything except "(", ")", "[", "]"
	      ;; or a keyword
	      (progn
		;;Search back for the close parenthesis
		;; and move to just after it.
		(re-search-backward "[])]" bol-limit 't)
		(forward-char 1) 
		(let ((state
		       (parse-partial-sexp (save-excursion
					     (haskell-back-to-zero-indent)
					     (point))
					   (point))))
		  (if (not (or (nth 3 state) (nth 4 state)))
		      ;;Not in a comment or string
		      (if (>= (car state) 0)
			  ;;The parenthesis has a match
			  (progn
			    (goto-char (nth 2 state))
			    ;;Move to the beginning of the parentheses
			    ;; as this new line will determine
			    ;; further indentation
			    (if (zerop (car state))
				;;This paren closes all unbalanced parens
				;; so move to
				;; the eol of last line with an equality.
				(progn
				  (setq eol-limit (point))
				  (goto-char
				   (max (save-excursion
					  (haskell-back-to-symbol "=")
					  (point))
					(save-excursion
					  (haskell-back-to-keyword)
					  (point))))
				  (goto-char eol-limit))
			      ;;esle just go to the end of the line
			      (goto-char (haskell-eol)))
			    (setq paren-indent 't)
			    ;;Set 'paren-indent' to true to indicate we
			    ;; are indenting a list.
			    (setq eol-limit (point))
			    (beginning-of-line)	
			    (setq bol-limit (point))
			    ;;Reduce the scope of any later
			    ;; indentation to
			    ;; exclude the balanced parentheses
			    ;; by making this point
			    ;; be the eol-limit.  
			    (goto-char eol-limit)))))))
	  (flag)
	  ;;This cond expression is structured, to an 
	  ;; extent, such that the keywords with highest
	  ;; indentation precedence come first.  Order is important.
	  ;;In each condition, the point of match is noted so
	  ;; that we can see if this point is in a string.
	  (let ((indent-point (point)))
	    (cond ((re-search-backward "\\bof\\b" bol-limit 't)
		   ;; `of' is contained in previous line
		   (setq indent-point (point))
		   (if (looking-at "of[ \t]*$")
		     ;;`of' at end of line
		       (setq indent (+ (haskell-last-unbalanced-key-column
					"case" "of")
				       haskell-case-offset))
		     ;;Otherwise, `of' is in line
		     (forward-word 1)
		     (skip-chars-forward " \t")
		     (setq indent (current-column))
		     (setq indent (list indent))))
		  
		  ((re-search-backward
		    "\\bthen[ \t]*$" bol-limit 't)
		   ;;There is a `then' at the end of the line.
		   (setq indent-point (point))
		   (if haskell-align-else-with-then
		       ;;We want to align the `else' (to follow) with the `then'
		       (setq indent (+ (current-column)
				       haskell-if-offset))
		     (setq indent (+ (haskell-last-unbalanced-key-column 
				      "if" "then")	
				     haskell-if-offset))))
		  ;; This was here but don't know why (setq indent (list indent))))
		  
		  ((save-excursion
		     (and (re-search-backward "\\bif\\b" bol-limit 't)
			  (setq indent-point (point))
			  (not (re-search-forward "\\bthen\\b" eol-limit 't))))
		   ;;There is an `if' on the (previous) line and the line does
		   ;; not have a `then' on it.
		   (setq indent (+ (haskell-last-unbalanced-key-column 
				    "if" "then")
				   haskell-then-offset)))
		  
		  ((save-excursion
		     (and (re-search-backward "\\bif\\b" bol-limit 't)
			  (setq indent-point (point))
			  (not (re-search-forward "\\belse\\b" eol-limit 't))))
		   ;;There is an `if' on the (previous) line (the line may
		   ;; have a `then' on it) and does not have an else on it.
		   (if (re-search-backward "\\bthen\\b" bol-limit 't)
		       ;;There is a then on the line and it is followed by
		       ;; some code.
		       (progn
			 (forward-word 1)
			 (skip-chars-forward " \t")
			 (setq indent (current-column)))
		     (if haskell-align-else-with-then
			 ;;We want to align the `else' with the `then'
			 (setq indent (haskell-last-unbalanced-key-column 
				       "then" "else"))	
		       (setq indent (haskell-last-unbalanced-key-column 
				     "if" "else")))))
		  
		  ((re-search-backward "\\b\\(let\\|in\\)\\b" bol-limit 't)
		   ;; 'let' or 'in' is contained in the (previous) line
		   (setq indent-point (point))
		   (forward-word 1) ;skip past the word
		   (skip-chars-forward " \t{")
		   (if (looking-at "\\($\\|--\\)")
		       ;;looking-at eol or comment
		       (progn
			 (forward-word -1)
			 (setq indent (+ (current-column)
					 haskell-let-offset)))
		     (setq indent (current-column))))
		  
		  ((re-search-backward
		    "\\belse[ \t]*$" bol-limit 't)
		   ;;There is a `else' at end of line
		   (setq indent-point (point))
		   (save-excursion
		     (goto-char eol-limit)
		     (forward-word -1)
		     (setq indent (+ (current-column)
				     haskell-if-offset))))
		  
		  ((re-search-backward
		    "\\belse\\b" bol-limit 't)
		   ;;There is a `else' on the line with no if or then
		   (setq indent-point (point))
		   (save-excursion
		     (forward-word 1)
		     (skip-chars-forward " \t")
		     (setq indent (current-column))))
		  
		  ((save-excursion
		     (beginning-of-line)
		     (looking-at 
		      "^[ \t]*then\\b"))
		   ;;There is a 'then' at beginning of line
		   (setq indent-point (point))
		   (setq indent (current-indentation)))
		  
		  ((save-excursion
		     (beginning-of-line)
		     (looking-at "^[ \t]*else[ \t]*if\\b"))
		   (setq indent-point (point))
		   ;;There is an 'else if' at start of (previous) line
		   (save-excursion
		     (beginning-of-line)
		     (if haskell-nest-ifs
			 (save-excursion
			   (forward-word 1)
			   (skip-chars-forward " \t")
			   (setq indent (current-column)))
		       (skip-chars-forward " \t")
		       (setq indent (current-column)))))
		  
		  ((re-search-backward "\\bcase\\b" bol-limit 't)
		   ;;There is a 'case' on the previous line
		   ;; so copy this line's indentation and add on
		   ;; the offset unless there is not an of.
		   (setq indent-point (point))
		   (setq indent (+ (current-column) 
				   haskell-case-offset)))
		  
		  ((save-excursion
		     (beginning-of-line)
		     (looking-at "^\\(instance\\|class\\)\\b"))
		   ;;This (previous) line has an 'instance' or 'class' at start
		   ;; so just set indentation to be this line indentation
		   ;; plus the standard offset
		   (setq indent-point (point))
		   (setq indent (+ (current-indentation)
				   haskell-indent-offset)))
		  
		  ((re-search-backward "where\\b" bol-limit 't)
		   ;;There is a 'where' on the (previous) line
		   (setq indent-point (point))
		   (if (looking-at "where[ \t]*$")
		       ;;There is nothing after the 'where'
		       ;; so set indent to be this column
		       ;; (ie. the column of the 'w')
		       ;; plus the standard offset
		       (if (save-excursion
			     (skip-chars-backward " \t")
			     (bolp))
			   ;;The 'where' is the only thing on the line.
			   (setq indent (+ (current-column) 
					   haskell-where-offset))
			 ;;Otherwise, the 'where' is at the end
			 ;; of the line and there is code before it.
			 ;;Look before the 'where' for the symbol
			 ;; it scopes over.
			 (forward-word -1)
			 (goto-char (max (save-excursion
					   (haskell-back-to-symbol "=")
					   (point))
					 (save-excursion
					   (haskell-back-to-symbol "->")
					   (point))))
			 (setq indent (haskell-indent-where)))
		     
		     ;;Otherwise, go past the 'where'
		     ;; and goto the last non space character.
		     ;;Set this column to be the indentation.
		     (forward-word 1) 
		     (skip-chars-forward " \t") 
		     (setq indent (current-column))))	
		  
		  ((re-search-backward
		    "[ \ta-z0-9A-Z]=[ \t]*$" bol-limit 't)
		   ;;There is an equals is at the end of line
		   ;; so make the indentation be this line's indentation
		   ;; plus the standard offset
		   (setq indent-point (point))
		   (setq indent (+ (current-indentation)
				   haskell-indent-offset))) 
		  
		  ((re-search-backward
		    "[ \ta-z0-9A-Z]\\+\\+[ \t]*$" bol-limit 't)
		   ;;There is a concat operator at the end of line
		   ;; so make the indentation be this line's indentation
		   (setq indent-point (point))
		   (setq indent (current-indentation)))
		  
		  ((save-excursion
		     (beginning-of-line)
		     (looking-at
		      "^[ \t]*=[ \ta-z0-9A-Z]"))
		   ;;There is an equals is at the beginning of line
		   ;; so make the indentation be the previous line's
		   ;; indentation unless the previous line's
		   ;; indentation is zero.
		   (setq indent-point (point))
		   (save-excursion
		     (haskell-backward-to-noncomment)
		     (if (zerop (current-indentation))
			 (setq indent (+ (current-indentation)
					 haskell-indent-offset))
		       (setq indent (haskell-current-indentation)))))
		  
		  ((re-search-backward "|" bol-limit 't)
		   ;;There is  an `|' on this line.
		   (setq indent-point (point))
		   (if (save-excursion
			 (goto-char original-position)
			 (looking-at "^[ \t]*\\($\\|--\\||\\)"))
		       ;;The original line is empty or has a `|' at the 
		       ;; start.  So set indent to be first `|' on this line
		       (save-excursion
			 (goto-char bol-limit)
			 (re-search-forward "|" eol-limit 't)
			 (setq indent (1- (current-column))))
		     ;;Otherwise set indent to be this (previous) line's
		     (setq indent 0)))
		  
		  ((re-search-backward "->" bol-limit 't)
		   ;;There is a `->' in the line.
		   ;;This may be from a `case' or a
		   ;; type declaration.
		   (setq indent-point (point))
		   (save-excursion
		     (if (re-search-backward "::" bol-limit 't)
			 ;;There is a '::' on this line
			 (if (looking-at ".*->[ \t]*$")
			     ;;The '->' is at the end of line.
			     ;;Move past the '::' and any spaces
			     ;; and set indent to be this column.
			     (progn
			       (skip-chars-forward ": \t") 
			       (setq indent (current-column)))
			   ;;Otherwise, the '->' is not at end of line
			   ;; so copy the indentation
			   (setq indent (haskell-context-indent)))
		       
		       ;;Otherwise, there is not a
		       ;; `::' on this line so copy this
		       ;; (previous) indentation.
		       (setq indent (haskell-context-indent)))))
		  
		  ((re-search-backward "::" bol-limit 't)
		   ;;There is  an '::' on this line.
		   ;;We know that the line does not end with '->'.
		   (setq indent-point (point))
		   (if (looking-at "::[ \t]*$")
		       ;;The '::' is at the end of the line
		       ;; so set indent to be this line's
		       ;; indentation plus the offset.
		       (setq indent (+ (current-indentation) 
				       haskell-indent-offset))
		     ;;Otherwise the `::' is in the line
		     (setq indent (current-indentation))))
		  
		  ((re-search-backward
		    "\\b\\(import\\|class\\)\\b"
		    bol-limit 't)
		   ;;There is an `import' or `class' on the line.
		   ;;Copy this indentation.
		   (setq indent-point (point))
		   (setq indent (current-indentation)))
		  
		  ((or
		    (haskell-looking-at-eqp)
		    (save-excursion
		      (beginning-of-line)
		      (looking-at "^[ \t]*$")))
		   ;;There is an '=' on the line
		   ;; or it is blank
		   (setq indent-point (point))
		   (cond ((save-excursion
			    (beginning-of-line)
			    (looking-at "^[ \t]*data\\b"))
			  ;;`data' at start of line
			  ;; so expect a `|'
			  (haskell-back-to-symbol "=")
			  (setq indent (current-column)))
			 ((zerop (current-indentation))
			  ;;If the indentation is zero, we expect a `where'
			  (goto-char eol-limit)
			  (haskell-back-to-symbol "=")
			  (setq indent (haskell-indent-where)))
			 ((looking-at "^[ \t]*=[ \t\na-z0-9A-Z]")
			  ;;The equality is the first thing on the line
			  ;; so copy the last lines indentation
			  (save-excursion
			    (haskell-backward-to-noncomment)
			    (setq indent (current-indentation))))
			 (t
			  ;;Otherwise, copy the indentation
			  (setq indent (current-indentation)))))
		  
		  ((save-excursion
		     (beginning-of-line)
		     (and (zerop (current-indentation))
			  (not (looking-at "^[ \t]*$"))))
		   ;;The line is not blank and its indentation is zero
		   ;;It is a function definition.  We know that 
		   ;; there is not an equals on the line
		   (goto-char eol-limit)
		   ;;We expect a keyword
		   ;; so set indent to be this line's indentation
		   ;; plus the offset
		   (setq indent-point (point))
		   (setq indent (+ (current-indentation)
				   haskell-indent-offset)))
		  
		  ((bobp)
		   ;;At the beginning of buffer
		   (setq indent 0))
		  
		  (paren-indent
		   ;;We are indenting a list and none
		   ;; of the above indentations are applicable
		   ;; so copy the indentation of this line
		   (setq indent (current-indentation)))
		  
		  (t
		   (save-excursion
		     (setq indent (haskell-context-indent)))))

	    (if (nth 3 (parse-partial-sexp
			(save-excursion
			  (goto-char indent-point)
			  (haskell-back-to-zero-indent)
			  (point))
			(save-excursion
			  (goto-char indent-point))))
		;;The point we determined indentation at is in a
		;; string so go to this point and go back one line to
		;; find indentation.
		(setq indent (haskell-context-indent))))
	  
	  
	  ;;HOWEVER, we may have to override any indentation if we are in
	  ;; an unbalanced parenthesis (on the original line).
	  (flag)
	  (save-excursion
	    (goto-char original-position)
	    (let* ((eq-point (save-excursion
			       (haskell-back-to-symbol "=")
			       (point)))
		   (state (parse-partial-sexp
			   eq-point
			   (point))))
	      (if (> (car state) 0)
		  ;;There is an unbalanced parenthesis between
		  ;; the function and here.
		  (if (not (or (nth 3 state) (nth 4 state)))
		      ;;We are not in a string or comment
		      ;; so goto the parenthesis
		      (progn
			(goto-char (nth 1 state))
			(if (not (haskell-keyword-in-regionp
				  (point)
				  original-position))
			    ;;There is not a keyword after the open
			    ;; bracket so we override the indentation
			    (progn
			      (if (not (looking-at "{"))
				  ;;The parenthesis is not a `{'
				  (if (or (looking-at "\\[")
					  (save-excursion
					    (goto-char (haskell-eol))
					    (skip-chars-backward " \t")
					    (and
					     (char-equal (preceding-char) ?,)
					     (= (car state)
						(car (parse-partial-sexp
						      eq-point
						      (point)))))))
				      ;;The paren is a square one
				      ;; or it is a tuple.
				      ;;Don't ignore what is after it.
				      (setq indent (haskell-list-align (haskell-eol)))
				    ;;Otherwise, ignore what comes after it.
				    (setq indent (haskell-list-align (point))))))))))))
	  ))
      
      indent)))
  

;;; Inserts the close parenthesis and reindents the line.
;;; We want to reindent the line if the parenthesis is 
;;; the first character on the line.  The parenthesis
;;; recognised by this function are `]', `}'.

(defun electric-haskell-brace ()
  "Inserts the character `]' or `}' and reindents the current line."
  "Insert character and correct line's indentation."
  (interactive)
  (if (save-excursion
	(skip-chars-backward " \t")
	(bolp))
      ;;The parenthesis is at the beginning of the line.
      (progn
        (insert last-command-char)
	(haskell-indent-line))
    ;;Otherwise it is not at the beginning of line.
    (insert last-command-char))
  ;; Match its beginning.
  (haskell-blink-open))




;;; This function returns the indentation for the next line given
;;; that it is contained in a bracket or we are extending a functions
;;; parameters over a line.  For the case of being in an unbalanced
;;; parenthesis list, the point lies on the unbalanced parenthesis.
;;; The parameter eol-limit is used to delimit the end of the line.

(defun haskell-list-align (eol-limit)
  "Returns the indentation for the next line given that
the point lies on an unbalanced open parenthesis."
  (save-excursion
    (let ((indent (1+ (current-column))))
      ;;Set indent to be the next char (at least).

      (cond ((not 
	      (looking-at ".[ \t]*\\($\\|--\\)"))
	     ;;There is something after the parenthesis
	     ;;ie. the line is not empty and ignore comments
	     (cond ((save-excursion
		      (goto-char eol-limit)
		      (skip-chars-backward " \t")
		      (and (char-equal (preceding-char) ?,)
			   (save-excursion
			     (beginning-of-line)
			     (not (search-forward "|" eol-limit 't)))))
		    ;;This is a normal list since a `,' at end
		    ;; and there is no a `|' on the line.
		    (forward-char 1)
		    (skip-chars-forward " \t")
		    (setq indent (current-column)))

		   ((looking-at "\\[")
		    ;;It is a list comp we are looking at
		    ;;Goto the bar.
		    (forward-char 1)	
		    (search-forward "|" eol-limit 't)
		    (skip-chars-forward " \t")
		    (setq indent (current-column)))
	  
		   ((looking-at ".[ \t]*(")
		    ;;We are looking at an open parenthesis
		    ;; after this character.
		    ;;It must be balanced so 
		    ;; move to the start of this paren
		    ;; and set indent to be here
		    (forward-char 1) 
		    (skip-chars-forward " \t")
		    (setq indent (current-column)))
		   
		   (t
		    (forward-word 1)
		    ;;We are not looking at another open
		    ;; parenthesis, so move forward past the
		    ;; (assumed) function name.
		    (if (or
			 haskell-std-list-indent
			 (looking-at"[ \t]*\\($\\|--\\)"))
			;;There is nothing after the name
			;; or haskell-std-list-offset is set
			;; so set indent to be its original
			;; value plus the offset minus 1
			;; since we added one on earlier.
			(setq indent
			      (+ indent
				 (1- haskell-list-offset)))
		      
		      ;;Otherwise there is something after the
		      ;; name, so skip to the first non space
		      ;; character.
		      (skip-chars-forward " \t")
		      (setq indent (current-column)))))))


      indent)))



(defun haskell-insert-round-paren ()
  "Inserts a `(' and blinks to its matching parenthesis."
  (interactive)
  (insert last-command-char)
  (haskell-blink-open))



;;; This function is called when a close parenthesis 
;;; `)', `]', or `}' is typed.
;;; Blinks the cursor on the corresponding open parnethesis.
;;; The point lies just after the close parenthesis.

(defun haskell-blink-open ()
  "Blinks the cursor to the matching open parenthesis.
The point lies just after a parenthesis."
  (let ((state (parse-partial-sexp (point)
				   (save-excursion
				     (haskell-back-to-zero-indent)
				     (point)))))
    (if (and
	 (>= (car state) 0)
	 (not (or (nth 3 state) (nth 4 state))))
	;;The parenthesis just inserted has a match
	;; and is not in a string or a comment
	;; so blink on its match
	(save-excursion
	  (goto-char (nth 2 state))
	  (sit-for 1)))))



;;; This function indents the line expecting the line to be a 
;;; continued function application.

;;;   foo a = bar a
;;;               b     {haskell-further-indent applied to this line
;;;                      indents the line as shown}

;;; The line would look like this if only tab had been applied:
;;;   foo a = bar a
;;;         b

(defun haskell-further-indent ()
  "Indents the line more than the ordinary indentation in order to 
extend function arguments over multiple lines."
  (interactive)
  (let (indent
	(new-point (max (save-excursion
			  (haskell-back-to-symbol "=")
			  (point))
			(save-excursion
			  (haskell-back-to-keyword)
			  (point)))))
    (save-excursion
      ;;This may be a continuation of a function
      ;; application so go back to the last '='
      ;; and set indent as designated by the style chosen
      (goto-char new-point)
      (skip-chars-forward "= \t")
      (setq indent (haskell-list-align (haskell-eol))))
    ;;The argument to haskell-list-align is not important here.
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent))
    (if (< (current-column) indent)
	(move-to-column indent))))


;;; This function indents the current line to the first previous
;;; indentation value which is less than the current indentation.

(defun haskell-lesser-indent ()
  "Indents the current line to the first previous indentation
value which is less than the current indentation."
  (interactive)
  (let ((original-indent
	 (current-indentation))
	(indent (haskell-context-indent))
	(done nil))
    (save-excursion
      (while (not done)
	(while (and (not (bobp))
		    (not (zerop (current-indentation)))
		    (>= indent original-indent))
	  (haskell-backward-to-noncomment)
	  (setq indent (current-indentation)))
	;;bobp or indent < original-indent
	(if (>=  indent original-indent)
	    ;;indent is still greater than or equal to original indent
	    (progn 
	      (setq indent 0)
	      (setq done t))
	  ;;Otherwise, indent is less than orignal indent.
	  (forward-line 1)
	  (setq indent (haskell-context-indent))
	  (if (< indent original-indent)
	      ;;The new indent is an improvement
	      (setq done t)
	    ;;Otherwise, indent is still >= original
	    ;; so go back to the line and keep typing.
	    (forward-line -1)))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent))
    (if (< (current-column) indent)
	(move-to-column indent))))

      

;;; Here are the functions which change the local variables
;;; to facilitate tailorability.

(defun default-mode ()
  "Calls the function haskell-mode."
  (interactive)
  (haskell-mode)
  (message haskell-indent-style))

(defun wadler-mode ()
  "Sets defaults according to Dr. Philip L. Wadler's preferences.
   - Aligns `where' clauses with the corresponding equality.
   - Aligns `else' keyword with the corresponding `then'
   - haskell-list-offset 2
   - haskell-indent-offset 8
   - haskell-if-indent   2
   - haskell-comment-column 0
   - haskell-case-offset 2
   - haskell-let-offset  5."
  ;;Preferences:
  ;;'haskell-align-where-with-eq  non-nil
  ;;'haskell-list-offset 2
  (interactive)
  (haskell-mode)
  (or haskell-align-where-with-eq
      (progn
	(setq haskell-align-where-with-eq t)
	(setq haskell-std-indent-where nil)))
  (setq haskell-align-else-with-then t)
  (setq haskell-list-offset 2)
  (setq haskell-indent-offset 8)
  (setq haskell-if-offset 2)
  (setq haskell-case-offset 2)
  (setq haskell-let-offset 5)
  (setq haskell-comment-column 0)
  (setq haskell-indent-style "Wadler")
  (message haskell-indent-style))
	      

(defun report-mode ()
  "Sets defaults according to the style of the Haskell Report.
   - Aligns `where' clauses after the corresponding equality.
   - Aligns `else' with `then'.
   - haskell-then-offset   = 3
   - haskell-where-offset  = 0.
   - haskell-case-offset   = 5."
  ;;Preferences:
  ;; haskell-align-where-after-eq  non-nil
  ;; haskell-then-offset  3
  ;; haskell-where-offset 0
  ;; haskell-case-offset  5
  (interactive)
  (haskell-mode)
  (haskell-align-where-after-eq)
  (or haskell-align-else-with-then
      (haskell-align-else-with-then))
  (setq haskell-then-offset 3)
  (setq haskell-where-offset 0)
  (setq haskell-case-offset 5)
  (setq haskell-indent-style "Report")
  (message haskell-indent-style))
	      

(defun haskell-align-where-with-eq ()
  "Sets indentation so that a 'where' clause lines up underneath
its corresponding equals sign."
  (interactive)
  (or haskell-align-where-with-eq
      (progn
	(setq haskell-align-where-after-eq nil)
	(setq haskell-std-indent-where nil)
	(setq haskell-align-where-with-eq t)
	haskell-align-where-with-eq)))



(defun haskell-align-where-after-eq ()
  "Sets indentation so that a 'where' clause lines up underneath
the first nonspace character after its corresponding equals sign."
  (interactive)
  (or haskell-align-where-after-eq
      (progn
	(setq haskell-align-where-with-eq nil)
	(setq haskell-std-indent-where nil)
	(setq haskell-align-where-after-eq t)
	haskell-align-where-after-eq)))


(defun haskell-std-indent-where ()
  "Sets indentation so that a `where' clause lines up underneath
its corresponding equals sign."
  (interactive)
  (or haskell-std-indent-where
      (progn
	(setq haskell-align-where-after-eq nil)
	(setq haskell-align-where-with-eq nil)
	(setq haskell-std-indent-where t)
	haskell-std-indent-where)))


(defun haskell-align-else-with-then ()
  "Sets indentation so that an `else' lines up underneath
it's corresponding `then'."
  (interactive)
  (setq haskell-align-else-with-then
	(not haskell-align-else-with-then))
  (setq haskell-nest-ifs nil))

(defun haskell-nest-ifs ()
  "Sets indentation so that an `if' is lined up
under an `if' in an `else ."
  (interactive)
  (setq haskell-nest-ifs
	(not haskell-nest-ifs))
  (setq haskell-align-else-with-then nil))


(defun haskell-always-fixup-comment-space ()
  "Non-nil means always position one space after a line comment `--',
when reindenting or inserting a comment,
whether or not one space exists."
  (setq haskell-always-fixup-comment-space
	(not haskell-always-fixup-comment-space))
  haskell-always-fixup-comment-space)

(defun haskell-indent-style ()
  "Echos the chosen indentation style in the mini-buffer."
  (interactive)
  (message haskell-indent-style))

(defun set-haskell-let-offset (offset)
  "Changes the value of haskell-let-offset, the variable which
determines extra indentation after a `let' and  `in'."
  (interactive "nSet haskell-let-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-let-offset offset)))

(defun set-haskell-if-offset (offset)
  "Changes the value of haskell-let-offset, the variable which
determines extra indentation after an `if', `then' and `else'."
  (interactive "nSet haskell-if-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-if-offset offset)))

(defun set-haskell-case-offset (offset)
  "Changes the value of haskell-case-offset, the variable which
determines extra indentation after a `case' and `of'."
  (interactive "nSet haskell-case-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-case-offset offset)))


(defun set-haskell-where-offset (offset)
  "Changes the value of haskell-where-offset, the variable which
determines extra indentation after a line of haskell code."
  (interactive "nSet haskell-where-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-where-offset offset)))


(defun set-haskell-indent-offset (offset)
  "Changes the value of haskell-indent-offset, the variable which
determines extra indentation after a line of haskell code."
  (interactive "nSet haskell-indent-offset to: ")
  (if (and (>= offset 1) (<= offset 10))
      (setq haskell-indent-offset offset)))


(defun set-haskell-list-offset (offset)
  "Changes the value of haskell-list-offset, the variable which
determines extra indentation after a line of haskell code for a list."
  (interactive "nSet haskell-list-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-list-offset offset)))


(defun set-haskell-comp-offset (offset)
  "Changes the value of haskell-comp-offset, the variable which
determines extra indentation after a list comprehension."
  (interactive "nSet haskell-comp-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-comp-offset offset)))


(defun set-haskell-then-offset (offset)
  "Changes the value of haskell-then-offset, the variable which
determines extra indentation for a `then' keyword after an `if'."
  (interactive "nSet haskell-then-offset to: ")
  (if (and (>= offset 0) (<= offset 10))
      (setq haskell-then-offset offset)))


(defun set-haskell-comment-column (column)
  "Changes the value of haskell-comment-column, the variable which
determines where to postition a line comment `--'."
  (interactive "nSet haskell-comment-column to: ")
  (if (and (>= column 0) (<= column 100))
      (setq haskell-comment-column column)))
	 
(defun set-haskell-concat-column (column)
  "Changes the value of haskell-concat-column, the variable which
determines where to postition a concatenation operator `++'."
  (interactive "nSet haskell-concat-column to: ")
  (if (and (>= column 0) (<= column 100))
      (setq haskell-concat-column column)))
	 
(defun set-haskell-where-threshold (column)
  "Changes the value of haskell-where-threshold, the variable which
determines when to override positioning a `where' under or after
its corresponding equality."
  (interactive "nSet haskell-where-threshold to: ")
  (if (and (>= column 0) (<= column 100))
      (setq haskell-where-threshold column)))
	 
(defun flag ())