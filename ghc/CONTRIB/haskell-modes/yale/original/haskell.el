;;; ==================================================================
;;; File: 		haskell.el     				   ;;;
;;;                                                                ;;;
;;;			Author: 	A. Satish Pai		   ;;;
;;;                                     Maria M. Gutierrez         ;;;
;;;                                     Dan Rabin (Jul-1991)       ;;;
;;; ==================================================================

;;; Description: Haskell mode for GNU Emacs.

;;; Related files:  comint.el

;;; Contents:

;;;  Update Log

;;;  Known bugs / problems
;;;  - the haskell editing mode (indentation, etc) is still missing.
;;;  - the handling for errors from haskell needs to be rethought.
;;;  - general cleanup of code.


;;;  Errors generated

;;; ==================================================================
;;; Haskell mode for editing files, and an Inferior Haskell mode to
;;; run a Haskell process. This file contains stuff snarfed and 
;;; modified from tea.el, scheme.el, etc. This file may be freely
;;; modified; however, if you have any bug-corrections or useful
;;; improvements, I'd appreciate it if you sent me the mods so that
;;; I can merge them into the version I maintain.
;;;
;;; The inferior Haskell mode requires comint.el. 
;;; 
;;; You might want to add this to your .emacs to go automagically
;;; into Haskell mode while finding .hs files.
;;; 
;;;   (setq auto-mode-alist 
;;;         (cons '("\\.hs$" . haskell-mode)
;;;                auto-mode-alist)_)
;;;
;;; To use this file, set up your .emacs to autoload this file for 
;;; haskell-mode. For example:
;;; 
;;;    (autoload 'haskell-mode "$HASKELL/emacs-tools/haskell.elc" 
;;;       "Load Haskell mode" t)
;;;
;;;    (autoload 'run-mode "$HASKELL/emacs-tools/haskell.elc" 
;;;       "Load Haskell mode" t)
;;;
;;; [Note: The path name given above is Yale specific!! Modify as
;;; required.]
;;; ================================================================

;;; Announce your existence to the world at large.

(provide 'haskell)


;;; Load these other files.

(require 'comint)        ; Olin Shivers' comint mode is the substratum




;;; ================================================================
;;; Declare a bunch of variables.
;;; ================================================================


;;; User settable (via M-x set-variable and M-x edit-options)

(defvar haskell-program-name (getenv "HASKELLPROG")
  "*Program invoked by the haskell command.")

(defvar haskell-auto-create-process t
  "*If not nil, create a Haskell process automatically when required to evaluate or compile Haskell code.")

(defvar haskell-auto-switch-input t
  "*If not nil, jump to *haskell* buffer automatically on input request.")

(defvar haskell-ask-before-saving t
  "*If not nil, ask before saving random haskell-mode buffers.")

(defvar haskell-initial-printers '("interactive")
  "*Printers to set when starting a new Haskell process.")


;;; Pad/buffer Initialization variables

(defvar *haskell-buffer* "*haskell*"
  "Name of the haskell process buffer")

(defvar haskell-main-pad "\*Main-pad\*"
  "Scratch pad associated with module Main")

(defvar haskell-main-module "Main")


(defvar *last-loaded* nil)
(defvar *last-module* haskell-main-module)
(defvar *last-pad* haskell-main-pad)


;;; These are used for haskell-tutorial mode.

(defvar *ht-source-file* "$HASKELL/progs/tutorial/tutorial.lhs")
(defvar *ht-temp-buffer* nil)
(defvar *ht-file-buffer* "Haskell-Tutorial-Master")



;;; ================================================================
;;; Haskell editing mode stuff
;;; ================================================================

;;; Leave this place alone...
;;; The definitions below have been pared down to the bare
;;; minimum; they will be restored later.
;;;
;;; -Satish 2/5.

;;; Keymap for Haskell mode
(defvar haskell-mode-map (make-sparse-keymap)
  "Keymap used for haskell-mode")

(defun haskell-establish-key-bindings (keymap)
  (define-key keymap "\C-ce"    'haskell-eval)
  (define-key keymap "\C-cr"    'haskell-run)
  (define-key keymap "\C-ct"    'haskell-report-type)
  (define-key keymap "\C-cm"    'haskell-run-main)
  (define-key keymap "\C-c\C-r" 'haskell-run-file)
  (define-key keymap "\C-cp"    'haskell-get-pad)
  (define-key keymap "\C-c\C-o" 'haskell-optimizers)
  (define-key keymap "\C-c\C-p" 'haskell-printers)
  (define-key keymap "\C-cc"    'haskell-compile)
  (define-key keymap "\C-cl"    'haskell-load)
  (define-key keymap "\C-ch"    'haskell-switch)
  (define-key keymap "\C-c\C-k" 'haskell-kill)
  (define-key keymap "\C-c:"    'haskell-command)
  (define-key keymap "\C-cq"    'haskell-exit)
  (define-key keymap "\C-ci"    'haskell-interrupt)
  (define-key keymap "\C-cu"    'haskell-edit-unit))


(haskell-establish-key-bindings haskell-mode-map)


(defvar haskell-mode-syntax-table nil
  "Syntax table used for haskell-mode")

(if haskell-mode-syntax-table
    nil
    (setq haskell-mode-syntax-table (standard-syntax-table)))

;;; Command for invoking the Haskell mode
(defun haskell-mode nil
  "Major mode for editing Haskell code to run in Emacs
The following commands are available:
\\{haskell-mode-map}

A Haskell process can be fired up with \"M-x haskell\". 

Customization: Entry to this mode runs the hooks that are the value of variable 
haskell-mode-hook.

Windows:

There are 3 types of windows associated with Haskell mode.  They are:
   *haskell*:  which is the process window.
   Pad:        which are buffers available for each module.  It is here
               where you want to test things before preserving them in a
               file.  Pads are always associated with a module.
               When issuing a command:
                 The pad and its associated module are sent to the Haskell
                 process prior to the execution of the command.
   .hs:        These are the files where Haskell programs live.  They
               have .hs as extension.
               When issuing a command:
                 The file is sent to the Haskell process prior to the
                 execution of the command.

Commands:

Each command behaves differently according to the type of the window in which 
the cursor is positioned when the command is issued .

haskell-eval:   \\[haskell-eval]
  Always promts user for a Haskell expression to be evaluated.  If in a
  .hs file buffer, then the cursor tells which module is the current 
  module and the pad for that module (if any) gets loaded as well.

haskell-run:    \\[haskell-run]
  Always queries for a variable of type Dialogue to be evaluated.

haskell-run-main:    \\[haskell-run-main]
  Run Dialogue named main in the current module.

haskell-report-type:   \\[haskell-report-type]
  Like haskell-eval, but prints the type of the expression without
  evaluating it.

haskell-mode:   \\[haskell-mode]
  Puts the current buffer in haskell mode.

haskell-compile:   \\[haskell-compile]
  Compiles file in current buffer.

haskell-load:   \\[haskell-load]
  Loads file in current buffer.

haskell-run-file:   \\[haskell-run-file]
  Runs file in the current buffer.

haskell-pad:   \\[haskell-pad]
  Creates a scratch pad for the current module.

haskell-optimizers:  \\[haskell-optimizers]
  Shows the list of available optimizers.  Commands for turning them on/off.

haskell-printers:  \\[haskell-printers]
  Shows the list of available printers.  Commands for turning them on/off.

haskell-command:   \\[haskell-command]
  Prompts for a command to be sent to the command interface.  You don't
  need to put the : before the command.

haskell-quit:   \\[haskell-quit]
  Terminates the haskell process.

haskell-switch:   \\[haskell-switch]
  Switches to the inferior Haskell buffer (*haskell*) and positions the
  cursor at the end of the buffer.

haskell-kill:  \\[haskell-kill]
  Kill the current contents of the *haskell* buffer.
  
haskell-interrupt:   \\[haskell-interrupt]
  Interrupts haskell process and resets it.

haskell-edit-unit:   \\[haskell-edit-unit]
  Edit the .hu file for the unit containing this file.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map haskell-mode-map)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  ;(setq local-abbrev-table haskell-mode-abbrev-table)
  (set-syntax-table haskell-mode-syntax-table)
  ;(setq tab-stop-list haskell-tab-stop-list) ;; save old list??
  (run-hooks 'haskell-mode-hook))
 


;;;================================================================
;;; Inferior Haskell stuff
;;;================================================================


(defvar inferior-haskell-mode-map (full-copy-sparse-keymap comint-mode-map))

(haskell-establish-key-bindings inferior-haskell-mode-map)
(define-key inferior-haskell-mode-map "\C-m"     'haskell-send-input)

(defvar haskell-source-modes '(haskell-mode)
  "*Used to determine if a buffer contains Haskell source code.
If it's loaded into a buffer that is in one of these major modes, 
it's considered a Haskell source file.")

(defvar haskell-prompt-pattern "^[A-Z]\\([A-Z]\\|[a-z]\\|[0-9]\\)*>\\s-*"
  "Regular expression capturing the Haskell system prompt.")

(defvar haskell-prompt-ring ()
  "Keeps track of input to haskell process from the minibuffer")

(defun inferior-haskell-mode-variables ()
  nil)  


;;; INFERIOR-HASKELL-MODE (adapted from comint.el)

(defun inferior-haskell-mode ()
  "Major mode for interacting with an inferior Haskell process.

The following commands are available:
\\{inferior-haskell-mode-map}

A Haskell process can be fired up with \"M-x haskell\". 

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-haskell-mode-hook (in that order).

You can send text to the inferior Haskell process from other buffers containing
Haskell source.  


Windows:

There are 3 types of windows in the inferior-haskell-mode.  They are:
   *haskell*:  which is the process window.
   Pad:        which are buffers available for each module.  It is here
               where you want to test things before preserving them in a
               file.  Pads are always associated with a module.
               When issuing a command:
                 The pad and its associated module are sent to the Haskell
                 process prior to the execution of the command.
   .hs:        These are the files where Haskell programs live.  They
               have .hs as extension.
               When issuing a command:
                 The file is sent to the Haskell process prior to the
                 execution of the command.

Commands:

Each command behaves differently according to the type of the window in which 
the cursor is positioned when the command is issued.

haskell-eval:   \\[haskell-eval]
  Always promts user for a Haskell expression to be evaluated.  If in a
  .hs file, then the cursor tells which module is the current module and
  the pad for that module (if any) gets loaded as well.

haskell-run:    \\[haskell-run]
  Always queries for a variable of type Dialogue to be evaluated.

haskell-run-main:    \\[haskell-run-main]
  Run Dialogue named main.

haskell-report-type:   \\[haskell-report-type]
  Like haskell-eval, but prints the type of the expression without
  evaluating it.

haskell-mode:   \\[haskell-mode]
  Puts the current buffer in haskell mode.

haskell-compile:   \\[haskell-compile]
  Compiles file in current buffer.

haskell-load:   \\[haskell-load]
  Loads file in current buffer.

haskell-run-file:   \\[haskell-run-file]
  Runs file in the current buffer.

haskell-pad:   \\[haskell-pad]
  Creates a scratch pad for the current module.

haskell-optimizers:  \\[haskell-optimizers]
  Shows the list of available optimizers.  Commands for turning them on/off.

haskell-printers:  \\[haskell-printers]
  Shows the list of available printers.  Commands for turning them on/off.

haskell-command:   \\[haskell-command]
  Prompts for a command to be sent to the command interface.  You don't
  need to put the : before the command.

haskell-quit:   \\[haskell-quit]
  Terminates the haskell process.

haskell-switch:   \\[haskell-switch]
  Switches to the inferior Haskell buffer (*haskell*) and positions the
  cursor at the end of the buffer.

haskell-kill:  \\[haskell-kill]
  Kill the current contents of the *haskell* buffer.
  
haskell-interrupt:   \\[haskell-interrupt]
  Interrupts haskell process and resets it.

haskell-edit-unit:   \\[haskell-edit-unit]
  Edit the .hu file for the unit containing this file.

The usual comint functions are also available. In particular, the 
following are all available:

comint-bol: Beginning of line, but skip prompt. Bound to C-a by default.
comint-delchar-or-maybe-eof: Delete char, unless at end of buffer, in 
            which case send EOF to process. Bound to C-d by default.

Note however, that the default keymap bindings provided shadow some of
the default comint mode bindings, so that you may want to bind them 
to your choice of keys. 

Comint mode's dynamic completion of filenames in the buffer is available.
(Q.v. comint-dynamic-complete, comint-dynamic-list-completions.)

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."

  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp haskell-prompt-pattern)
  ;; Customise in inferior-haskell-mode-hook
  (inferior-haskell-mode-variables) 
  (setq major-mode 'inferior-haskell-mode)
  (setq mode-name "Inferior Haskell")
  (setq mode-line-process '(": %s : busy"))
  (use-local-map inferior-haskell-mode-map)
  (setq comint-input-filter 'haskell-input-filter)
  (setq comint-input-sentinel 'ignore)
  (setq comint-get-old-input 'haskell-get-old-input)
  (run-hooks 'inferior-haskell-mode-hook)
    ;Do this after the hook so the user can mung INPUT-RING-SIZE w/his hook.
    ;The test is so we don't lose history if we run comint-mode twice in
    ;a buffer.
  (setq haskell-prompt-ring (make-ring input-ring-size)))


(defun haskell-input-filter (str)
  "Don't save whitespace."
  (not (string-match "\\s *" str)))



;;; ==================================================================
;;; Random utilities
;;; ==================================================================


;;; This keeps track of the status of the haskell process.
;;; Values are:
;;; busy -- The process is busy.
;;; ready -- The process is ready for a command.
;;; input -- The process is waiting for input.
;;; debug -- The process is in the debugger.

(defvar *haskell-status* 'busy
  "Status of the haskell process")

(defun set-haskell-status (value)
  (setq *haskell-status* value)
  (haskell-update-mode-line))

(defun get-haskell-status ()
  *haskell-status*)

(defun haskell-update-mode-line ()
  (save-excursion
    (set-buffer *haskell-buffer*)
    (cond ((eq *haskell-status* 'ready)
	   (setq mode-line-process '(": %s: ready")))
	  ((eq *haskell-status* 'input)
	   (setq mode-line-process '(": %s: input")))
	  ((eq *haskell-status* 'busy)
	   (setq mode-line-process '(": %s: busy")))
	  ((eq *haskell-status* 'debug)
	   (setq mode-line-process '(": %s: debug")))
	  (t
	   (haskell-mode-error "Confused about status of haskell process!")))
    ;; Yes, this is the officially sanctioned technique for forcing
    ;; a redisplay of the mode line.
    (set-buffer-modified-p (buffer-modified-p))))


(defun haskell-send-to-process (string)
  (process-send-string "haskell" string)
  (process-send-string "haskell" "\n"))



;;; ==================================================================
;;; Handle input in haskell process buffer; history commands.
;;; ==================================================================

(defun haskell-get-old-input ()
  "Get old input text from Haskell process buffer."
  (save-excursion
    (if (re-search-forward haskell-prompt-pattern (point-max) 'move)
	(goto-char (match-beginning 0)))
    (cond ((re-search-backward haskell-prompt-pattern (point-min) t)
	   (comint-skip-prompt)
	   (let ((temp  (point)))
	     (end-of-line)
	     (buffer-substring temp (point)))))))


(defun haskell-send-input ()
  "Send input to Haskell while in the process buffer"
  (interactive)
  (if (eq (get-haskell-status) 'debug)
      (comint-send-input)
      (haskell-send-input-aux)))

(defun haskell-send-input-aux ()
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
	(haskell-mode-error "Current buffer has no process!")
	(let* ((pmark (process-mark proc))
	       (pmark-val (marker-position pmark))
	       (input (if (>= (point) pmark-val)
			  (buffer-substring pmark (point))
			  (let ((copy (funcall comint-get-old-input)))
			    (goto-char pmark)
			    (insert copy)
			    copy))))
	  (insert ?\n)
	  (if (funcall comint-input-filter input)
	      (ring-insert input-ring input))
	  (funcall comint-input-sentinel input)
	  (set-marker (process-mark proc) (point))
	  (set-marker comint-last-input-end (point))
	  (haskell-send-to-process input)))))



;;; ==================================================================
;;; Minibuffer input stuff
;;; ==================================================================

;;; Haskell input history retrieval commands   (taken from comint.el)
;;; M-p -- previous input    M-n -- next input

(defvar haskell-minibuffer-local-map nil
  "Local map for minibuffer when in Haskell")

(if haskell-minibuffer-local-map
    nil
    (progn
      (setq haskell-minibuffer-local-map
	    (full-copy-sparse-keymap minibuffer-local-map))
      ;; Haskell commands
      (define-key haskell-minibuffer-local-map "\ep"   'haskell-previous-input)
      (define-key haskell-minibuffer-local-map "\en"   'haskell-next-input)
      ))

(defun haskell-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (let ((len (ring-length haskell-prompt-ring)))
    (cond ((<= len 0)
	   (message "Empty input ring.")
	   (ding))
	  (t
	   (cond ((eq last-command 'haskell-previous-input)
		  (delete-region (mark) (point))
		  (set-mark (point)))
		 (t                          
		  (setq input-ring-index
			(if (> arg 0) -1
			    (if (< arg 0) 1 0)))
		  (push-mark (point))))
	   (setq input-ring-index (comint-mod (+ input-ring-index arg) len))
	   (insert (ring-ref haskell-prompt-ring input-ring-index))
	   (setq this-command 'haskell-previous-input))
	  )))
	 
(defun haskell-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (haskell-previous-input (- arg)))

(defvar haskell-last-input-match ""
  "Last string searched for by Haskell input history search, for defaulting.
Buffer local variable.") 

(defun haskell-previous-input-matching (str)
  "Searches backwards through input history for substring match"
  (interactive (let ((s (read-from-minibuffer 
			 (format "Command substring (default %s): "
				 haskell-last-input-match))))
		 (list (if (string= s "") haskell-last-input-match s))))
  (setq haskell-last-input-match str) ; update default
  (let ((str (regexp-quote str))
        (len (ring-length haskell-prompt-ring))
	(n 0))
    (while (and (<= n len)
		(not (string-match str (ring-ref haskell-prompt-ring n))))
      (setq n (+ n 1)))
    (cond ((<= n len) (haskell-previous-input (+ n 1)))
	  (t (haskell-mode-error "Not found.")))))


;;; Actually read an expression from the minibuffer using the new keymap.

(defun haskell-get-expression (prompt)
  (let ((exp  (read-from-minibuffer prompt nil haskell-minibuffer-local-map)))
    (ring-insert haskell-prompt-ring exp)
    exp))



;;; ==================================================================
;;; Handle output from Haskell process
;;; ==================================================================

;;; The haskell process produces output with embedded control codes.
;;; These control codes are used to keep track of what kind of input
;;; the haskell process is expecting.  Ordinary output is just displayed.
;;;
;;; This is kind of complicated because control sequences can be broken
;;; across multiple batches of text received from the haskell process.
;;; If the string ends in the middle of a control sequence, save it up
;;; for the next call.

(defvar *haskell-saved-output* nil)

;;; On the Next, there is some kind of race condition that causes stuff
;;; sent to the Haskell subprocess before it has really started to be lost.
;;; The point of this variable is to force the Emacs side to wait until
;;; Haskell has started and printed out its banner before sending it
;;; anything.  See start-haskell below.

(defvar *haskell-process-alive* nil)

(defun haskell-output-filter (process str)
  "Filter for output from Yale Haskell command interface"
  ;; *** debug
  ;;(let ((buffer  (get-buffer-create "haskell-output")))
  ;;  (save-excursion
  ;;    (set-buffer buffer)
  ;;    (insert str)))
  (setq *haskell-process-alive* t)
  (let ((next    0)
	(start   0)
	(data    (match-data)))
    (unwind-protect
	(progn
	  ;; If there was saved output from last time, glue it in front of the
	  ;; newly received input.
	  (if *haskell-saved-output*
	      (progn
		(setq str (concat *haskell-saved-output* str))
		(setq *haskell-saved-output* nil)))
	  ;; Loop, looking for complete command sequences.
	  ;; Set next to point to the first one.
	  ;; start points to first character to be processed.
	  (while (setq next
		       (string-match *haskell-message-match-regexp*
				     str start))
	    ;; Display any intervening ordinary text.
	    (if (not (eq next start))
		(haskell-display-output (substring str start next)))
	    ;; Now dispatch on the particular command sequence found.
	    ;; Handler functions are called with the string and start index
	    ;; as arguments, and should return the index of the "next"
	    ;; character.
	    (let ((end  (match-end 0)))
	      (haskell-handle-message str next)
	      (setq start end)))
	  ;; Look to see whether the string ends with an incomplete 
	  ;; command sequence.
	  ;; If so, save the tail of the string for next time.
	  (if (and (setq next
		     (string-match *haskell-message-prefix-regexp* str start))
		   (eq (match-end 0) (length str)))
              (setq *haskell-saved-output* (substring str next))
	      (setq next (length str)))
	  ;; Display any leftover ordinary text.
	  (if (not (eq next start))
	      (haskell-display-output (substring str start next))))
      (store-match-data data))))

(defvar *haskell-message-match-regexp*
  "EMACS:.*\n")

(defvar *haskell-message-prefix-regexp*
  "E\\(M\\(A\\(C\\(S\\(:.*\\)?\\)?\\)?\\)?\\)?")

(defvar *haskell-message-dispatch*
  '(("EMACS:debug\n"         . haskell-got-debug)
    ("EMACS:busy\n"          . haskell-got-busy)
    ("EMACS:input\n"         . haskell-got-input)
    ("EMACS:ready\n"         . haskell-got-ready)
    ("EMACS:printers .*\n"   . haskell-got-printers)
    ("EMACS:optimizers .*\n" . haskell-got-optimizers)
    ("EMACS:message .*\n"    . haskell-got-message)
    ("EMACS:error\n"         . haskell-got-error)
    ))

(defun haskell-handle-message (str idx)
  (let ((list  *haskell-message-dispatch*)
	(fn    nil))
    (while (and list (null fn))
      (if (eq (string-match (car (car list)) str idx) idx)
	  (setq fn (cdr (car list)))
	  (setq list (cdr list))))
    (if (null fn)
	(haskell-mode-error "Garbled message from Haskell!")
	(let ((end  (match-end 0)))
	  (funcall fn str idx end)
	  end))))


(defun haskell-message-data (string start end)
  (let ((real-start  (+ (string-match " " string start) 1))
	(real-end    (- end 1)))
    (substring string real-start real-end)))

(defun haskell-got-debug (string start end)
  (beep)
  (message "In the debugger!")
  (set-haskell-status 'debug))

(defun haskell-got-busy (string start end)
  (set-haskell-status 'busy))

(defun haskell-got-input (string start end)
  (if haskell-auto-switch-input
      (progn
	(haskell-switch)
	(beep)))
  (set-haskell-status 'input)
  (message "Waiting for input..."))

(defun haskell-got-ready (string start end)
  (set-haskell-status 'ready))

(defun haskell-got-printers (string start end)
  (haskell-printers-update (haskell-message-data string start end)))

(defun haskell-got-optimizers (string start end)
  (haskell-optimizers-update (haskell-message-data string start end)))

(defun haskell-got-message (string start end)
  (message "%s" (haskell-message-data string start end)))

(defun haskell-got-error (string start end)
  (beep)
  (message "Haskell error."))


;;; Displays output at end of given buffer.
;;; This function only ensures that the output is visible, without 
;;; selecting the buffer in which it is displayed.
;;; Note that just using display-buffer instead of all this rigamarole
;;; won't work; you need to temporarily select the window containing
;;; the *haskell-buffer*, or else the display won't be scrolled to show
;;; the new output.
;;; *** This should really position the window in the buffer so that 
;;; *** the point is on the last line of the window.

(defun haskell-display-output (str)
  (let ((window  (selected-window)))
    (unwind-protect
	(progn
	  (pop-to-buffer *haskell-buffer*)
	  (haskell-display-output-aux str))
      (select-window window))))

(defun haskell-display-output-aux (str)
  (haskell-move-marker)
  (insert str)
  (haskell-move-marker))



;;; ==================================================================
;;; Interactive commands
;;; ==================================================================


;;; HASKELL
;;; -------
;;;
;;; This is the function that fires up the inferior haskell process.

(defun haskell ()
  "Run an inferior Haskell process with input and output via buffer *haskell*.
Takes the program name from the variable haskell-program-name.  
Runs the hooks from inferior-haskell-mode-hook 
(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (if (not (haskell-process-exists-p))
    (start-haskell)))

(defun start-haskell ()
  (message "Starting haskell subprocess...")
  ;; Kill old haskell process.  Normally this routine is only called
  ;; after checking haskell-process-exists-p, but things can get
  ;; screwed up if you rename the *haskell* buffer while leaving the
  ;; old process running.  This forces it to get rid of the old process
  ;; and start a new one.
  (if (get-process "haskell")
      (delete-process "haskell"))
  (let ((haskell-buffer
	 (apply 'make-comint
		"haskell"
		(or haskell-program-name
		    (haskell-mode-error "Haskell-program-name undefined!"))
		nil
		nil)))
    (save-excursion
      (set-buffer haskell-buffer)
      (inferior-haskell-mode))
    (haskell-session-init)
    ;; Wait for process to get started before sending it anything
    ;; to avoid race condition on NeXT.
    (setq *haskell-process-alive* nil)
    (while (not *haskell-process-alive*)
      (sleep-for 1))
    (haskell-send-to-process ":(use-emacs-interface)")
    (haskell-printers-set haskell-initial-printers nil)
    (display-buffer haskell-buffer))
  (message "Starting haskell subprocess...  Done."))


(defun haskell-process-exists-p ()
  (let ((haskell-buffer  (get-buffer *haskell-buffer*)))
    (and haskell-buffer (comint-check-proc haskell-buffer))))



;;; Initialize things on the emacs side, and tell haskell that it's
;;; talking to emacs.

(defun haskell-session-init ()
  (set-haskell-status 'busy)
  (setq *last-loaded* nil)
  (setq *last-module* haskell-main-module)
  (setq *last-pad* haskell-main-pad)
  (setq *haskell-saved-output* nil)
  (haskell-create-main-pad)
  (set-process-filter (get-process "haskell") 'haskell-output-filter)
  )


(defun haskell-create-main-pad ()
  (let ((buffer (get-buffer-create haskell-main-pad)))
    (save-excursion
      (set-buffer buffer)
      (haskell-mode))
    (haskell-record-pad-mapping
      haskell-main-pad haskell-main-module nil)
    buffer))


;;; Called from evaluation and compilation commands to start up a Haskell
;;; process if none is already in progress.

(defun haskell-maybe-create-process ()
  (cond ((haskell-process-exists-p)
	 t)
	(haskell-auto-create-process
	 (start-haskell))
	(t
	 (haskell-mode-error "No Haskell process!"))))



;;; HASKELL-GET-PAD
;;; ------------------------------------------------------------------

;;; This always puts the pad buffer in the "other" window.
;;; Having it wipe out the .hs file window is clearly the wrong
;;; behavior.

(defun haskell-get-pad ()
  "Creates a new scratch pad for the current module.
Signals an error if the current buffer is not a .hs file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
	(do-get-pad fname (current-buffer))
        (haskell-mode-error "Not in a .hs buffer!"))))


(defun do-get-pad (fname buff)
  (let* ((mname (or (haskell-get-modname buff)
		    (read-no-blanks-input "Scratch pad for module? " nil)))
	 (pname (haskell-lookup-pad mname fname))
	 (pbuff nil))
    ;; Generate the base name of the pad buffer, then create the
    ;; buffer.  The actual name of the pad buffer may be something
    ;; else because of name collisions.
    (if (not pname)
	(progn
	  (setq pname (format "*%s-pad*" mname))
	  (setq pbuff (generate-new-buffer pname))
	  (setq pname (buffer-name pbuff))
	  (haskell-record-pad-mapping pname mname fname)
	  )
	(setq pbuff (get-buffer pname)))
    ;; Make sure the pad buffer is in haskell mode.
    (pop-to-buffer pbuff)
    (haskell-mode)))



;;; HASKELL-SWITCH
;;; ------------------------------------------------------------------

(defun haskell-switch ()
  "Switches to \*haskell\* buffer."
  (interactive)
  (haskell-maybe-create-process)
  (pop-to-buffer *haskell-buffer*)
  (push-mark)
  (goto-char (point-max)))



;;; HASKELL-KILL
;;; ------------------------------------------------------------------

(defun haskell-kill ()
  "Kill contents of *haskell* buffer.  \\[haskell-kill]"
  (interactive)
  (save-excursion
    (set-buffer *haskell-buffer*)
    (beginning-of-buffer)
    (let ((mark  (point)))
      (end-of-buffer)
      (kill-region mark (point)))))



;;; HASKELL-COMMAND
;;; ------------------------------------------------------------------

(defun haskell-command (str)
  "Format STRING as a haskell command and send it to haskell process.  \\[haskell-command]"
  (interactive "sHaskell command: ")
  (haskell-send-to-process (format ":%s" str)))


;;; HASKELL-EVAL and HASKELL-RUN
;;; ------------------------------------------------------------------

(defun haskell-eval ()
  "Evaluate expression in current module. \\[haskell-eval]"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux (haskell-get-expression "Haskell expression: ")
		    "emacs-eval"))

(defun haskell-run ()
  "Run Haskell Dialogue in current module"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux (haskell-get-expression "Haskell dialogue: ")
		    "emacs-run"))

(defun haskell-run-main ()
  "Run Dialogue named main in current module"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux "main" "emacs-run"))

(defun haskell-report-type ()
  "Print the type of the expression."
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux (haskell-get-expression "Haskell expression: ")
		    "emacs-report-type"))

(defun haskell-eval-aux (exp fn)
  (cond ((equal *haskell-buffer* (buffer-name))
	 ;; In the *haskell* buffer.
	 (let* ((pname  *last-pad*)
		(mname  *last-module*)
		(fname  *last-loaded*))
	   (haskell-eval-aux-aux exp pname mname fname fn)))
	((buffer-file-name)
	 ;; In a .hs file.
	 (let* ((fname  (buffer-file-name))
		(mname  (haskell-get-modname (current-buffer)))
		(pname  (haskell-lookup-pad mname fname)))
	   (haskell-eval-aux-aux exp pname mname fname fn)))
	(t
	 ;; In a pad.
	 (let* ((pname  (buffer-name (current-buffer)))
		(mname  (haskell-get-module-from-pad pname))
		(fname  (haskell-get-file-from-pad pname)))
	   (haskell-eval-aux-aux exp pname mname fname fn)))
	))

(defun haskell-eval-aux-aux (exp pname mname fname fn)
  (haskell-save-modified-source-files fname)
  (haskell-send-to-process (format ":(%s" fn))
  (haskell-send-to-process
    (prin1-to-string exp))
  (haskell-send-to-process
    (prin1-to-string (or pname fname "interactive")))
  (haskell-send-to-process
    (prin1-to-string
      (if (and pname (get-buffer pname))
	  (save-excursion
	    (set-buffer pname)
	    (buffer-string))
	  "")))
  (haskell-send-to-process
    (format "'|%s|" mname))
  (haskell-send-to-process
    (if fname
	(prin1-to-string (haskell-maybe-get-unit-file-name fname))
	"'#f"))
  (haskell-send-to-process ")")
  (setq *last-pad* pname)
  (setq *last-module* mname)
  (setq *last-loaded* fname))



;;; HASKELL-RUN-FILE, HASKELL-LOAD, HASKELL-COMPILE
;;; ------------------------------------------------------------------

(defun haskell-run-file ()
  "Runs Dialogue named main in current file."
  (interactive)
  (haskell-maybe-create-process)
  (let ((fname  (haskell-get-file-to-operate-on)))
    (haskell-save-modified-source-files fname)
    (haskell-send-to-process ":(emacs-run-file")
    (haskell-send-to-process (prin1-to-string fname))
    (haskell-send-to-process ")")))

(defun haskell-load ()
  "Load current file."
  (interactive)
  (haskell-maybe-create-process)
  (let ((fname  (haskell-get-file-to-operate-on)))
    (haskell-save-modified-source-files fname)
    (haskell-send-to-process ":(emacs-load-file")
    (haskell-send-to-process (prin1-to-string fname))
    (haskell-send-to-process ")")))

(defun haskell-compile ()
  "Compile current file."
  (interactive)
  (haskell-maybe-create-process)
  (let ((fname  (haskell-get-file-to-operate-on)))
    (haskell-save-modified-source-files fname)
    (haskell-send-to-process ":(emacs-compile-file")
    (haskell-send-to-process (prin1-to-string fname))
    (haskell-send-to-process ")")))


(defun haskell-get-file-to-operate-on ()
  (cond ((equal *haskell-buffer* (buffer-name))
	 ;; When called from the haskell process buffer, prompt for a file.
	 (call-interactively 'haskell-get-file/prompt))
	((buffer-file-name)
	 ;; When called from a .hs file buffer, use the unit file
	 ;; associated with it, if there is one.
	 (haskell-maybe-get-unit-file-name (buffer-file-name)))
	(t
	 ;; When called from a pad, use the file that the module the
	 ;; pad belongs to lives in.
	 (haskell-maybe-get-unit-file-name 
	   (haskell-get-file-from-pad (buffer-name (current-buffer)))))))

(defun haskell-get-file/prompt (filename)
  (interactive "fHaskell file:  ")
  (haskell-run-file-aux filename))



;;; HASKELL-EXIT
;;; ------------------------------------------------------------------

(defun haskell-exit ()
  "Quit the haskell process."
  (interactive)
  (cond ((not (haskell-process-exists-p))
	 (message "No process currently running."))
	((y-or-n-p "Do you really want to quit Haskell? ")
	 (haskell-send-to-process ":quit")
	 ;; If we were running the tutorial, mark the temp buffer as unmodified
	 ;; so we don't get asked about saving it later.
	 (if (and *ht-temp-buffer*
		  (get-buffer *ht-temp-buffer*))
	     (save-excursion
	       (set-buffer *ht-temp-buffer*)
	       (set-buffer-modified-p nil)))
	 ;; Try to remove the haskell output buffer from the screen.
	 (bury-buffer *haskell-buffer*)
	 (replace-buffer-in-windows *haskell-buffer*))
	(t
	 nil)))


;;; HASKELL-INTERRUPT
;;; ------------------------------------------------------------------

(defun haskell-interrupt ()
  "Interrupt the haskell process."
  (interactive)
  (if (haskell-process-exists-p)
      (haskell-send-to-process "\C-c")))



;;; HASKELL-EDIT-UNIT
;;; ------------------------------------------------------------------

(defun haskell-edit-unit ()
  "Edit the .hu file."
  (interactive)
  (let ((fname       (buffer-file-name)))
    (if fname
	(let ((find-file-not-found-hooks  (list 'haskell-new-unit))
	      (file-not-found             nil)
	      (units-fname                (haskell-get-unit-file-name fname)))
	  (find-file-other-window units-fname)
	  ;; If creating a new file, initialize it to contain the name
	  ;; of the haskell source file.
	  (if file-not-found
	      (save-excursion
		(insert
	          (if (string= (file-name-directory fname)
			       (file-name-directory units-fname))
		      (file-name-nondirectory fname)
		      fname)
		  "\n"))))
	(haskell-mode-error "Not in a .hs buffer!"))))

(defun haskell-new-unit ()
  (setq file-not-found t))


;;; Look for a comment like "-- unit:" at top of file.
;;; If not found, assume unit file has same name as the buffer but
;;; a .hu extension.

(defun haskell-get-unit-file-name (fname)
  (or (haskell-get-unit-file-name-from-file fname)
      (concat (haskell-strip-file-extension fname) ".hu")))

(defun haskell-maybe-get-unit-file-name (fname)
  (or (haskell-get-unit-file-name-from-file fname)
      (haskell-strip-file-extension fname)))

(defun haskell-get-unit-file-name-from-file (fname)
  (let ((buffer  (get-file-buffer fname)))
    (if buffer
	(save-excursion
	  (beginning-of-buffer)
	  (if (re-search-forward "-- unit:[ \t]*" (point-max) t)
	      (let ((beg  (match-end 0)))
		(end-of-line)
		(buffer-substring beg (point)))
	      nil))
	nil)))




;;; ==================================================================
;;; Support for printers/optimizers menus
;;; ==================================================================

;;; This code was adapted from the standard buff-menu.el code.

(defvar haskell-menu-mode-map nil "")

(if (not haskell-menu-mode-map)
    (progn
      (setq haskell-menu-mode-map (make-keymap))
      (suppress-keymap haskell-menu-mode-map t)
      (define-key haskell-menu-mode-map "m" 'hm-mark)
      (define-key haskell-menu-mode-map "u" 'hm-unmark)
      (define-key haskell-menu-mode-map "x" 'hm-exit)
      (define-key haskell-menu-mode-map "q" 'hm-exit)
      (define-key haskell-menu-mode-map " " 'next-line)
      (define-key haskell-menu-mode-map "\177" 'hm-backup-unmark)
      (define-key haskell-menu-mode-map "?" 'describe-mode)))

;; Printers Menu mode is suitable only for specially formatted data.

(put 'haskell-menu-mode 'mode-class 'special)

(defun haskell-menu-mode ()
  "Major mode for editing Haskell flags.
Each line describes a flag.
Letters do not insert themselves; instead, they are commands.
m -- mark flag (turn it on)
u -- unmark flag (turn it off)
x -- exit; tell the Haskell process to update the flags, then leave menu.
q -- exit; same as x.
Precisely,\\{haskell-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map haskell-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'haskell-menu-mode)
  (setq mode-name "Haskell Flags Menu")
  ;; These are all initialized elsewhere
  (make-local-variable 'hm-current-flags)
  (make-local-variable 'hm-request-fn)
  (make-local-variable 'hm-update-fn)
  (run-hooks 'haskell-menu-mode-hook))


(defun haskell-menu (help-file buffer request-fn update-fn)
  (haskell-maybe-create-process)
  (if (get-buffer buffer)
      (progn
	(pop-to-buffer buffer)
	(goto-char (point-min)))
      (progn
        (pop-to-buffer buffer)
	(insert-file-contents help-file)
	(haskell-menu-mode)
	(setq hm-request-fn request-fn)
	(setq hm-update-fn update-fn)
	))
  (hm-mark-current)
  (message "m = mark; u = unmark; x = execute; q = quit; ? = more help."))



;;; A line that starts with *hm-marked* is a menu item turned on.
;;; A line that starts with *hm-unmarked* is turned off.
;;; A line that starts with anything else is just random text and is
;;; ignored by commands that deal with menu items.

(defvar *hm-marked*   " on")
(defvar *hm-unmarked* "   ")
(defvar *hm-marked-regexp*   " on   \\w")
(defvar *hm-unmarked-regexp* "      \\w")

(defun hm-mark ()
  "Mark flag to be turned on."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at *hm-marked-regexp*)
	 (forward-line 1))
	((looking-at *hm-unmarked-regexp*)
	 (let ((buffer-read-only  nil))
	   (delete-char (length *hm-unmarked*))
	   (insert *hm-marked*)
	   (forward-line 1)))
	(t
	 (forward-line 1))))

(defun hm-unmark ()
  "Unmark flag."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at *hm-unmarked-regexp*)
	 (forward-line 1))
	((looking-at *hm-marked-regexp*)
	 (let ((buffer-read-only  nil))
	   (delete-char (length *hm-marked*))
	   (insert *hm-unmarked*)
	   (forward-line 1)))
	(t
	 (forward-line 1))))

(defun hm-backup-unmark ()
  "Move up and unmark."
  (interactive)
  (forward-line -1)
  (hm-unmark)
  (forward-line -1))


;;; Actually make the changes.

(defun hm-exit ()
  "Update flags, then leave menu."
  (interactive)
  (hm-execute)
  (hm-quit))

(defun hm-execute ()
  "Tell haskell process to tweak flags."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((flags-on   nil)
	  (flags-off  nil))
      (while (not (eq (point) (point-max)))
	(cond ((looking-at *hm-unmarked-regexp*)
	       (setq flags-off (cons (hm-flag) flags-off)))
	      ((looking-at *hm-marked-regexp*)
	       (setq flags-on (cons (hm-flag) flags-on)))
	      (t
	       nil))
	(forward-line 1))
      (funcall hm-update-fn flags-on flags-off))))


(defun hm-quit ()
  (interactive)
  "Make the menu go away."
  (bury-buffer (current-buffer))
  (replace-buffer-in-windows (current-buffer)))

(defun hm-flag ()
  (save-excursion
    (beginning-of-line)
    (forward-char 6)
    (let ((beg  (point)))
      ;; End of flag name marked by tab or two spaces.
      (re-search-forward "\t\\|  ")
      (buffer-substring beg (match-beginning 0)))))


;;; Update the menu to mark only those items currently turned on.

(defun hm-mark-current ()
  (funcall hm-request-fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (cond ((and (looking-at *hm-unmarked-regexp*)
		  (hm-item-currently-on-p (hm-flag)))
	     (hm-mark))
	    ((and (looking-at *hm-marked-regexp*)
		  (not (hm-item-currently-on-p (hm-flag))))
	     (hm-unmark))
	    (t
	     (forward-line 1))))))


;;; See if a menu item is turned on.

(defun hm-item-currently-on-p (item)
  (member-string= item hm-current-flags))

(defun member-string= (item list)
  (cond ((null list)
	 nil)
	((string= item (car list))
	 list)
	(t
	 (member-string= item (cdr list)))))



;;; Make the menu for printers.

(defvar *haskell-printers-help*
  (concat (getenv "HASKELL") "/emacs-tools/printer-help.txt")
  "Help file for printers.")

(defvar *haskell-printers-buffer* "*Haskell printers*")

(defun haskell-printers ()
  "Set printers interactively."
  (interactive)
  (haskell-menu
    *haskell-printers-help*
    *haskell-printers-buffer*
    'haskell-printers-inquire
    'haskell-printers-set))
		
(defun haskell-printers-inquire ()
  (setq hm-current-flags t)
  (haskell-send-to-process ":(emacs-send-printers)")
  (while (eq hm-current-flags t)
    (sleep-for 1)))

(defun haskell-printers-update (data)
  (setq hm-current-flags (read data)))

(defun haskell-printers-set (flags-on flags-off)
  (haskell-send-to-process ":(emacs-set-printers '")
  (haskell-send-to-process (prin1-to-string flags-on))
  (haskell-send-to-process ")"))


;;; Equivalent stuff for the optimizers menu

(defvar *haskell-optimizers-help*
  (concat (getenv "HASKELL") "/emacs-tools/optimizer-help.txt")
  "Help file for optimizers.")

(defvar *haskell-optimizers-buffer* "*Haskell optimizers*")

(defun haskell-optimizers ()
  "Set optimizers interactively."
  (interactive)
  (haskell-menu
    *haskell-optimizers-help*
    *haskell-optimizers-buffer*
    'haskell-optimizers-inquire
    'haskell-optimizers-set))
		
(defun haskell-optimizers-inquire ()
  (setq hm-current-flags t)
  (haskell-send-to-process ":(emacs-send-optimizers)")
  (while (eq hm-current-flags t)
    (sleep-for 1)))

(defun haskell-optimizers-update (data)
  (setq hm-current-flags (read data)))

(defun haskell-optimizers-set (flags-on flags-off)
  (haskell-send-to-process ":(emacs-set-optimizers '")
  (haskell-send-to-process (prin1-to-string flags-on))
  (haskell-send-to-process ")"))



;;; ==================================================================
;;; Random utilities
;;; ==================================================================


;;; Keep track of the association between pads, modules, and files.
;;; The global variable is a list of (pad-buffer-name module-name file-name)
;;; lists.

(defvar *haskell-pad-mappings* ()
  "Associates pads with their corresponding module and file.")

(defun haskell-record-pad-mapping (pname mname fname)
  (setq *haskell-pad-mappings*
	(cons (list pname mname fname) *haskell-pad-mappings*)))

(defun haskell-get-module-from-pad (pname)
  (car (cdr (assoc pname *haskell-pad-mappings*))))

(defun haskell-get-file-from-pad (pname)
  (car (cdr (cdr (assoc pname *haskell-pad-mappings*)))))

(defun haskell-lookup-pad (mname fname)
  (let ((pname  (haskell-lookup-pad-aux mname fname *haskell-pad-mappings*)))
    (if (and pname (get-buffer pname))
	pname
	nil)))

(defun haskell-lookup-pad-aux (mname fname list)
  (cond ((null list)
	 nil)
	((and (equal mname (car (cdr (car list))))
	      (equal fname (car (cdr (cdr (car list))))))
	 (car (car list)))
	(t
	 (haskell-lookup-pad-aux mname fname (cdr list)))))



;;; Save any modified .hs and .hu files.
;;; Yes, the two set-buffer calls really seem to be necessary.  It seems
;;; that y-or-n-p makes emacs forget we had temporarily selected some
;;; other buffer, and if you just do save-buffer directly it will end
;;; up trying to save the current buffer instead.  The built-in
;;; save-some-buffers function has this problem....

(defun haskell-save-modified-source-files (filename)
  (let ((buffers   (buffer-list))
	(found-any nil))
    (while buffers
      (let ((buffer  (car buffers)))
	(if (and (buffer-modified-p buffer)
		 (save-excursion
		   (set-buffer buffer)
		   (and buffer-file-name
			(haskell-source-file-p buffer-file-name)
			(setq found-any t)
			(or (null haskell-ask-before-saving)
			    (and filename (string= buffer-file-name filename))
			    (y-or-n-p
			        (format "Save file %s? " buffer-file-name))))))
	    (save-excursion
	      (set-buffer buffer)
	      (save-buffer))))
      (setq buffers (cdr buffers)))
    (if found-any
	(message "")
        (message "(No files need saving)"))))
  
(defun haskell-source-file-p (filename)
  (or (string-match "\\.hs$" filename)
      (string-match "\\.lhs$" filename)
      (string-match "\\.hi$" filename)
      (string-match "\\.hu$" filename)))



;;; Buffer utilities

(defun haskell-move-marker ()
  "Moves the marker and point to the end of buffer"
  (set-marker comint-last-input-end (point-max))
  (set-marker (process-mark (get-process "haskell")) (point-max))
  (goto-char (point-max)))
  

	
;;; Extract the name of the module the point is in, from the given buffer.

(defvar *haskell-re-module-hs*  "^module\\s *")
(defvar *haskell-re-module-lhs* "^>\\s *module\\s *")
(defvar *haskell-re-modname* "[A-Z]\\([a-z]\\|[A-Z]\\|[0-9]\\|'\\|_\\)*")

(defun haskell-get-modname (buff)
  "Get module name in BUFFER that point is in."
  (save-excursion
    (set-buffer buff)
    (let ((regexp  (if (haskell-lhs-filename-p (buffer-file-name))
		       *haskell-re-module-lhs*
		       *haskell-re-module-hs*)))
      (if (or (looking-at regexp)
	      (re-search-backward regexp (point-min) t)
	      (re-search-forward regexp (point-max) t))
	  (progn
	    (goto-char (match-end 0))
	    (if (looking-at *haskell-re-modname*)
		(buffer-substring (match-beginning 0) (match-end 0))
		(haskell-mode-error "Module name not found!!")))
	  "Main"))))


;;; Strip file extensions.
;;; Only strip off extensions we know about; e.g.
;;; "foo.hs" -> "foo" but "foo.bar" -> "foo.bar".

(defvar *haskell-filename-regexp* "\\(.*\\)\\.\\(hs\\|lhs\\)$")

(defun haskell-strip-file-extension (filename)
  "Strip off the extension from a filename."
  (if (string-match *haskell-filename-regexp* filename)
      (substring filename (match-beginning 1) (match-end 1))
      filename))


;;; Is this a .lhs filename?

(defun haskell-lhs-filename-p (filename)
  (string-match ".*\\.lhs$" filename))


;;; Haskell mode error

(defun haskell-mode-error (msg)
  "Show MSG in message line as an error from the haskell mode."
  (error (concat "Haskell mode:  " msg)))



;;; ==================================================================
;;; User customization
;;; ==================================================================

(defvar haskell-load-hook nil
  "This hook is run when haskell is loaded in.
This is a good place to put key bindings."
  )
	
(run-hooks 'haskell-load-hook)




;;;======================================================================
;;; Tutorial mode setup
;;;======================================================================

;;; Set up additional key bindings for tutorial mode.

(defvar ht-mode-map (make-sparse-keymap))

(haskell-establish-key-bindings ht-mode-map)
(define-key ht-mode-map "\C-c\C-f" 'ht-next-page)
(define-key ht-mode-map "\C-c\C-b" 'ht-prev-page)
(define-key ht-mode-map "\C-c\C-l" 'ht-restore-page)
(define-key ht-mode-map "\C-c?"    'describe-mode)

(defun haskell-tutorial-mode ()
  "Major mode for running the Haskell tutorial.  
You can use these commands:
\\{ht-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ht-mode-map)
  (setq major-mode 'haskell-tutorial-mode)
  (setq mode-name "Haskell Tutorial")
  (set-syntax-table haskell-mode-syntax-table)
  (run-hooks 'haskell-mode-hook))


(defun haskell-tutorial ()
  "Run the haskell tutorial."
  (interactive)
  (ht-load-tutorial)
  (ht-make-buffer)
  (ht-display-page)
  (haskell-maybe-create-process)
  (haskell-send-to-process ":(emacs-set-printers '(interactive))")
  )


;;; Load the tutorial file into a read-only buffer.  Do not display this
;;; buffer.

(defun ht-load-tutorial ()
  (let ((buffer  (get-buffer *ht-file-buffer*)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (beginning-of-buffer))
	(save-excursion
	  (set-buffer (setq buffer (get-buffer-create *ht-file-buffer*)))
	  (let ((fname (substitute-in-file-name *ht-source-file*)))
	    (if (file-readable-p fname)
		(ht-load-tutorial-aux fname)
		(call-interactively 'ht-load-tutorial-aux)))))))

(defun ht-load-tutorial-aux (filename)
  (interactive "fTutorial file: ")
  (insert-file filename)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (beginning-of-buffer))


;;; Create a buffer to use for messing about with each page of the tutorial.
;;; Put the buffer into haskell-tutorial-mode.

(defun ht-make-buffer ()
  (find-file (concat "/tmp/" (make-temp-name "ht") ".lhs"))
  (setq *ht-temp-buffer* (buffer-name))
  (haskell-tutorial-mode))


;;; Commands for loading text into the tutorial pad buffer

(defun ht-next-page ()
  "Go to the next tutorial page."
  (interactive)
  (if (ht-goto-next-page)
      (ht-display-page)
      (beep)))

(defun ht-goto-next-page ()
  (let ((buff  (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (search-forward "\C-l" nil t))
      (set-buffer buff))))

(defun ht-prev-page ()
  "Go to the previous tutorial page."
  (interactive)
  (if (ht-goto-prev-page)
      (ht-display-page)
      (beep)))

(defun ht-goto-prev-page ()
  (let ((buff  (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (search-backward "\C-l" nil t))
      (set-buffer buff))))

(defun ht-goto-page (arg)
  "Go to the tutorial page specified as the argument."
  (interactive "sGo to page: ")
  (if (ht-searchfor-page (format "Page: %s " arg))
      (ht-display-page)
      (beep)))

(defun ht-goto-section (arg)
  "Go to the tutorial section specified as the argument."
  (interactive "sGo to section: ")
  (if (ht-searchfor-page (format "Section: %s " arg))
      (ht-display-page)
      (beep)))

(defun ht-searchfor-page (search-string)
  (let ((buff           (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (let ((point  (point)))
	    (beginning-of-buffer)
	    (if (search-forward search-string nil t)
		t
		(progn
		  (goto-char point)
		  nil))))
      (set-buffer buff))))

(defun ht-restore-page ()
  (interactive)
  (let ((old-point  (point)))
    (ht-display-page)
    (goto-char old-point)))

(defun ht-display-page ()
  (set-buffer *ht-file-buffer*)
  (let* ((beg   (progn
		 (if (search-backward "\C-l" nil t)
		     (forward-line 1)
		     (beginning-of-buffer))
		 (point)))
	 (end   (progn
		  (if (search-forward "\C-l" nil t)
		      (beginning-of-line)
		      (end-of-buffer))
		  (point)))
	 (text  (buffer-substring beg end)))
    (set-buffer *ht-temp-buffer*)
    (erase-buffer)
    (insert text)
    (beginning-of-buffer)))



;;;======================================================================
;;; Menu bar stuff
;;;======================================================================

;;; This only works in Emacs version 19, so it's in a separate file for now.

(if (featurep 'menu-bar)
    (load-library "haskell-menu"))
