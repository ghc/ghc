;; ---------------------------------------------------------------------------
;; Time-stamp: <Tue Jun 11 1996 18:01:28 Stardate: [-31]7643.54 hwloidl>
;;
;; Mode for GrAnSim profiles
;; ---------------------------------------------------------------------------

(defvar gransim-auto-hilit t
  "Automagically invoke hilit19.")

(defvar	grandir (getenv "GRANDIR")
  "Root of the GrAnSim installation. Executables should be in grandir/bin")

(defvar hwl-hi-node-face 'highlight
  "Face to be used for specific highlighting of a node")

(defvar hwl-hi-thread-face 'holiday-face
  "Face to be used for specific highlighting of a thread")

;; ---------------------------------------------------------------------------

(setq exec-path (cons (concat grandir "/bin") exec-path))

;; Requires hilit19 for highlighting parts of a GrAnSim profile
(cond (window-system
   (setq hilit-mode-enable-list  '(not text-mode)
         hilit-background-mode   'light
         hilit-inhibit-hooks     nil
         hilit-inhibit-rebinding nil);

   (require 'hilit19)
))


(setq auto-mode-alist
      (append '(("\\.gr" . gr-mode))
	     auto-mode-alist))

(defvar gr-mode-map (make-keymap "GrAnSim Profile Mode SetUp")
  "Keymap for GrAnSim profiles.")

; (fset 'GrAnSim-mode-fiddly gr-mode-map)

;(define-key gr-mode-map [wrap]
;  '("Wrap lines" . hwl-wrap))

;(define-key gr-mode-map [truncate]
;  '("Truncate lines" . hwl-truncate))

;(define-key global-map [C-S-down-mouse-1] 'GrAnSim-mode-fiddly)

;(modify-frame-parameters (selected-frame)
;                         '((menu-bar-lines . 2)))

;(define-key-after gr-mode-map [menu-bar GrAnSim]
;  '("GrAnSim" . (make-sparse-keymap "GrAnSim")) 'edit)

;(defvar GrAnSim-menu-map (make-sparse-keymap "GrAnSim"))

(define-key gr-mode-map [menu-bar GrAnSim]
  (cons "GrAnSim"  (make-sparse-keymap "GrAnSim"))) ;  'edit)

(define-key gr-mode-map [menu-bar GrAnSim wrap]
  '("Wrap lines" . hwl-wrap))

(define-key gr-mode-map [menu-bar GrAnSim truncate]
  '("Truncate lines" . hwl-truncate))

(define-key gr-mode-map [menu-bar GrAnSim toggle-truncate]
  '("Toggle truncate/wrap" . hwl-toggle-truncate-wrap) )

(define-key gr-mode-map [menu-bar GrAnSim hi-clear]
  '("Clear highlights" . hwl-hi-clear))

(define-key gr-mode-map [menu-bar GrAnSim hi-thread]
  '("Highlight specific Thread" . hwl-hi-thread))

(define-key gr-mode-map [menu-bar GrAnSim hi-node]
  '("Highlight specific Node" . hwl-hi-node))

(define-key gr-mode-map [menu-bar GrAnSim highlight]
  '("Highlight buffer" . hilit-rehighlight-buffer))

(define-key gr-mode-map [menu-bar GrAnSim narrow-event]
  '("Narrow to Event" . hwl-narrow-to-event))

(define-key gr-mode-map [menu-bar GrAnSim narrow-thread]
  '("Narrow to Thread" . hwl-narrow-to-thread))

(define-key gr-mode-map [menu-bar GrAnSim narrow-pe]
  '("Narrow to PE" . hwl-narrow-to-pe))



; (define-key global-map [C-S-down-mouse-1] 'GrAnSim-mode-fiddly)
     

(defvar gr-mode-hook nil
  "Invoked in gr mode.")


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
;(put 'gr-mode 'mode-class 'special)

(defun gr-mode ()
  "Major mode for GrAnSim profiles."
  (interactive)
  (kill-all-local-variables)
  ;(use-local-map gr-mode-map)
  (use-local-map gr-mode-map)     ; This provides the local keymap.
  (setq major-mode 'gr-mode)
  (setq mode-name "GrAnSim Profile Mode")
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (setq truncate-lines t)         ; do not wrap lines (truncates END lines!)
  (auto-save-mode -1)
  ;(setq buffer-offer-save t)
  (run-hooks 'gr-mode-hook))

;; same as mh-make-local-vars
(defun gr-make-local-vars (&rest pairs)
  ;; Take VARIABLE-VALUE pairs and make local variables initialized to the
  ;; value.
  (while pairs
    (make-variable-buffer-local (car pairs))
    (set (car pairs) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))

;; ----------------------------------------------------------------------
;; Highlighting stuff (currently either hilit19 or fontlock is used)
;; ----------------------------------------------------------------------

(hilit-set-mode-patterns
   'gr-mode
   '(;; comments
	("--.*$" nil comment)
	("\\+\\+.*$" nil comment)
	;; hilight important bits in the header
	("^Granularity Simulation for \\(.*\\)$" 1 glob-struct)
	("^PEs[ \t]+\\([0-9]+\\)" 1 decl)
	("^Latency[ \t]+\\([0-9]+\\)" 1 decl)
	("Arith[ \t]+\\([0-9]+\\)" 1 decl)
	("Branch[ \t]+\\([0-9]+\\)" 1 decl)
	("Load[ \t]+\\([0-9]+\\)" 1 decl)
	("Store[ \t]+\\([0-9]+\\)" 1 decl)
	("Float[ \t]+\\([0-9]+\\)" 1 decl)
	("Alloc[ \t]+\\([0-9]+\\)" 1 decl)
	;; hilight PE number and time in each line
	("^PE[ \t]+\\([0-9]+\\)" 1 glob-struct)
	(" \\[\\([0-9]+\\)\\]:" 1 define)
	;; in this case the events are the keyword
					; ("\\(FETCH\\|REPLY\\|RESUME\\|RESUME(Q)\\|SCHEDULE\\|SCHEDULE(Q)\\|BLOCK\\|STEALING\\|STOLEN\\|STOLEN(Q)\\)[ \t]" 1 keyword)
	("\\(FETCH\\|BLOCK\\)[ \t]" 1 label)
	("\\(REPLY\\|RESUME(Q)\\|SCHEDULE(Q)\\|STOLEN(Q)\\)[ \t]" 1 named-param)
	("\\(RESUME\\|SCHEDULE\\|STOLEN\\)[ \t]" 1 msg-quote)
	("\\(STEALING\\)[ \t]" 1 keyword)
	("\\(START\\|END\\)[ \t]" 1 defun)
	("\\(SPARK\\|SPARKAT\\|USED\\|PRUNED\\)[ \t]" 1 crossref)
	("\\(EXPORTED\\|ACQUIRED\\)[ \t]" 1 string)
	;; especially interesting are END events; hightlight runtime etc
	(",[ \t]+RT[ \t]+\\([0-9]+\\)" 1 define)
	;; currently unused but why not?
	("\"" ".*\"" string))
)

;; --------------------------------------------------------------------------
;; Own fcts for selective highlighting
;; --------------------------------------------------------------------------

(defun hwl-hi-node (node)
 "Highlight node in GrAnSim profile."
 (interactive "sNode (hex): ")
 (save-excursion 
  (let* ( (here (point))
	  (len (length node)) )
  (goto-char (point-min))
  (while (search-forward node nil t)
         (let* ( (end (point))
                 (start (- end len)) )
               (add-text-properties start end `(face ,hwl-hi-node-face))
         )
  ) )
 )
)

(defun hwl-hi-thread (task)
 "Highlight task in GrAnSim profile."
 (interactive "sTask: ")
 (save-excursion 
  (let* ( (here (point))
	  (len (length task))
	  (se-str (format "[A-Z)]\\s-+%s\\(\\s-\\|,\\)" task))
        )
  (goto-char (point-min))
  (while (re-search-forward se-str nil t)
    (let ( (c (current-column)) )
         (if (and (> c 10) (< c 70))
            (let* ( (end (1- (point)))
                    (start (- end len)) )
                  (add-text-properties start end `(face ,hwl-hi-thread-face))
         ) ) )
  ) )
 )
)

(defun hwl-hi-line ()
 "Highlight the current line."
 (interactive)
 (save-excursion
  (beginning-of-line)
  (let ( (beg (point)) )
       (end-of-line)
       (add-text-properties beg (point) '(face highlight))
  )
 )
)

(defun hwl-unhi-line ()
 "Unhighlight the current line."
 (interactive)
 (save-excursion
  (beginning-of-line)
  (let ( (beg (point)) )
       (end-of-line)
       (add-text-properties beg (point) '(face nil))
  )
 )
)

; Doesn't work yet
(defun hwl-hi-from-to (from to)
 "Highlight region between two timestamps."
 (interactive "nFrom: \nnTo:")
 (save-excursion 
  (let* ( (here (point))
	  (now 0)
	  start end 
	  (separator '"+++++")
        )
  (goto-char (point-min))
    ; (re-search-forward REGEXP)
  (search-forward separator nil t)
  (forward-line)
  (while (< now from) 
    (beginning-of-line)
    (forward-line)
    (forward-char 7)
    (setq beg (point))
    (search-forward "]")
    (setq time-str (buffer-substring beg (- (point) 2)))
    (setq now (string-to-number time-str))
  )
  (if (< now from)
    nil
    (setq start (point))
    (while (< now to) 
      (beginning-of-line)
      (forward-line)
      (forward-char 7)
      (setq beg (point))
      (search-forward "]")
      (setq time-str (buffer-substring beg (- (point) 2)))
      (setq now (string-to-number time-str))
    )
    (if (< now to)
      nil
      (setq end (point))
      (add-text-properties start end '(face paren-match-face))
         )
   ) 
  ) ; let
 ) ; excursion
)

(defun hwl-hi-clear ()
  (interactive)
  (let ( (start (point-min) )
         (end (point-max)) )
       (remove-text-properties start end '(face nil))
  )
)

;; --------------------------------------------------------------------------
;; Misc Elisp functions
;; --------------------------------------------------------------------------

(defun hwl-wrap ()
  (interactive)
  (setq truncate-lines nil)
  (hilit-recenter nil)
)

(defun hwl-truncate ()
  (interactive)
  (setq truncate-lines t)
  (hilit-recenter nil)
)

(defun hwl-toggle-truncate-wrap ()
  (interactive)
  (if truncate-lines (setq truncate-lines nil)
                     (setq truncate-lines t))
  (hilit-recenter nil)
)

(defun hwl-narrow-to-pe (pe)
    (interactive "nPE: ")
    (hwl-narrow 1 pe "")
)

(defun hwl-narrow-to-thread (thread)
    (interactive "sThread: ")
    (hwl-narrow 2 thread "")
)

(defun hwl-narrow-to-event (event)
    (interactive "sEvent: ")
    (hwl-narrow 3 0 event)
)

(defun hwl-narrow (mode id str)
  ( let* ((outbuffer (get-buffer-create "*GrAnSim Narrowed*"))
	 ;(from (beginning-of-buffer))
	 ;(to   (end-of-buffer))
	 ;(to   (point))                            ; (region-end))
	 ;(text (buffer-substring from to))         ; contains text in region
	 (w    (selected-window))		   
	 ;(nh   5)                                  ; height of new window
	 ;(h    (window-height w))                  ; height of selcted window
	 ;(h1   (if (<= h nh) (- h 1) (- h nh)))    ; height of old window
	 (w1   (get-buffer-window outbuffer 'visible))

	 (infile (buffer-file-name))                ; or 
	 (inbuffer (current-buffer))
	 (command "tf")
	 ;(mode_opt (cond ((eq mode 1) "-p")
	 ;	         ((eq mode 2) "-t")
	 ;	         ((eq mode 3) "-e")
	 ;		 (t "-v")))
	)
        (if w1 (message "Window *GrAnSim Narrowed* already visible") 
               (split-window w nil nil))
        (switch-to-buffer-other-window outbuffer)
	(erase-buffer)
	(setq truncate-lines t)
	(gr-mode)
	;(beginning-of-buffer)
	;(set-mark)
	;(end-of-buffer)
	;(delete-region region-beginning region-end)
	(cond ((eq mode 1)
	       ;(message (format "Narrowing to Processor %d" id))
		(call-process command nil outbuffer t "-p" (format "%d" id) infile ))
	      ((eq mode 2)
	       ;(message (format "Narrowing to Thread %d" id))
		(call-process command nil outbuffer t "-t" (format "%s" id) infile ))
	      ((eq mode 3)
	       ;(message (format "Narrowing to Event %s" str))
		(call-process command nil outbuffer t "-e" str infile ))
	      )
  )
)

(defun hwl-command-on-buffer (prg opts file)
  (interactice "CProgram:\nsOptions:\nfFile:")
  ( let* ((outbuffer (get-buffer-create "*GrAnSim Command*"))
	 (from (beginning-of-buffer))
	 (to   (end-of-buffer))
	 ;(to   (point))                            ; (region-end))
	 ;(text (buffer-substring from to))         ; contains text in region
	 (w    (selected-window))		   
	 ;(nh   5)                                  ; height of new window
	 ;(h    (window-height w))                  ; height of selcted window
	 ;(h1   (if (<= h nh) (- h 1) (- h nh)))    ; height of old window
	 (w1   (get-buffer-window outbuffer 'visible))

	 (infile (buffer-file-name))                ; or 
	 (inbuffer (current-buffer))
	 ;(command "tf")
	 ;(mode_opt (cond ((eq mode 1) "-p")
	 ;	         ((eq mode 2) "-t")
	 ;	         ((eq mode 3) "-e")
	 ;		 (t "-v")))
	)
        (if w1 (message "Window *GrAnSim Command* already visible") 
               (split-window w nil nil))
        (switch-to-buffer-other-window outbuffer)
	(erase-buffer)
	(setq truncate-lines t)
	(gr-mode)
	(call-process prg nil outbuffer opts file)
  )
)

;; ToDo: Elisp Fcts for calling scripts like gr3ps etc

(define-key gr-mode-map "\C-ct" 'hwl-truncate)
(define-key gr-mode-map "\C-cw" 'hwl-wrap)
(define-key gr-mode-map "\C-ch" 'hilit-rehighlight-buffer)
(define-key gr-mode-map "\C-cp" 'hwl-narrow-to-pe)
(define-key gr-mode-map "\C-ct" 'hwl-narrow-to-thread)
(define-key gr-mode-map "\C-ce" 'hwl-narrow-to-event)
(define-key gr-mode-map "\C-c\C-e" '(lambda () (hwl-narrow-to-event "END")))
(define-key gr-mode-map "\C-c " 'hwl-toggle-truncate-wrap)
(define-key gr-mode-map "\C-cN" 'hwl-hi-node)
(define-key gr-mode-map "\C-cT" 'hwl-hi-thread)
(define-key gr-mode-map "\C-c\C-c" 'hwl-hi-clear)

;; ---------------------------------------------------------------------------
;; Mode for threaded C files
;; ---------------------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.hc" . hc-mode))
	     auto-mode-alist))

(define-derived-mode hc-mode c-mode "hc Mode"
  "Derived mode for Haskell C files."
)

(hilit-set-mode-patterns
      'hc-mode
      '(
	("\\(GRAN_FETCH\\|GRAN_RESCHEDULE\\|GRAN_FETCH_AND_RESCHEDULE\\|GRAN_EXEC\\|GRAN_YIELD\\)" 1 keyword)
	("FB_" nil defun)
	("FE_" nil  define)
	("__STG_SPLIT_MARKER" nil msg-note)
	("^.*_ITBL.*$" nil defun)
	("^\\(I\\|E\\|\\)FN.*$" nil define)
	)
)

; (define-key global-map [S-pause] 'hc-mode)
