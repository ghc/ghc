<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA dsssl>
]]>
]>

<!-- This is (or was) the standard Cygnus DocBook tools stylesheet

Options added:

%section-autolabel%: true
%paper-type%: A4

-->


<style-sheet>

<style-specification id="print" use="docbook">
<style-specification-body> 

;; ====================
;; customize the print stylesheet
;; ====================

;; make funcsynopsis look pretty
(define %funcsynopsis-decoration%
  ;; Decorate elements of a FuncSynopsis?
  #t)

;; use graphics in admonitions, and have their path be "."
;; NO: we are not yet ready to use gifs in TeX and so forth
(define %admon-graphics-path%
  "./")
(define %admon-graphics%
  #f)

;; this is necessary because right now jadetex does not understand
;; symbolic entities, whereas things work well with numeric entities.
(declare-characteristic preserve-sdata?
          "UNREGISTERED::James Clark//Characteristic::preserve-sdata?"
          #f)
(define %two-side% #t)

(define %section-autolabel% 
  ;; Are sections enumerated?
  #t)
;; (define %title-font-family% 
;;   ;; The font family used in titles
;;   "Ariel")
(define %visual-acuity%
  ;; General measure of document text size
  ;; "presbyopic"
  ;; "large-type"
  "normal")

(define %generate-set-toc% #t)
(define %generate-part-toc% #t)

;; (define %block-start-indent% 10pt)

(define %graphic-default-extension% "eps")

(define %paper-type%
  ;; Name of paper type
  "A4")

</style-specification-body>
</style-specification>

<!--
;; ====================
;; customize the html stylesheet
;; ====================
-->
<style-specification id="html" use="docbook">
<style-specification-body> 

;; this is necessary because right now jadetex does not understand
;; symbolic entities, whereas things work well with numeric entities.
(declare-characteristic preserve-sdata?
          "UNREGISTERED::James Clark//Characteristic::preserve-sdata?"
          #f)

;; put the legal notice in a separate file
(define %generate-legalnotice-link%
  #t)

;; use graphics in admonitions, and have their path be "stylesheet-images"
;; NO: they do not yet look very good
(define %admon-graphics-path%
  "./stylesheet-images/")
(define %admon-graphics%
  #f)

;; make funcsynopsis look pretty
(define %funcsynopsis-decoration%
  ;; Decorate elements of a FuncSynopsis?
  #t)

(define %html-ext% ".html")
(define %body-attr%
  ;; What attributes should be hung off of BODY?
  '())
;;  (list
;;   (list "BGCOLOR" "#FFFFFF")
;;   (list "TEXT" "#000000")))

(define %generate-article-toc% 
  ;; Should a Table of Contents be produced for Articles?
  ;; If true, a Table of Contents will be generated for each 'Article'.
  #t)

(define %generate-part-toc% #t)

(define %shade-verbatim%
  #t)

(define %use-id-as-filename%
  ;; Use ID attributes as name for component HTML files?
  #t)

(define %graphic-default-extension% "gif")

(define %section-autolabel% #t)

;; Uncomment the setting below if you want .html output as one 
;; big page. [ Notice that 'jade' dumps the output on stdout 
;; rather than on honour the -o option...at least my copy does.]
;;
;;(define nochunks #t)

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">

</style-sheet>
