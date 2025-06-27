;;; package --- Summary: Custom ox-html publisher -*- lexical-binding: t -*-

;; Filename: kothman-ox-html.el
;; Description: An ox-html plugin
;; Author: Morgan Kothman <abkothman@gmail.com>
;; Maintainer: Morgan Kothman <abkothman@gmail.com>
;; Copyright: You can't copyright this.

;;; Commentary:
;;
;; Morgan Kothman's Ox-Html Plugin
;;
;; This plugin replaces some of the built-in ox-html publishing functions with
;; my own, purely for my personal convenience.  There's probably a better way to
;; do this, like writing my own exporter that builds from ox-html
;;

;;; Code:
(require 'ox-publish)
(require 'ox-html)
;; version >= 1.3

;;; Define what directory name should be looked for during post-save hook
(defvar directory-name "kothman.github.io"
  "The directory name this project resides in, to be used by helper.
Scripts in determining if the saved-buffer should trigger recompilation.")

;;; Define our navigation directories to display
(defvar my-org-index-nav nil "My navigation menu items.")
(setq my-org-index-nav
  '(("home" . "/")
   ; ("resources" . "/resources/")
   ; ("services" . "/services/")
    ("resume" . "/resume/")
    ("blog" . "/blog/")))

(defvar base-url nil
  "The base url value to be used when building links.
I'm not sure if org-mode's relative path building works for <head> html.")
(defcustom export-for-dev nil
  "Nil unless the project is being exported for development."
  :type '(boolean)
  :group 'kothman-ox-html
  :options '(nil t))

;;; CHANGE ME IF NEEDED
(setq export-for-dev t)

;;; Define the postable format for each language here
(defvar my-org-html-postamble-html nil
  "My footer for html exports.")
(setq my-org-html-postamble-html
      (concat "<footer>"
	      "<span>"
	      "made with "
	      "<a href=\"https://www.gnu.org/software/emacs/\" target=\"_blank\">emacs</a>, "
	      "<a href=\"https://orgmode.org/\" target=\"_blank\">org-mode</a>, and persistence"
	      "</span>"
	      "<br>"
	      "<a target=\"_blank\" href=\"https://validator.w3.org/check?uri=https://kothman.github.io\">"
	      "w3c validation"
	      "</a>"
	      "</footer>"))

(defun my-export-for-prod ()
  "Build the project for production."
  (interactive)
  (my-export-for-dev t))
(defun my-export-for-dev (&optional build-for-prod)
  "Build the project for dev if BUILD-FOR-PROD is nil."
  (interactive)
  (let ((prev-export-for-dev export-for-dev))
    (if build-for-prod
	(setq export-for-dev nil)
      (setq export-for-dev t))
    (my-update-base-url)
   (my-org-force-publish)
    (setq export-for-dev prev-export-for-dev)))
(defun my-update-base-url ()
  "Update MY-BASE-URL based on MY-EXPORT FOR DEV."
  (interactive)
  (if export-for-dev
      (setq base-url "file:///home/kothman/src/kothman.github.io/public")
    (setq base-url "https://kothman.github.io")))


(my-update-base-url) ; call this once on load


;;; Helper functions for publishing the project
(defun my-org-publish ()
  "My function for publishing org projects as HTML."
  (interactive)
  (org-publish-project "org"))
;;; Force publish, shouldn't need to use this
(defun my-org-force-publish ()
  "My function for force-publishing org projects as HTML."
  (interactive)
  (org-publish-project "org" t))

;;; Helper function to see if key in list. Isn't there already some
;;; standard elisp function that already does this?
(defun contains (key list)
  "Return t if KEY is in LIST."
       (let ((current (car list)))
	 ;; If the current item is nil or matches the key,
	 ;; then it can be returned either way.
	 (if (or (equal key current) (equal nil current))
	     current
	     ;; If the current item isn't the key, and isn't nil...
	     ;; recurse into the list
	   (contains key (cdr list)))))

;;; My hook to export org documents as hooks if the saved file
;;; is in the "directory-name" directory.
(defun my-org-after-save-hook ()
  "An after-save hook to publish the org files."
  ;; set list-of-path-parts to a list of directory names (and ending
  ;;;; with the file name)
  (let ((list-of-path-parts (split-string (buffer-file-name) "\\/")))
    ;; If and only if the list contains the our project key, the
    ;; directory name
    (when (contains directory-name list-of-path-parts)
      ;; publish the org project
      (org-publish-project "org"))))
(add-hook 'after-save-hook 'my-org-after-save-hook)
;;; Define some kbd shortcuts for publishing the project
(keymap-global-set "C-c o P" 'my-org-force-publish)
(keymap-global-set "C-c o p" 'my-org-publish)

(defvar my-org-nav-item-template)
(setq my-org-nav-item-template
      (concat "<li id=\"nav-%s\"><a href=\""
	      base-url
	      "%s"
	      (when export-for-dev "index.html")
	      "\">%s</a></li>"))
(defvar my-org-nav-template)
(setq my-org-nav-template
  "<header><nav><ul>%s</ul></nav></header>")
(defvar my-org-nav-html
  ""
  "The HTML for the navigation of each page.  Set by later function.")

(defun my-org-build-navigation-html (navlist)
  "Get the html for the nav section, built from NAVLIST."
  (interactive)
  (let ((formatted-item nil)
	(inside-html
	 (concat "<li><a href=\""
		 base-url
		 (when export-for-dev "/index.html")
		 "\"><h1>%t</h1><h3>%s</h3></a>")))
    
    (dolist
	;;; (list-item/element, list to loop through,
	;;;    &optional return element )
	(list-item navlist)
      ;; set the formatted-item to the formatted result
      (setq formatted-item
	    (format my-org-nav-item-template
		    (car list-item) (cdr list-item) (car list-item)))
      (setq inside-html
	    (concat inside-html formatted-item)))
    ;; add the contact link custom, since it's an email based on a var
    (setq inside-html (concat inside-html "<li id=\"nav-contact\">%e</li>"))
    (setq my-org-nav-html (format my-org-nav-template inside-html))))

;;; Build the html sections that should go on each page
(my-org-build-navigation-html my-org-index-nav)

;;;;; Redefine ox-html functions for better control
;;
;; Timestamp
(defun org-html-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))
;; List Item formatting
(defun org-html-format-list-item (contents type checkbox info
					   &optional term-counter-id
					   headline)
  "Format a list item into HTML.
CONTENTS is the item contents.  TYPE is one of symbols `ordered',
`unordered', or `descriptive'.  CHECKBOX checkbox type is nil or one of
symbols `on', `off', or `trans'.  INFO is the info plist.
TERM-COUNTER-ID keeps track of the count for ordered lists, I think.
HEADLINE is maybe the headline for the list-item?  I'm not quite sure."
  (let ((class (if checkbox
		   (format " class=\"%s\""
			   (symbol-name checkbox)) ""))
	(checkbox (concat (org-html-checkbox checkbox info)
			  (and checkbox " ")))
	(br (org-html-close-tag "br" nil info))
	(extra-newline (if (and (org-string-nw-p contents) headline) "\n" "")))
    (concat
     (pcase type
       (`ordered
	(let* ((counter term-counter-id)
	       (extra (if counter (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s%s>" class extra)
	   (when headline (concat headline br)))))
       (`unordered
	(let* ((id term-counter-id)
	       (extra (if id (format " id=\"%s\"" id) "")))
	  (concat
	   (format "<li%s%s>" class extra)
	   (when headline (concat headline br)))))
       (`descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat "<div class=\"description-list-item\">"
		  (format "<dt%s>%s</dt>"
			  class (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     extra-newline
     (and (org-string-nw-p contents) (org-trim contents))
     extra-newline
     (pcase type
       (`ordered "</li>")
       (`unordered "</li>")
       (`descriptive "</dd></div>")))))
(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (coding-system-get org-html-coding-system :mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   ;; add another container for some fun styling tricks
   (let ((div (assq 'content (plist-get info :html-divs))))
     (concat
      (format "<%s id=\"%s-subcontainer\" class=\"%s-subcontainer\">\n"
	      (nth 1 div)
	      (nth 2 div)
	      (plist-get info :html-content-class))
      contents
      "</div>\n"))
   
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))
;; Remove closing tags for meta-info
(defun org-html--build-meta-entry
    (label identity &optional content-format &rest content-formatters)
  "Build a meta tag using the provided information.

Construct <meta> tag of form <meta LABEL=\"IDENTITY\">, or when CONTENT-FORMAT
is present: <meta LABEL=\"IDENTITY\" content=\"{content}\">

Here {content} is determined by applying any CONTENT-FORMATTERS to the
CONTENT-FORMAT and encoding the result as plain text."
  (concat "<meta "
	  (format "%s=\"%s" label identity)
	  (when content-format
	    (concat "\" content=\""
		    (replace-regexp-in-string
		     "\"" "&quot;"
		     (org-html-encode-plain-text
		      (if content-formatters
			  (apply #'format content-format content-formatters)
			content-format)))))
	  ;;; a changed line
	  "\">\n"))

;;;;;; Set org export variables
;;;;;;
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;;; Link Export Filter
;;
;; All external links should open in a new tab
;;
;;;
(defun my-org-export-add-target-blank-to-http-links (text backend info)
  "Add target=\"_blank\" to external links.
TEXT must start with http, BACKEND is checked to ensure html export. INFO is a
variable."
  (setq info info) ; shut up the compiler
  (when (and
         (org-export-derived-backend-p backend 'html)
         (string-match "href=\"http[^\"]+" text)
         (not (string-match "target=\"" text)))
    (string-match "<a " text)
    (replace-match "<a target=\"_blank\" " nil nil text)))
(add-to-list 'org-export-filter-link-functions
             'my-org-export-add-target-blank-to-http-links)

;;; Include timestamps
; I thought brackets were supposed to be removed automatically when using a
; custom timestamp format, but apparently a filter is still needed to replace
; them.
(setq org-export-filter-timestamp-functions
      '((lambda (transcoded-data-string backend-symbol communication-channel-plist)
	  "My custom expression to remove those silly brackets im timestampts."
	  (replace-regexp-in-string "\\(&gt;\\)?\\(&lt;\\)?" "" transcoded-data-string))))
(setq org-timestamp-custom-formats '("%A, %B %e, %Y" . "%Y-%m-%d %a %H:%M"))
(add-hook 'org-mode-hook
	  #'(lambda () "Set variables as buffer-local"
	      (setq org-display-custom-times t)))

;;; A bunch of export variables
(setq org-export-with-timestamps t
      org-export-timestamp-file t
      org-export-with-date t
      org-export-with-entities t
      org-export-with-tags t
      org-export-headline-levels 6
      org-html-toplevel-hlevel 1
      ;;; Don't export the Table of Contents
      org-export-with-toc nil
      ;;; Don't export drawers
      org-export-with-drawers nil
      ;;; Don't export section numbers
      org-export-with-section-numbers nil
      ;;; Mark broken links
      org-export-with-broken-links 'mark
      ;;; Remove the standard title
      org-export-with-title nil
      org-export-with-email t)

;;;;;;
;;;;;; Set HTML-specific export variables
;;;;;;
;;; Replace the title custom preamble
(setq org-html-preamble t)
;;; Define the preamble format for each language here
(setq org-html-preamble-format
      `(("en" ,my-org-nav-html)))
;;; Customize Postamble
(setq org-html-postamble t)
(setq org-html-postamble-format
      `(("en" ,my-org-html-postamble-html)))
;;; Include stylesheet in the head
(setq org-html-head
      (concat "<link rel=\"stylesheet\" href=\"" base-url "/styles.css?busted=" (format-time-string "%s") "\">"
			    "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css\">"))
(setq org-html-head-extra
 "<!-- don't forget to (use-package package-name :ensure t)date me in each org file with the active nav item! -->")
(defvar org-html-head-develop nil
  "The development stylesheet headers to use when testing locally.")
(setq org-html-head-develop (concat
		     "<link rel=\"stylesheet\" href=\"/styles.css\">"
		     "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css\">"))

;;; No JavaScript for now
(setq org-html-head-include-scripts nil)
;;; Set our own styles in org-html-head per spec
(setq org-html-head-include-default-style nil)
;;; Not using InfoJS
(setq org-html-use-infojs nil)
;;; Scripts are also set in the org-html-head, if needed
(setq org-html-scripts nil)

;;; DON'T PROMPT for EVAL
(setq org-confirm-babel-evaluate nil)
;;; refresh all org-mode views
(org-mode-restart)
;;;;;;
;;;;;; Setup org projects for each export type
;;;;;;
(setq org-publish-project-alist
      '(
	;;; Org Notes exports .org files to HTML5
	("org-notes"
	 :base-directory "~/src/kothman.github.io/src/"
	 :base-extension "org"
	 :publishing-directory "~/src/kothman.github.io/public/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 6
	 :auto-preamble t)
	("org-static"
	 :base-directory "~/src/kothman.github.io/src/"
	 :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf"
	 :publishing-directory "~/src/kothman.github.io/public/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("org" :components ("org-notes" "org-static"))
	))

(my-org-force-publish)

(provide 'kothman-ox-html)
;;; kothman-ox-html.el ends here
