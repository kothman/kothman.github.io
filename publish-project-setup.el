;;; package --- Summary: Custom OX-HTML Publisher -*- lexical-binding: t -*-
;;; Commentary:

;;; This file loads the modified ox-html org publisher, to make publishing
;;; org docs as HTML much easier and prettier.

;;; Org-Mode Export - Publish
(require 'ox-publish)
(require 'ox-html)
;;; Code:

;;; Define what directory name should be looked for during post-save hook
(defvar directory-name "static-streamline"
  "The directory name this project resides in, to be used by helper.
Scripts in determining if the saved-buffer should trigger recompilation.")

;;; Define our navigation directories to display
(defvar my-org-index-nav)
(setq my-org-index-nav
  '(("home" . "/")
    ("resources" . "/resources/")
    ("services" . "/services/")
    ("blog" . "/blog/")
    ("contact" . "/contact/")))

(defvar base-url nil
  "The base url value to be used when building links.
I'm not sure if org-mode's relative path building works for <head> html.")
(defvar export-for-dev nil
  "Nil unless the project is being exported for development.")
(setq export-for-dev nil)
(if export-for-dev
    (setq base-url "file:///home/kothman/code/static-streamline/public")
  (setq base-url "https://kothman.github.io"))


;;; Helper functions for publishing the project
(defun my-org-publish ()
  "My function for publishing org projects as HTML."
  (interactive)
  (org-publish-project "org"))
;;; Force publish, shouldn't need to use this
(defun my-org-publish-force ()
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
;; @todo change this to only add-hook if not in list
(add-hook 'after-save-hook 'my-org-after-save-hook)


;;; Define some kbd shortcuts for publishing the project
(keymap-global-set "C-c o P" 'my-org-publish-force)
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
      (setq formatted-item
	    (format my-org-nav-item-template
		    (car list-item) (cdr list-item) (car list-item)))
      (setq inside-html
	    (concat inside-html formatted-item)))
    (setq my-org-nav-html (format my-org-nav-template inside-html)))
  )

;;; Build the html sections that should go on each page
(my-org-build-navigation-html my-org-index-nav)


;;;;; Redefine ox-html functions for better control

;;;;;; Redefine org-html-format-list-item
(defun org-html-format-list-item (contents type checkbox info
					   &optional term-counter-id
					   headline)
  "Format a list item into HTML.
CONTENTS is the item contents.  TYPE is one of symbols `ordered',
`unordered', or `descriptive'.  CHECKBOX checkbox type is nil or one of
symbols `on', `off', or `trans'.   INFO is the info plist."
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

;;;;;; Set org export variables
;;;;;;
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;;; Don't export the Table of Contents
(setq org-export-with-toc nil)
;;; Don't export drawers
(setq org-export-with-drawers nil)
;;; Don't export section numbers
(setq org-export-with-section-numbers nil)
;;; Mark broken links
(setq org-export-with-broken-links 'mark)
;;; Remove the standard title
(setq org-export-with-title nil)

;;;;;;
;;;;;; Set HTML-specific export variables
;;;;;;
;;; Replace the title custom preamble
(setq org-html-preamble t)
;;; Define the preamble format for each language here
(setq org-html-preamble-format
      `(("en" ,my-org-nav-html)))
;;; Customize Postamble
(setq org-html-postamble nil)
;;; Include stylesheet in the head
(setq org-html-head (concat "<link rel=\"stylesheet\" href=\"" base-url "/styles.css?busted=" (format-time-string "%s") "\">"))
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css\">")
(defvar org-html-head-develop nil "The development stylesheet headers to use when testing locally.")
(setq org-html-head-develop (concat
		     "<link rel=\"stylesheet\" href=\"/styles.css\">\n"
		     "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css\">"))

;;; No JavaScript for now
(setq org-html-head-include-scripts nil)
;;; Set our own styles in org-html-head per spec
(setq org-html-head-include-default-style nil)
;;; Not using InfoJS
(setq org-html-use-infojs nil)
;;; Scripts are also set in the org-html-head, if needed
(setq org-html-scripts nil)

;;;;;;
;;;;;; Setup org projects for each export type
;;;;;;
(setq org-publish-project-alist
      '(
	;;; Org Notes exports .org files to HTML5
	("org-notes"
	 :base-directory "~/code/static-streamline/src/"
	 :base-extension "org"
	 :publishing-directory "~/code/static-streamline/public/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t)
	("org-static"
	 :base-directory "~/code/static-streamline/src/"
	 :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif"
	 :publishing-directory "~/code/static-streamline/public/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("org" :components ("org-notes" "org-static"))
	))

(my-org-publish-force)
