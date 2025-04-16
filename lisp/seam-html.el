;;; seam-html.el --- Seam HTML exporter  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Spencer Williams
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.

;; Author: Spencer Williams <spnw@plexwave.org>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Seam's HTML exporter.
;;
;; This was blithely hacked together using large chunks of code lifted
;; straight from ox-html.el, and could do with much improvement.
;;
;; The original authors of ox-html are:
;;     Carsten Dominik <carsten.dominik@gmail.com>
;;     Jambunathan K <kjambunathan at gmail dot com>

;;; Code:

(require 'ox-html)

;;; Org <9.7 compatibility.

(fset 'seam-html--element-parent-element
 (if (fboundp 'org-element-parent-element)
     'org-element-parent-element
   'org-export-get-parent-element))

(fset 'seam-html--element-parent
 (if (fboundp 'org-element-parent)
     'org-element-parent
   (lambda (node)
     (org-element-property :parent node))))

(fset 'seam-html--element-type-p
      (if (fboundp 'org-element-type-p)
          'org-element-type-p
        (lambda (node types)
          (memq (org-element-type node)
                (ensure-list types)))))

;;; NOTE: This function does not respect `:headline-levels' or
;;; `:html-self-link-headlines'.
(defun seam-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (cond
   ((org-element-property :SEAM-TITLE-P headline)
    contents)
   ((org-element-property :footnote-section-p headline)
    nil)
   (t
    (let* ((level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (text (org-export-data (org-element-property :title headline) info))
           (contents (or contents ""))
	         (id (org-html--reference headline info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info)))
      (let ((headline-class
	           (org-element-property :HTML_HEADLINE_CLASS headline)))
        (format "%s%s\n"
                (format "<h%d id=\"%s\"%s>%s</h%d>\n"
                        level
                        id
			                  (if (not headline-class) ""
			                    (format " class=\"%s\"" headline-class))
                        full-text
                        level)
                contents))))))

(defun seam-html-section (_section contents _info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section."
  contents)

(defvar seam-html-standalone-image-predicate)
(defun seam-html-standalone-image-p (element info)
  "Non-nil if ELEMENT is a standalone image.

INFO is a plist holding contextual information.

An element or object is a standalone image when

  - its type is `paragraph' and its sole content, save for white
    spaces, is a link that qualifies as an inline image;

  - its type is `link' and its containing paragraph has no other
    content save white spaces.

Bind `seam-html-standalone-image-predicate' to constrain paragraph
further.  For example, to check for only captioned standalone
images, set it to:

  (lambda (paragraph) (org-element-property :caption paragraph))"
  (let ((paragraph (pcase (org-element-type element)
		     (`paragraph element)
		     (`link (seam-html--element-parent element)))))
    (and (seam-html--element-type-p paragraph 'paragraph)
	 (or (not (and (boundp 'seam-html-standalone-image-predicate)
                     (fboundp seam-html-standalone-image-predicate)))
	     (funcall seam-html-standalone-image-predicate paragraph))
	 (catch 'exit
	   (let ((link-count 0))
	     (org-element-map (org-element-contents paragraph)
		 (cons 'plain-text org-element-all-objects)
	       (lambda (obj)
		 (when (pcase (org-element-type obj)
			 (`plain-text (org-string-nw-p obj))
			 (`link (or (> (cl-incf link-count) 1)
				    (not (org-html-inline-image-p obj info))))
			 (_ t))
		   (throw 'exit nil)))
	       info nil 'link)
	     (= link-count 1))))))

(defun seam-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
	       (dot (when (> (length html-ext) 0) "."))
	       (link-org-files-as-html-maybe
	        (lambda (raw-path info)
	          ;; Treat links to `file.org' as links to `file.html', if
	          ;; needed.  See `org-html-link-org-files-as-html'.
            (save-match-data
	            (cond
	             ((and (plist-get info :html-link-org-files-as-html)
                     (let ((case-fold-search t))
                       (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
	              (concat (match-string 1 raw-path) dot html-ext))
	             (t raw-path)))))
	       (link-type (org-element-property :type link))
	       (raw-path (org-element-property :path link))
	       ;; Ensure DESC really exists, or set it to nil.
	       (desc (org-string-nw-p desc))
         (path
	        (cond
           ((string= "seam" link-type)
            (let ((slug raw-path))
              (when-let ((file (seam-lookup-slug slug)))
                (let ((type (seam-get-note-type file)))
                  (when (and (member type seam-export--types)
                             (file-exists-p (seam-make-file-name slug type)))
                    (concat seam-export--root-path
                            slug
                            (if seam-export--no-extension "" ".html")))))))
	         ((string= "file" link-type)
	          ;; During publishing, turn absolute file names belonging
	          ;; to base directory into relative file names.  Otherwise,
	          ;; append "file" protocol to absolute file name.
	          (setq raw-path
		              (org-export-file-uri
		               (org-publish-file-relative-name raw-path info)))
	          ;; Possibly append `:html-link-home' to relative file
	          ;; name.
	          (let ((home (and (plist-get info :html-link-home)
			                       (org-trim (plist-get info :html-link-home)))))
	            (when (and home
			                   (plist-get info :html-link-use-abs-url)
			                   (not (file-name-absolute-p raw-path)))
		            (setq raw-path (concat (file-name-as-directory home) raw-path))))
	          ;; Maybe turn ".org" into ".html".
	          (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	          ;; Add search option, if any.  A search option can be
	          ;; relative to a custom-id, a headline title, a name or
	          ;; a target.
	          (let ((option (org-element-property :search-option link)))
	            (if (not option) raw-path
		            (let ((path (org-element-property :path link)))
		              (concat raw-path
			                    "#"
			                    (org-publish-resolve-external-link option path t))))))
	         (t (url-encode-url (concat link-type ":" raw-path)))))
	       (attributes-plist
	        (org-combine-plists
	         ;; Extract attributes from parent's paragraph.  HACK: Only
	         ;; do this for the first link in parent (inner image link
	         ;; for inline images).  This is needed as long as
	         ;; attributes cannot be set on a per link basis.
	         (let* ((parent (seam-html--element-parent-element link))
		              (link (let ((container (seam-html--element-parent link)))
			                    (if (and (seam-html--element-type-p container 'link)
				                           (org-html-inline-image-p link info))
			                        container
			                      link))))
	           (and (eq link (org-element-map parent 'link #'identity info t))
		              (org-export-read-attribute :attr_html parent)))
           ;; Add Seam internal link class if appropriate.
           (when (and seam-export--internal-link-class (string= "seam" link-type))
             (list :class seam-export--internal-link-class))
	         ;; Also add attributes from link itself.  Currently, those
	         ;; need to be added programmatically before `org-html-link'
	         ;; is invoked, for example, by backends building upon HTML
	         ;; export.
	         (org-export-read-attribute :attr_html link)))
	       (attributes
	        (let ((attr (org-html--make-attribute-string attributes-plist)))
	          (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	         (org-export-inline-image-p
	          link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= link-type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	      (if (not destination) desc
	        (format "<a href=\"#%s\"%s>%s</a>"
		              (org-export-get-reference destination info)
		              attributes
		              desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member link-type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= link-type "fuzzy")
			                       (org-export-resolve-fuzzy-link link info)
			                     (org-export-resolve-id-link link info))))
	      (pcase (org-element-type destination)
	        ;; ID link points to an external file.
	        (`plain-text
	         (let ((fragment (concat org-html--id-attr-prefix raw-path))
		             ;; Treat links to ".org" files as ".html", if needed.
		             (path (funcall link-org-files-as-html-maybe
				                        destination info)))
	           (format "<a href=\"%s#%s\"%s>%s</a>"
		                 path fragment attributes (or desc destination))))
	        ;; Fuzzy link points nowhere.
	        (`nil
	         (;; format "<i>%s</i>"
            identity
		        (or desc
		            (org-export-data
			           (org-element-property :raw-link link) info))))
	        ;; Link points to a headline.
	        (`headline
	         (let ((href (org-html--reference destination info))
		             ;; What description to use?
		             (desc
		              ;; Case 1: Headline is numbered and LINK has no
		              ;; description.  Display section number.
		              (if (and (org-export-numbered-headline-p destination info)
			                     (not desc))
		                  (mapconcat #'number-to-string
				                         (org-export-get-headline-number
				                          destination info) ".")
		                ;; Case 2: Either the headline is un-numbered or
		                ;; LINK has a custom description.  Display LINK's
		                ;; description or headline's title.
		                (or desc
			                  (org-export-data
			                   (org-element-property :title destination) info)))))
	           (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
	        ;; Fuzzy link points to a target or an element.
	        (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (seam-html--element-type-p destination 'latex-environment)
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
	             ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (seam-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (seam-html--element-type-p destination 'latex-environment)
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
		                 (cond
		                  (desc nil)
		                  ((seam-html-standalone-image-p destination info)
		                   (org-export-get-ordinal
			                  (org-element-map destination 'link #'identity info t)
			                  info '(link) 'seam-html-standalone-image-p))
		                  (t (org-export-get-ordinal
			                    destination info nil counter-predicate))))
                    (desc
		                 (cond (desc)
			                     ((not number) "No description for this link")
			                     ((numberp number) (number-to-string number))
			                     (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= link-type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text raw-path))))
	      (format "<a href=\"#%s\" %s%s>%s</a>"
		            fragment
		            (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			                  fragment fragment)
		            attributes
		            (format (org-export-get-coderef-format raw-path desc)
			                  (org-export-resolve-coderef raw-path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
	            (org-html-encode-plain-text path)
	            attributes
	            desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
	      (format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.
     (t desc))))


(defun seam-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	         (code (org-html-format-code src-block info))
	         (label (let ((lbl (org-html--reference src-block info t)))
		                (if lbl (format " id=\"%s\"" lbl) ""))))
      (format "<pre>%s%s</pre>"
	            ;; Build caption.
	            (let ((caption (org-export-get-caption src-block)))
		            (if (not caption) ""
		              (let ((listing-number
			                   (format
			                    "<span class=\"listing-number\">%s </span>"
			                    (format
			                     (org-html--translate "Listing %d:" info)
			                     (org-export-get-ordinal
			                      src-block info nil #'org-html--has-caption-p)))))
		                (format "<label class=\"org-src-name\">%s%s</label>"
			                      listing-number
			                      (org-trim (org-export-data caption info))))))
	            ;; Contents.
	            (format "<code class=\"src src-%s\"%s>%s</code>"
                      ;; Lang being nil is OK.
                      lang label code)))))

(org-export-define-derived-backend
    'seam
    'html
  :translate-alist
  `((headline . seam-html-headline)
    (link . seam-html-link)
    (section . seam-html-section)
    (src-block . seam-html-src-block)))

(provide 'seam-html)

;;; seam-html.el ends here
