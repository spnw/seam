;;; seam-export.el --- Seam HTML exporter  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Spencer Williams

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

;; Frontend for Seam's HTML exporter.

;;; Code:

(require 'cl-lib)
(require 'mustache)
(require 'seam-html)

(defvar seam-export--types nil)
(defvar seam-export--template nil)
(defvar seam-export--template-values nil)
(defvar seam-export--root-path nil)
(defvar seam-export--include-drafts nil)
(defvar seam-export--no-extension nil)
(defvar seam-export--time-format nil)
(defvar seam-export--time-format-dt nil)
(defvar seam-export--time-zone nil)
(defvar seam-export--internal-link-class nil)
(defvar seam-export--options nil)

(defgroup seam-export nil
  "Options for Seam exporter."
  :tag "Seam Export"
  :group 'seam)

(defcustom seam-export-alist nil
  "Association list used by Seam to determine how to export notes.

The car of each element is an HTML directory to which Seam will export a
subset of notes.  The cdr is a plist containing any number of these
properties:

  `:types'

    List of note types to export to this directory.  Required.

  `:template-file'

    The HTML template file to be used by the exporter.  If this
    is missing, falls back to :template-string,
    `seam-export-template-file', or `seam-export-template-string'
    in that order.

  `:template-string'

    The HTML template string to be used by the exporter.  If this
    is missing, falls back to :template-file,
    `seam-export-template-file', or `seam-export-template-string'
    in that order.

  `:template-values'

    An alist of template variables and their values.  Values
    specified here will take precedence over those in
    `seam-export-template-values'.  Defaults to nil.

  `:root-path'

    The root path used for rendering internal links.  Defaults to \"\",
    which means all paths are relative.

  `:include-drafts'

    Whether to export draft notes as well.  Defaults to nil.

  `:no-extension'

    Whether to drop the \".html\" file extension in links.  Defaults to
    nil.

  `:time-format'

    Human-readable format for template time strings.  Defaults to
    the value of `seam-export-time-format'.

  `:time-format-dt'

    Machine-readable format for template time strings.  Defaults
    to the value of `seam-export-time-format-dt'.

  `:time-zone'

    Time zone used for template time strings.  Defaults to the
    value of `seam-export-time-zone'.

  `:internal-link-class'

    CSS class name for internal links.  Defaults to the value of
    `seam-export-internal-link-class'.

  `:backend-options'

    A plist of extra options passed to the Org HTML backend.  This can be
    used to override any of the defaults set in
    `seam-export-backend-options'."
  :group 'seam-export
  :type '(alist :key-type string :value-type plist))

(defcustom seam-export-template-file nil
  "The HTML template file to be used by the exporter.  The template format
is documented at `seam-export-default-template-string'.

See `seam-export-alist' for more information about specifying templates."
  :group 'seam-export
  :type '(choice file (const nil)))

(defvar seam-export-default-template-string
  "<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
<title>{{title}}</title>
</head>
<body>
<main>
<header>
<h1>{{{raw-title}}}</h1>
<p class=\"modified\">Last modified: <time datetime=\"{{modified-dt}}\">{{modified}}</time></p>
</header>
{{{contents}}}
<section class=\"backlinks\">
<h1>Backlinks</h1>
{{{backlinks}}}
</section>
</main>
</body>
</html>"
  "The default HTML template string if no other template is specified.

It should be plain HTML5.  Several variables are defined which
can be interpolated using Mustache bracket syntax.  {{variable}}
will HTML-escape the interpolated text, while {{{variable}}} will
interpolate it as-is.

  `contents'

   The full HTML contents of the note, sans the title header.

  `title'

   The note's title, in a plain text format suitable for a
   <title> tag.

  `raw-title'

   The note's title, in an HTML format suitable for an <h1> tag.

  `backlinks'

   A list (<ul>) of notes that link to the given note.

  `created'

   The human-readable date that the note was created.  See
   `seam-export-time-format'.

  `created-dt'

   The machine-readable date that the note was created.  See
   `seam-export-time-format-dt'.

  `modified'

   The human-readable date that the note was last modified.  See
   `seam-export-time-format'.

  `modified-dt'

   The machine-readable date that the note was last modified.
   See `seam-export-time-format-dt'.")

(defcustom seam-export-template-string seam-export-default-template-string
  "The HTML template string to be used by the exporter.  The template
format is documented at `seam-export-default-template-string'. 

See `seam-export-alist' for more information about specifying templates."
  :group 'seam-export
  :type '(choice string (const nil)))

(defcustom seam-export-template-values nil
  "An alist of (VAR . VALUE) pairs, where VAR is a string naming a
template variable, and VALUE is the value to be used when
interpolating that variable.  See the mustache.el docs for more
information."
  :group 'seam-export
  :type '(alist :key-type string :value-type sexp))

(defcustom seam-export-time-format "%e %B %Y"
  "Human-readable format for template time strings.  Passed to
`format-time-string'."
  :group 'seam-export
  :type 'string)

(defcustom seam-export-time-format-dt "%Y-%m-%d"
  "Machine-readable format for template time strings.  Meant to be used in
the datetime attribute of <time>.  Passed to `format-time-string'."
  :group 'seam-export
  :type 'string)

(defcustom seam-export-time-zone t
  "Time zone used for template time strings.  Passed to
`format-time-string'."
  :group 'seam-export
  :type 'sexp)

(defcustom seam-export-internal-link-class nil
  "CSS class name to use for internal links (i.e., links to other Seam
notes)."
  :group 'seam-export
  :type 'string)

(defvar seam-export-backend-options
  (list
   :html-container "article"
   :html-doctype "html5"
   :html-html5-fancy t
   :html-text-markup-alist
   '((bold . "<strong>%s</strong>")
     (code . "<code>%s</code>")
     (italic . "<em>%s</em>")
     (strike-through . "<s>%s</s>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))
   :html-toplevel-hlevel 1
   :html-use-infojs nil
   :section-numbers nil
   :time-stamp-file nil
   :with-smart-quotes t
   :with-toc nil))

(defmacro seam-export--export-to-html-string (&rest body)
  (declare (indent 0))
  (let ((buf (gensym)))
    `(let ((,buf (generate-new-buffer " *seam-export*")))
       (unwind-protect
           (progn (with-temp-buffer
                    ,@body
                    ;; This let prevents Org from popping up a window.
                    (let ((org-export-show-temporary-export-buffer nil))
                      (org-export-to-buffer 'seam ,buf nil nil nil t seam-export--options nil)))
                  (with-current-buffer ,buf
                    (buffer-string)))
         (kill-buffer ,buf)))))

(defmacro seam-export--export-to-text-string (&rest body)
  (declare (indent 0))
  (let ((buf (gensym)))
    `(let ((,buf (generate-new-buffer " *seam-export*")))
       (unwind-protect
           (progn (with-temp-buffer
                    ,@body
                    ;; This let prevents Org from popping up a window.
                    (let ((org-export-show-temporary-export-buffer nil)
                          (org-ascii-charset 'utf-8))
                      (org-export-to-buffer 'ascii ,buf nil nil nil t seam-export--options nil)))
                  (with-current-buffer ,buf
                    (buffer-string)))
         (kill-buffer ,buf)))))

(defun seam-export--org-to-html (s)
  "Convert single-line Org string to HTML via Org exporter."
  (string-remove-prefix
   "<p>\n"
   (string-remove-suffix
    "</p>\n"
    (seam-export--export-to-html-string
      (insert s)))))

(defun seam-export--org-to-text (s)
  "Convert single-line Org string to plain text via Org exporter."
  (string-chop-newline
   (seam-export--export-to-text-string
     (insert s))))

(defun seam-export--get-props (file props)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward "^\\* " nil t)
      (org-mode)
      (cl-loop for prop in props
               collect (org-element-property prop (org-element-at-point))))))

(defun seam-export--generate-backlinks (file)
  (seam-export--export-to-html-string
    (let ((files (cl-sort
                  (let ((seam--subset seam-export--types))
                    (cl-loop for x in (seam-get-links-to-file file)
                             collect (cons (seam-get-title-from-file x) x)))
                  #'string<
                  :key #'car)))
      (when files
        (cl-loop for (title . file) in files
                 do (insert (format "- [[seam:%s][%s]]\n" (file-name-base file) title)))))))

(defun seam-export--note-to-html (note-file html-directory)
  (seam-ensure-directory-exists html-directory)
  (cl-destructuring-bind (created-prop modified-prop)
      (seam-export--get-props note-file '(:SEAM_CREATED :SEAM_MODIFIED))
    (let* ((html-file (file-name-concat html-directory
                                        (concat (seam-get-slug-from-file-name note-file) ".html")))
           (modified
            (or (ignore-errors (parse-iso8601-time-string modified-prop))
                (file-attribute-modification-time
                 (file-attributes note-file))))
           (created
            (or (ignore-errors (parse-iso8601-time-string created-prop))
                modified)))
      (with-temp-buffer
        (insert
         (mustache-render
          seam-export--template
          (append
           seam-export--template-values
           seam-export-template-values
           `(("title" .
              ,(seam-export--org-to-text
                (seam-get-title-from-file note-file)))
             ("raw-title" .
              ,(seam-export--org-to-html
                (seam-get-title-from-file note-file)))
             ("created" .
              ,(format-time-string
                seam-export--time-format
                created
                seam-export--time-zone))
             ("created-dt" .
              ,(format-time-string
                seam-export--time-format-dt
                created
                seam-export--time-zone))
             ("modified" .
              ,(format-time-string
                seam-export--time-format
                modified
                seam-export--time-zone))
             ("modified-dt" .
              ,(format-time-string
                seam-export--time-format-dt
                modified
                seam-export--time-zone))
             ("modified?" .
              ,(lambda (template context)
                 (unless (equal created modified)
                   (mustache-render template context))))
             ("contents" .
              ,(seam-export--export-to-html-string
                 (insert-file-contents note-file)
                 (re-search-forward "^\\* ")
                 (org-mode)            ;Needed for `org-set-property'.
                 (org-set-property "seam-title-p" "t")))
             ("backlinks" .
              ,(seam-export--generate-backlinks note-file)))
           nil)))
        (write-file html-file)))))

(defun seam-export--file-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun seam-export-note (file)
  (let ((type (seam-get-note-type file))
        (draft-p (seam-draft-p file)))
    (cl-loop for (dir . plist) in seam-export-alist
             do
             (let ((types (plist-get plist :types))
                   (template-file (plist-get plist :template-file))
                   (template-string (plist-get plist :template-string))
                   (template-values (plist-get plist :template-values)))
               (unless types
                 (error "You must specify :types for export"))
               (let ((template
                      (cond
                       (template-file (seam-export--file-string template-file))
                       (template-string template-string)
                       (seam-export-template-file (seam-export--file-string
                                                   seam-export-template-file))
                       (seam-export-template-string seam-export-template-string)
                       (t (error "You must specify a template for export (see `seam-export-alist')")))))
                 (when (and (member type types)
                            (or (not (seam-draft-p file))
                                (plist-get plist :include-drafts)))
                   (let ((seam-export--types types)
                         (seam-export--root-path (or (plist-get plist :root-path) ""))
                         (seam-export--include-drafts (plist-get plist :include-drafts))
                         (seam-export--no-extension (plist-get plist :no-extension))
                         (seam-export--template template)
                         (seam-export--template-values template-values)
                         (seam-export--time-format
                          (or (plist-get plist :time-format)
                              seam-export-time-format))
                         (seam-export--time-format-dt
                          (or (plist-get plist :time-format-dt)
                              seam-export-time-format-dt))
                         (seam-export--time-zone
                          (or (plist-get plist :time-zone)
                              seam-export-time-zone))
                         (seam-export--internal-link-class
                          (or (plist-get plist :internal-link-class)
                              seam-export-internal-link-class))
                         (seam-export--options (org-combine-plists
                                                seam-export-backend-options
                                                (plist-get plist :backend-options))))
                     (seam-export--note-to-html file dir))))))))

(defun seam-export-all-notes ()
  "Export all note files as HTML."
  (interactive)
  (unless seam-export-alist
    (error "Nothing to export.  Please configure `seam-export-alist'."))
  (dolist (dir (seam-note-subdirectories))
    (dolist (file (directory-files dir t seam-note-file-regexp))
      (seam-delete-html-files-for-note file)
      (seam-export-note file))))

(provide 'seam-export)

;;; seam-export.el ends here
