;;; seam-export.el --- Seam HTML exporter  -*- lexical-binding: t -*-

;; Copyright (c) 2025 Spencer Williams

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
(require 'seam-html)

(defvar seam-export--types nil)
(defvar seam-export--template nil)
(defvar seam-export--root-path nil)
(defvar seam-export--no-extension nil)
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

   The HTML template file to be used by the exporter.  If this is
   missing, falls back to :template-string, `seam-export-template-file',
   or `seam-export-template-string' in that order.

  `:template-string'

   The HTML template string to be used by the exporter.  If this is
   missing, falls back to :template-file, `seam-export-template-file',
   or `seam-export-template-string' in that order.

  `:root-path'

   The root path used for rendering internal links.  Defaults to \"\",
   which means all paths are relative.

  `:no-extension'

   Whether to drop the \".html\" file extension in links.  Defaults to
   nil.

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
<h1>{{title}}</h1>
<p class=\"modified\">Last modified: <time datetime=\"{{modified-dt}}\">{{modified}}</time></p>
</header>
{{contents}}
<section class=\"backlinks\">
<h1>Backlinks</h1>
{{backlinks}}
</section>
</main>
</body>
</html>"
  "The default HTML template string if no other template is specified.

It should be plain HTML5. Several variables are defined which can be
interpolated using the {{variable}} syntax:

- contents: The full HTML contents of the note, sans the title header.

- title: The note's title (HTML-escaped).

- backlinks: A list (<ul>) of notes that link to the given note.

- modified: The human-readable date that the note was last modified.
  See `seam-export-time-format'.

- modified-dt: The machine-readable date that the note was last
  modified. See `seam-export-time-format-datetime'.")

(defcustom seam-export-template-string seam-export-default-template-string
  "The HTML template string to be used by the exporter.  The template
format is documented at `seam-export-default-template-string'. 

See `seam-export-alist' for more information about specifying templates."
  :group 'seam-export
  :type '(choice string (const nil)))

(defcustom seam-export-time-format "%e %B %Y"
  "Human-readable format for template time strings.  Passed to
`format-time-string'."
  :group 'seam-export
  :type 'string)

(defcustom seam-export-time-format-datetime "%Y-%m-%d"
  "Machine-readable format for template time strings.  Meant to be used in
the datetime attribute of <time>.  Passed to `format-time-string'."
  :group 'seam-export
  :type 'string)

(defcustom seam-export-time-zone t
  "Time zone used for template time strings.  Passed to
`format-time-string'."
  :group 'seam-export
  :type 'sexp)

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

(defmacro seam-export--to-string (&rest body)
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

;;; Some HACK-ery to get fully escaped and smartquote-ized string.
(defun seam-export--escape-string (s)
  (string-remove-prefix
   "<p>\n"
   (string-remove-suffix
    "</p>\n"
    (seam-export--to-string
      (insert s)))))

(defun seam-export--replace-variable (var replacement)
  (goto-char 1)
  (while (re-search-forward (format "{{%s}}" var) nil t)
    (replace-match replacement)))

(defun seam-export--generate-backlinks (file)
  (seam-export--to-string
    (let ((files (sort
                  (let ((seam--subset seam-export--types))
                    (cl-loop for x in (seam-get-links-to-file file)
                             collect (cons (seam-get-title-from-file x) x)))
                  :key #'car
                  :lessp #'string<)))
      (when files
        (cl-loop for (title . file) in files
                 do (insert (format "- [[seam:%s][%s]]\n" (file-name-base file) title)))))))

(defun seam-export--note-to-html (note-file html-directory)
  (seam-ensure-directory-exists html-directory)
  (let ((html-file (file-name-concat html-directory
                                     (concat (file-name-base note-file) ".html")))
        (modified (file-attribute-modification-time
                   (file-attributes note-file))))
    (with-temp-buffer
      (insert seam-export--template)
      (seam-export--replace-variable
       "title"
       (seam-export--escape-string
        (seam-get-title-from-file note-file)))
      (seam-export--replace-variable
       "modified"
       (format-time-string
        seam-export-time-format
        modified
        seam-export-time-zone))
      (seam-export--replace-variable
       "modified-dt"
       (format-time-string
        seam-export-time-format-datetime
        modified
        seam-export-time-zone))
      (seam-export--replace-variable
       "contents"
       (seam-export--to-string
         (insert-file-contents note-file)
         (re-search-forward (org-headline-re 1))
         (org-mode)                    ;Needed for `org-set-property'.
         (org-set-property "seam-title-p" "t")))
      (seam-export--replace-variable
       "backlinks"
       (seam-export--generate-backlinks note-file))
      (write-file html-file))))

(defun seam-export--file-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun seam-export-note (file)
  (let ((type (seam-get-note-type file)))
    (cl-loop for (dir . plist) in seam-export-alist
             do
             (let ((types (plist-get plist :types))
                   (root-path (plist-get plist :root-path))
                   (no-extension (plist-get plist :no-extension))
                   (template-file (plist-get plist :template-file))
                   (template-string (plist-get plist :template-string))
                   (backend-options (plist-get plist :backend-options)))
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
                 (when (member type types)
                   (let ((seam-export--types types)
                         (seam-export--root-path (or root-path ""))
                         (seam-export--no-extension no-extension)
                         (seam-export--template template)
                         (seam-export--options (org-combine-plists
                                                seam-export-backend-options
                                                backend-options)))
                     (seam-export--note-to-html file dir))))))))

(defun seam-export-all-notes ()
  "Export all note files as HTML."
  (interactive)
  (unless seam-export-alist
    (error "Nothing to export.  Please configure `seam-export-alist'."))
  (dolist (dir (seam-note-subdirectories))
    (dolist (file (directory-files dir t seam-note-file-regexp))
      (seam-export-note file))))

(provide 'seam-export)

;;; seam-export.el ends here
