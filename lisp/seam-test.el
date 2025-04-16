;;; seam-test.el --- Tests for Seam  -*- lexical-binding: t -*-

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

;; Tests for Seam.

;;; Code:

;;; FIXME: Tests can fail if certain buffer names are already in use
;;; on the test runner's system.

(require 'seam)
(require 'seam-export)
(require 'ert)
(require 'cl-lib)

(defvar seam-test-directory nil)

(defmacro seam-test-environment (&rest body)
  (declare (indent 0))
  `(let* ((seam-test-directory (file-name-as-directory (make-temp-file "seam-test" t)))
          (seam-note-directory seam-test-directory)
          (default-directory seam-test-directory)
          (seam-note-types '("private" "public"))
          (seam-default-note-type "private")
          (seam-title-formatter (lambda (title _type) title))
          (seam-export-template-file nil)
          (seam-export-template-string seam-export-default-template-string)
          (seam-export-internal-link-class nil)
          (seam-export-alist
           `((,(file-name-concat seam-test-directory "html")
              :types ("public")
              :root-path "/")))
          ;; Manually install hooks in test directory.
          (dir-locals-directory-cache
           (cons (list seam-test-directory 'seam-note-directory nil)
                 dir-locals-directory-cache)))
     (unwind-protect
         (progn ,@body)
       (delete-directory seam-test-directory t))))

(defmacro seam-test-with-notes (options varlist &rest body)
  (declare (indent 2))
  `(seam-test-environment
     (let ,options
       (let ,(cl-loop for (name . args) in varlist
                      collect `(,name (seam-make-note ,@args)))
         ;; FIXME: It's quite possible for tests to fail in such a way
         ;; that this does not kill the buffers.
         (unwind-protect (progn ,@body)
           (mapcar #'kill-buffer (list ,@(mapcar #'car varlist))))))))

(defun seam-test-strip-testdir (filename)
  (string-remove-prefix seam-test-directory filename))

(defun seam-test-list-files ()
  (mapcar
   #'seam-test-strip-testdir
   (directory-files-recursively seam-test-directory "")))

(defun seam-test-add-contents (buffer contents)
  (with-current-buffer buffer
    (insert contents)
    (insert "\n")
    (save-buffer)))

(defun seam-test-replace-contents (buffer contents)
  (with-current-buffer buffer
    (erase-buffer)
    (insert contents)
    (insert "\n")
    (save-buffer)))

(defun seam-test-link-to-buffer (buffer)
  (format "[[seam:%s]]" (file-name-base (buffer-file-name buffer))))

(defun seam-test-links-from-html (file)
  (with-temp-buffer
    (insert-file-contents file)
    (delete-dups
     (cl-loop for ret = (re-search-forward "<a href=\"/\\(.*\\)?\">" nil t)
              while ret collect (match-string 1)))))

(ert-deftest seam-test-make-note-private ()
  (should
   (equal
    '("private/note.org")
    (seam-test-with-notes ()
        ((note "Note"))
      (seam-test-list-files)))))

(ert-deftest seam-test-make-note-public ()
  (should
   (equal
    '("html/note.html" "public/note.org")
    (seam-test-with-notes ()
        ((note "Note" "public"))
      (seam-test-list-files)))))

(ert-deftest seam-test-make-note-weird-filename ()
  (should
   (equal
    '("./Weird file name!" ("private/weird-file-name.org"))
    (seam-test-with-notes ()
        ((weird "./Weird file name! "))
      (list (buffer-name weird)
            (seam-test-list-files))))))

(ert-deftest seam-test-get-title-from-buffer ()
  (seam-test-with-notes () ((note "Note"))
    (should
     (equal
      (buffer-name note)
      (seam-get-title-from-buffer note)))))

(ert-deftest seam-test-get-title-from-buffer-narrowed ()
  (should
   (equal
    "foo"
    (seam-test-with-notes ()
        ((foo "foo"))
      (with-current-buffer foo
        (seam-test-add-contents foo "* My headline")
        (forward-line)
        (org-narrow-to-subtree)
        (seam-get-title-from-buffer))))))

(ert-deftest seam-test-get-title-from-file ()
  (seam-test-with-notes () ((note "Note"))
    (should
     (equal
      (buffer-name note)
      (seam-get-title-from-file (buffer-file-name note))))))

(ert-deftest seam-test-make-note-invalid-type ()
  (should-error
   (seam-test-environment
     (kill-buffer (seam-make-note "Note" "invalid-type")))))

(ert-deftest seam-test-make-note-name-conflict ()
  (should-error
   (seam-test-environment
     (kill-buffer (seam-make-note " Note 1 "))
     (kill-buffer (seam-make-note "Note_1")))))

(ert-deftest seam-test-make-note-name-conflict-different-types ()
  (should-error
   (seam-test-environment
     (kill-buffer (seam-make-note "Note"))
     (kill-buffer (seam-make-note "Note" "public")))))

(ert-deftest seam-test-rename-note ()
  (should
   (equal
    '("New name" ("private/new-name.org"))
    (seam-test-with-notes () ((note "Note"))
      (with-current-buffer note
        (erase-buffer)
        (insert "* New name\n")
        (save-buffer)
        (list (buffer-name) (seam-test-list-files)))))))

(ert-deftest seam-test-rename-conflict ()
  (should-error
   (seam-test-with-notes ()
       (let ((note1 "Note 1")
             (note2 "Note 2"))
         (with-current-buffer note2
           (erase-buffer)
           (insert "* note_1!\n")
           (unwind-protect
               (save-buffer)
             (set-buffer-modified-p nil)))))))

(ert-deftest seam-test-buffer-name-format-default ()
  (should
   (equal
    "Note (private)"
    (seam-test-with-notes ((seam-title-formatter #'seam-format-title-default))
        ((note "Note"))
      (buffer-name note)))))

(ert-deftest seam-test-buffer-name-format-custom ()
  (should
   (equal
    "[private] Note"
    (seam-test-with-notes ((seam-title-formatter
                            (lambda (title type) (format "[%s] %s" type title))))
        ((note "Note"))
      (buffer-name note)))))

(ert-deftest seam-test-link-update ()
  "Test that renaming a note updates its HTML and that of notes which link to it."
  (should
   (equal '(("qux.html")
            ("public/qux.org")
            ("html/foo.html" "html/qux.html" "public/foo.org" "public/qux.org"))
          (seam-test-with-notes ()
              ((foo "foo" "public")
               (bar "bar" "public"))
            (progn
              (seam-test-add-contents foo (seam-test-link-to-buffer bar))
              (seam-test-replace-contents bar "* qux")
              (list
               (seam-test-links-from-html "html/foo.html")
               (mapcar #'seam-test-strip-testdir (seam-get-links-from-file (buffer-file-name foo)))
               (seam-test-list-files)))))))

(ert-deftest seam-test-link-update-no-unnecessary-export ()
  "Test that updating the contents of a note does not unnecessarily
re-export note to which it links."
  (should-not
   (member
    "html/bar.html"
    (seam-test-with-notes ()
        ((foo "foo" "public")
         (bar "bar" "public"))
      (seam-test-add-contents foo (seam-test-link-to-buffer bar))
      (delete-file "html/bar.html")
      (seam-test-add-contents foo "hello")
      (seam-test-list-files)))))

(ert-deftest seam-test-link-to-private ()
  "Test that a private link does not get exported in HTML."
  (should-error
   (seam-test-with-notes ()
       ((foo "foo" "public")
        (bar "bar"))
     (seam-test-add-contents foo (seam-test-link-to-buffer bar))
     (with-temp-buffer
       (insert-file-contents "html/foo.html")
       (buffer-string)
       (re-search-forward "<a href=\"/bar.html\">")))))

(ert-deftest seam-test-link-no-extension ()
  "Test that the :no-extension option causes links to render without .html
extension."
  (should
   (identity
    (seam-test-with-notes ((seam-export-alist
                            `((,(file-name-concat seam-test-directory "html")
                               :types ("public")
                               :root-path "/"
                               :no-extension t))))
        ((foo "foo" "public")
         (bar "bar" "public"))
      (seam-test-add-contents foo (seam-test-link-to-buffer bar))
      (with-temp-buffer
        (insert-file-contents "html/foo.html")
        (buffer-string)
        (re-search-forward "<a href=\"/bar\">"))))))

(ert-deftest seam-test-link-internal-class ()
  "Test that setting `seam-export-internal-link-class' correctly renders
the class."
  (should
   (identity
    (seam-test-with-notes ((seam-export-internal-link-class "internal"))
        ((foo "foo" "public")
         (bar "bar" "public"))
      (seam-test-add-contents foo (seam-test-link-to-buffer bar))
      (with-temp-buffer
        (insert-file-contents "html/foo.html")
        (buffer-string)
        (re-search-forward "<a href=\"/bar.html\" class=\"internal\">"))))))

(ert-deftest seam-test-link-getters ()
  (should
   (equal
    '(("private/bar.org" "private/qux.org")
      ("private/foo.org")
      ("private/bar.org" "private/foo.org"))
    (seam-test-with-notes ()
        ((foo "foo")
         (bar "bar")
         (qux "qux"))
      (seam-test-add-contents foo (seam-test-link-to-buffer bar))
      (seam-test-add-contents foo (seam-test-link-to-buffer qux))
      (seam-test-add-contents bar (seam-test-link-to-buffer qux))
      (list
       (mapcar #'seam-test-strip-testdir (seam-get-links-from-file (buffer-file-name foo)))
       (mapcar #'seam-test-strip-testdir (seam-get-links-to-file (buffer-file-name bar)))
       (mapcar #'seam-test-strip-testdir (seam-get-links-to-file (buffer-file-name qux))))))))

(ert-deftest seam-test-delete-note ()
  "Test that deleting a note also deletes its HTML and re-exports linking
notes such that they no longer link to it."
  (should
   (equal
    '(nil ("html/foo.html" "public/foo.org"))
    (seam-test-with-notes ()
        ((foo "foo" "public")
         (bar "bar" "public"))
      (with-current-buffer foo
        (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
      (let ((delete-by-moving-to-trash nil))
        (seam-delete-note (buffer-file-name bar)))
      (list
       (seam-test-links-from-html "html/foo.html")
       (seam-test-list-files))))))

(ert-deftest seam-test-backlinks-public ()
  "Test that linking to a note from a public note creates a backlink."
  (should
   (identity
    (seam-test-with-notes ((seam-export-template-string "{{backlinks}}"))
        ((foo "foo" "public")
         (bar "bar" "public"))
      (with-current-buffer foo
        (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
      (with-temp-buffer
        (insert-file-contents "html/bar.html")
        (re-search-forward "<a href=\"/foo.html\">"))))))

(ert-deftest seam-test-backlinks-private ()
  "Test that linking to a note from a private note does not create a
backlink."
  (should-error
   (seam-test-with-notes ((seam-export-template-string "{{backlinks}}"))
       ((foo "foo")
        (bar "bar" "public"))
     (with-current-buffer foo
       (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
     (with-temp-buffer
       (insert-file-contents "html/bar.html")
       (re-search-forward "<a href=\"/foo.html\">")))))

(ert-deftest seam-test-backlinks-delete ()
  "Test that deleting a note removes backlink."
  (should-error
   (seam-test-with-notes ((seam-export-template-string "{{backlinks}}"))
       ((foo "foo" "public")
        (bar "bar" "public"))
     (with-current-buffer foo
       (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
     (let ((delete-by-moving-to-trash nil))
       (seam-delete-note (buffer-file-name foo)))
     (with-temp-buffer
       (insert-file-contents "html/bar.html")
       (re-search-forward "<a href=\"/foo.html\">")))))

(ert-deftest seam-test-set-type-private ()
  "Test that setting a public note to private will delete its HTML file and
update linking HTML files such that they no longer link to it."
  (should
   (equal
    '(nil ("html/foo.html" "private/bar.org" "public/foo.org"))
    (seam-test-with-notes ()
        ((foo "foo" "public")
         (bar "bar" "public"))
      (with-current-buffer foo
        (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
      (seam-set-note-type (buffer-file-name bar) "private")
      (list
       (seam-test-links-from-html "html/foo.html")
       (seam-test-list-files))))))

(ert-deftest seam-test-set-type-public ()
  "Test that setting a private note to public will export its HTML file and
update linking HTML files such that they link to it."
  (should
   (equal
    '(("bar.html")
      ("html/bar.html" "html/foo.html" "public/bar.org" "public/foo.org"))
    (seam-test-with-notes ()
        ((foo "foo" "public")
         (bar "bar"))
      (with-current-buffer foo
        (seam-test-add-contents foo (seam-test-link-to-buffer bar)))
      (seam-set-note-type (buffer-file-name bar) "public")
      (list
       (seam-test-links-from-html "html/foo.html")
       (seam-test-list-files))))))

(ert-deftest seam-test-set-type-invalid ()
  "Test that setting a note to an invalid type raises an error."
  (should-error
   (seam-test-with-notes ()
       ((foo "foo"))
     (seam-set-note-type (buffer-file-name foo) "invalid-type"))))

(ert-deftest seam-test-follow-link-existing ()
  "Test that following a link to an existing note opens that note."
  (should
   (equal
    "bar"
    (seam-test-with-notes ()
        ((foo "foo")
         (bar "bar"))
      (with-current-buffer foo
        (seam-test-add-contents foo "[[seam:bar]]")
        (org-previous-link)
        (org-open-at-point)
        (buffer-name))))))

(ert-deftest seam-test-follow-link-new ()
  "Test that following a link to an nonexistent note creates and opens that note."
  (should
   (equal
    '("bar" ("private/bar.org" "private/foo.org"))
    (seam-test-with-notes ()
        ((foo "foo"))
      (with-current-buffer foo
        (seam-test-add-contents foo "[[seam:bar]]")
        (goto-char 1)
        (org-next-link)
        (org-open-at-point)
        (unwind-protect
            (list
             (buffer-name)
             (seam-test-list-files))
          (kill-buffer)))))))

(ert-deftest seam-test-escape-title ()
  (should
   (equal
    "&ldquo;quotes&rdquo; &amp; &lt;symbols&gt;\n"
    (seam-test-with-notes ((seam-export-template-string "{{title}}"))
        ((note "\"quotes\" & <symbols>" "public"))
      (seam-export--file-string "html/quotes-symbols.html")))))

(ert-deftest seam-test-custom-slug ()
  "Test that setting the SEAM_SLUG property saves and exports accordingly."
  (should
   (equal
    '("html/c-vs-cpp.html" "public/c-vs-cpp.org")
    (seam-test-with-notes ()
        ((note "C vs C++" "public"))
      (seam-test-add-contents note ":PROPERTIES:\n:SEAM_SLUG: c-vs-cpp\n:END:")
      (seam-test-list-files)))))

(provide 'seam-test)

;;; seam-test.el ends here
