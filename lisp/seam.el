;;; seam.el --- Personal Org mode wiki  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Spencer Williams

;; Author: Spencer Williams <spnw@plexwave.org>
;; Homepage: https://wiki.plexwave.org/seam
;; Keywords: hypermedia, outlines

;; Version: 0.1.0

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

;; Seam is a package that lets you easily create, link, and publish
;; Org notes in the form of a personal wiki.

;;; Code:

(require 'seam-export)
(require 'org)
(require 'cl-lib)
(require 'grep)

(defgroup seam nil
  "Options for Seam."
  :group 'org
  :tag "Seam")

(defcustom seam-note-directory nil
  "Seam note directory."
  :group 'seam
  :type '(choice directory (const nil)))

(defcustom seam-default-note-type "private"
  "Default type for Seam notes."
  :group 'seam
  :type 'string)

(defcustom seam-note-types '("private" "public")
  "List of valid Seam note types.  Each element can either be a
string (the name of the type), or an alist.  If using an alist,
the car should be the type name, and the cdr should be a plist
containing any number of these properties:

  `:create-as-draft'

   When this is non-nil, new Seam notes of this type will be
   created as drafts.  If this is missing, falls back to
   `seam-create-as-draft'."
  :group 'seam
  :type '(repeat
          (choice string
                  (alist :key-type string :value-type plist))))

(defcustom seam-create-as-draft nil
  "When non-nil, new Seam notes will be created as drafts."
  :group 'seam
  :type 'boolean)

(defun seam-format-title-default (title type draft-p)
  "Default Seam title formatter.  Formats like this: \"TITLE (TYPE[ draft])\"."
  (format "%s %s"
          title
          (propertize
           (format "(%s%s)" type (if draft-p " draft" ""))
           'face 'font-lock-comment-face)))

(defcustom seam-title-formatter
  #'seam-format-title-default
  "Function used by Seam to format note titles for completion and buffer
naming.  Must be a function taking two arguments: TITLE and TYPE."
  :group 'seam
  :type 'function)

(defun seam-html-directories ()
  (mapcar #'car seam-export-alist))

(defun seam-slugify (title)
  (setq title (string-replace "'" "" title))
  (setq title (string-split title "\\W+" t))
  (setq title (string-join title "-"))
  (downcase title))

(defun seam-lookup-slug (slug)
  (cl-dolist (type (seam-get-all-note-type-names))
    (let ((file (file-name-concat seam-note-directory type (concat slug ".org")))
          (draft-file (file-name-concat seam-note-directory type (concat "-" slug ".org"))))
      (cond
       ((file-exists-p file)
        (cl-return (expand-file-name file)))
       ((file-exists-p draft-file)
        (cl-return (expand-file-name draft-file)))))))

(defun seam--check-conflict (slug)
  (when (seam-lookup-slug slug)
    (error "A note called `%s.org' already exists" slug)))

(defun seam-link-open (path _prefix)
  (org-mark-ring-push)
  (if-let ((file (seam-lookup-slug path)))
      (find-file file)
    (seam-create-note path nil t (seam-draft-p path)))
  (seam-set-buffer-name))

(defvar seam-note-file-regexp "\\`[^.].+\\.org\\'")
(defvar seam--subset nil)

(defcustom seam-completing-read-function #'completing-read
  "The completion function used by Seam."
  :group 'seam
  :type 'function)

(defun seam-ensure-directory-exists (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun seam-ensure-note-subdirectories-exist ()
  (unless seam-note-directory
    (error "Please set `seam-note-directory'"))
  (cl-dolist (type (seam-get-all-note-type-names))
    (let ((dir (file-name-concat seam-note-directory type)))
      (seam-ensure-directory-exists dir))))

(defcustom seam-sort-method 'title
  "The method used by Seam to sort notes."
  :group 'seam
  :type '(choice (const :tag "Sort by title" title)
                 (const :tag "Sort by modification date" modified)))

(defun seam-get-all-note-type-names ()
  (mapcar (lambda (x) (car (ensure-list x))) seam-note-types))

(cl-defgeneric seam-get-all-notes (sort-by))

(cl-defmethod seam-get-all-notes ((sort-by (eql 't)))
  (ignore sort-by)
  (seam-get-all-notes seam-sort-method))

(cl-defmethod seam-get-all-notes ((sort-by (eql 'modified)))
  (ignore sort-by)
  (let ((files (cl-loop for type in (seam--active-subset)
                        append (directory-files-and-attributes
                                (file-name-concat seam-note-directory type)
                                t
                                seam-note-file-regexp))))
    (cl-loop for (file . _attributes)
             in (cl-sort
                 files
                 (lambda (f1 f2)
                   (time-less-p (file-attribute-modification-time f2)
                                (file-attribute-modification-time f1)))
                 :key #'cdr)
             collect (cons (seam-get-title-from-file file) file))))

(cl-defmethod seam-get-all-notes ((sort-by (eql 'title)))
  (ignore sort-by)
  (let ((files (cl-loop for type in (seam--active-subset)
                        append (directory-files
                                (file-name-concat seam-note-directory type)
                                t
                                seam-note-file-regexp))))
    (cl-sort
     (cl-loop for file in files
              collect (cons (seam-get-title-from-file file) file))
     #'string<
     :key #'car)))

(cl-defun seam-get-title-from-buffer (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (save-mark-and-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (when (re-search-forward "^\\* " nil t)
          (let ((start (point)))
            (end-of-line)
            (let ((title (string-trim (buffer-substring-no-properties start (point)))))
              (unless (string-empty-p title)
                title))))))))

(defun seam-get-title-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (seam-get-title-from-buffer)))

(defun seam-get-slug-from-file-name (file)
  (string-remove-prefix "-" (file-name-base file)))

(cl-defun seam-get-slug-from-buffer (&optional (buffer (current-buffer)))
  (or (with-current-buffer buffer
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (goto-char 1)
            (when (re-search-forward "^\\* " nil t)
              (org-element-property :SEAM_SLUG (org-element-at-point))))))
      (seam-slugify (seam-get-title-from-buffer buffer))))

(defun seam-format-title (title type draft-p)
  (funcall seam-title-formatter title type draft-p))

(defun seam-validate-note-type (type)
  (unless (member type (seam-get-all-note-type-names))
    (error "`%s' is not a valid Seam note type" type)))

(cl-defun seam-create-note (title &optional type select (draft-p nil draft-supplied-p))
  (unless type
    (setq type seam-default-note-type))
  (seam-validate-note-type type)
  (seam-ensure-note-subdirectories-exist)
  (let* ((slug (seam-slugify title))
         (draft-p
          (if draft-supplied-p
              draft-p
            (cl-getf (cdr (assoc type (mapcar #'ensure-list seam-note-types)))
                     :create-as-draft
                     seam-create-as-draft)))
         (file (file-name-concat seam-note-directory
                                 type
                                 (concat (when draft-p "-") slug ".org"))))
    (when (string= "" slug)
      (error "Cannot create a note with an empty slug"))
    (seam--check-conflict slug)
    (let ((buffer (funcall (if select #'find-file #'find-file-noselect) file)))
      (with-current-buffer buffer
        (insert (format "* %s\n" title))
        (save-buffer)
        buffer))))

(defun seam-read-title (prompt)
  (seam-ensure-note-subdirectories-exist)
  (let* ((notes (seam-get-all-notes t))
         (self (cl-find (buffer-file-name) notes :key #'cdr :test #'equal)))
    (let ((notes
           (append (cl-remove self
                              notes
                              :test #'equal)
                   (and self (list self)))))
      (let ((files (cl-loop for (title . file) in notes
                            collect (cons (seam-format-title
                                           title
                                           (seam-get-note-type file)
                                           (seam-draft-p file))
                                          file))))
        (let ((completion (string-trim (funcall seam-completing-read-function prompt (mapcar #'car files)))))
          (or (assoc completion files)
              (cons completion nil)))))))

(defun seam--read-type (prompt arg &optional choices)
  (when arg
    (if (listp arg)
        (let ((type (funcall seam-completing-read-function
                             prompt
                             (or choices (seam-get-all-note-type-names))
                             nil
                             t)))
          (seam-validate-note-type type)
          type)
      (nth (1- arg)
           (seam-get-all-note-type-names)))))

;;;###autoload
(defun seam-find-note (arg)
  "Find Seam note interactively by title, creating it if it does not exist.
`seam-completing-read-function' is used for completion.

A prefix argument can be used to show only a specific note type (and to
use that type if a new note is created).  With a numeric argument N, the
Nth type in `seam-note-types' is chosen (counting from 1).  With C-u, a
completion prompt is given to choose the type."
  (interactive "P")
  (let* ((type (seam--read-type "Type: " arg))
         (seam--subset
          (if type (list type) (seam-get-all-note-type-names))))
    (cl-destructuring-bind (completion . file)
        (seam-read-title "Open note: ")
      (if file
          (with-current-buffer (find-file file)
            ;; Ensure buffer name is up to date (e.g. after changing
            ;; formatter function) (NOTE: Redundant if buffer wasn't
            ;; already open, as `seam-setup-buffer' does this too.)
            (seam-set-buffer-name))
        (seam-create-note (string-trim completion) (or type seam-default-note-type) t)))))

(cl-defun seam-get-note-type (file &optional no-error)
  (when (and file (equal "org" (file-name-extension file)))
    (let ((type (cadr (nreverse (file-name-split file)))))
      (when (member type (seam-get-all-note-type-names))
        (cl-return-from seam-get-note-type type))))
  (unless no-error
    (error "%s is not a Seam note" file)))

(defun seam-make-file-name (slug type &optional draft)
  (expand-file-name
   (file-name-concat
    seam-note-directory type
    (concat (when draft "-") slug ".org"))))

(defun seam-get-links-to-file (file &optional include-drafts)
  "Return filename of each note which links to FILE."
  (cl-loop for file in (remove (expand-file-name file)
                               (seam-note-files-containing-string
                                (format "[[seam:%s]" (file-name-base file))))
           when (or include-drafts
                    seam-export--include-drafts
                    (not (seam-draft-p file)))
           collect file))

(cl-defun seam-get-links-from-buffer (&optional (buffer (current-buffer)))
  "Return filename of each existing note which is linked to from BUFFER."
  (let ((links (with-current-buffer buffer
                 (save-mark-and-excursion
                   (save-restriction
                     (widen)
                     (goto-char 1)
                     (delete-dups
                      (cl-loop for ret = (re-search-forward "\\[\\[seam:\\(.*?\\)\\]" nil t)
                               while ret collect (match-string 1))))))))
    (let ((file (buffer-file-name buffer)))
      (remove (and file (expand-file-name file))
              (cl-loop for link in links
                       as f = (seam-lookup-slug link)
                       when f collect f)))))

(defun seam-get-links-from-file (file)
  "Return filename of each existing note which is linked to from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (seam-get-links-from-buffer)))

(defun seam-delete-html-files-for-note (note-file)
  (let ((html-nd (concat (seam-get-slug-from-file-name note-file) ".html")))
    (dolist (dir (seam-html-directories))
      (let ((html (file-name-concat dir html-nd)))
        (when (file-exists-p html)
          (delete-file html)
          (message "Deleted %s" html))))))

(defun seam--rename-file (old new interactive)
  (rename-file old new)
  (when interactive
    (set-visited-file-name new nil t)
    (seam-set-buffer-name))
  (seam-post-save-or-rename old new))

(defun seam-post-save-or-rename (old new &optional previous-links-from-file slug-or-title-changed)
  (unless (string= old new)
    (seam-update-links old new))
  (seam-delete-html-files-for-note old)
  (seam-export-note new)
  (let* ((current-links (seam-get-links-from-file new))
         (added-links (cl-set-difference current-links
                                         previous-links-from-file
                                         :test #'string=))
         (removed-links (cl-set-difference previous-links-from-file
                                           current-links
                                           :test #'string=)))
    (let ((type-changed
           (not (string= (seam-get-note-type old)
                         (seam-get-note-type new)))))
      (mapc #'seam-export-note
            (delete-dups
             (append
              removed-links

              ;; Backlinks sections must be updated when either
              ;; slug or title changes.
              (if slug-or-title-changed
                  current-links
                added-links)

              ;; `seam-update-links' inherently triggers
              ;; re-exporting of notes when links change.
              ;; However, note type is not encoded in the link,
              ;; so we must handle that case manually.
              (when type-changed
                (seam-get-links-to-file new))))))))

(defun seam-draft-p (file)
  (string-prefix-p "-" (file-name-base file)))

(defun seam-save-buffer ()
  (let* ((old (buffer-file-name))
         (type (seam-get-note-type old t))
         (draft-p (seam-draft-p old)))
    (when type
      (unless (seam-get-title-from-buffer)
        (error "Note must have a title"))
      (let* ((slug (seam-get-slug-from-buffer))
             (new (seam-make-file-name slug type draft-p))
             (newly-created-p (not (file-exists-p old)))
             (slug-changed-p (not (string= slug (file-name-base old))))
             (title-changed-p (unless newly-created-p
                                (not (string= (seam-get-title-from-buffer)
                                              (seam-get-title-from-file old))))))
        (unless (string= old new)       ;This is valid because
                                        ;`seam-save-buffer' cannot
                                        ;change type or draft status.
          (seam--check-conflict slug)
          (rename-file old new)
          (set-visited-file-name new nil t))
        (let ((previous-links-from-file
               ;; If we've yet to create the file, don't check it.
               (unless newly-created-p
                 (seam-get-links-from-file new))))
          (let ((write-contents-functions
                 (remove 'seam-save-buffer write-contents-functions)))
            (save-buffer))
          (seam-post-save-or-rename old
                                    new
                                    previous-links-from-file
                                    (or slug-changed-p title-changed-p))
          (seam-set-buffer-name)
          t)))))

(defun seam--set-note-type (file new-type interactive)
  (let ((old-type (seam-get-note-type file))
        (new-file (seam-make-file-name (file-name-base file) new-type)))
    (if (string= new-type old-type)
        file
      (seam--rename-file file new-file interactive)
      new-file)))

;;;###autoload
(defun seam-set-note-type (file new-type &optional interactive)
  "Set Seam note FILE to NEW-TYPE.  Error if file is not a Seam note.

When called interactively, FILE is the currently visited file.  A
numeric argument N chooses the Nth type in `seam-note-types' (counting
from 1).  Otherwise a completion prompt is given for the desired type."
  (interactive
   (let* ((file (buffer-file-name))
          (old-type (seam-get-note-type file)))
     (list file
           (or (seam--read-type "New type: "
                                ;; HACK: Treat nil prefix as C-u.
                                (or current-prefix-arg '(4))
                                (remove old-type (seam-get-all-note-type-names)))
               old-type)
           t)))
  (seam--set-note-type file new-type interactive))

;;;###autoload
(defun seam-toggle-draft (file &optional interactive)
  "Toggle the draft status of Seam note FILE."
  (interactive (list (buffer-file-name) t))
  (seam-get-note-type file)          ;Error if file isn't a Seam note.
  (let* ((base (file-name-nondirectory file))
         (new-file (file-name-concat
                    (file-name-directory file)
                    (if (string-prefix-p "-" base)
                        (string-remove-prefix "-" base)
                      (concat "-" base)))))
    (seam--rename-file file new-file interactive)))

(defun seam-update-links (old new)
  (let* ((old-link (file-name-base old))
         (new-link (file-name-base new)))
    (unless (string= old-link new-link)
      (let ((count (seam-replace-string-in-all-notes
                    (format "[[seam:%s]" old-link)
                    (format "[[seam:%s]" new-link)
                    t)))
        (unless (zerop count)
          (message "Updated links in %d file%s"
                   count (if (= count 1) "" "s")))))))

(defun seam--active-subset ()
  (or seam--subset (seam-get-all-note-type-names)))

(defun seam-note-subdirectories ()
  (cl-loop for type in (seam--active-subset)
           collect (expand-file-name
                    (file-name-as-directory
                     (file-name-concat seam-note-directory type)))))

(defun seam-note-files-containing-string (string)
  "Search all Seam note files for literal STRING.  Case-sensitive."
  (seam-ensure-note-subdirectories-exist)
  (with-temp-buffer
    (apply #'call-process find-program
           nil t nil
           (append
            (seam-note-subdirectories)
            (list "-type" "f" "-name" "*.org" "-and" "-not" "-name" ".*"
                  "-exec" grep-program "-F" "-l" "-s" "-e" string "{}" "+")))
    (string-lines (string-trim (buffer-string)) t)))

;;;###autoload
(defun seam-search (query &optional delimited)
  "Search all Seam notes for the regexp QUERY (case-insensitively).  If
DELIMITED is non-nil, only search at word boundaries.

When called interactively, DELIMITED is t if a prefix argument is given.
Otherwise, it's nil."
  (interactive (list (read-string (format "Search all notes%s: "
                                          (if current-prefix-arg
                                              " for word"
                                            "")))
                     current-prefix-arg))
  (when (eq grep-highlight-matches 'auto-detect)
    (grep-compute-defaults))
  (let ((default-directory seam-note-directory))
    (grep
     (format "%s %s -type f -name %s -and -not -name %s -exec %s %s -n -i -e %s \\{\\} \\+"
             find-program
             (string-join (mapcar (lambda (type)
                                    (shell-quote-argument (concat type "/")))
                                  (seam-get-all-note-type-names))
                          " ")
             (shell-quote-argument "*.org")
             (shell-quote-argument ".*")
             grep-program
             (if grep-highlight-matches "--color=always" "")
             (shell-quote-argument
              (if delimited
                  (concat "\\b" query "\\b")
                query))))))

(defun seam-visited-notes ()
  (let ((subdirs (seam-note-subdirectories)))
    (cl-loop for buf in (buffer-list)
             as file = (buffer-file-name buf)
             when (and file
                       (member (file-name-directory file) subdirs)
                       (string-match seam-note-file-regexp file))
             collect file)))

(defun seam-replace-string-in-all-notes (old new preserve-modtime)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (file (seam-note-files-containing-string old))
      (puthash file nil hash))
    (dolist (file (seam-visited-notes))
      (puthash file t hash))
    (let ((update-count 0))
      (maphash
       (lambda (file was-open-p)
         (with-current-buffer (find-file-noselect file)
           (let ((was-modified-p (buffer-modified-p)))
             (save-mark-and-excursion
               (without-restriction
                 (goto-char (point-min))
                 (let ((updated-p nil))
                   (while (search-forward old nil t)
                     (setq updated-p t)
                     (replace-match new))
                   (when updated-p
                     (setq update-count (1+ update-count))))))
             (when (and (not was-modified-p)
                        (buffer-modified-p))
               (if preserve-modtime
                   (let ((modtime (visited-file-modtime)))
                     (save-buffer)
                     (set-file-times file modtime)
                     (set-visited-file-modtime modtime))
                 (save-buffer)))
             (unless was-open-p
               (kill-buffer)))))
       hash)
      update-count)))

(cl-defun seam-set-buffer-name (&optional (buffer (current-buffer)))
  (when-let ((title (seam-get-title-from-buffer)))
    (let ((file (buffer-file-name buffer)))
      (with-current-buffer buffer
        (rename-buffer
         (seam-format-title title
                            (seam-get-note-type file)
                            (seam-draft-p file)))))))

(defun seam-setup-buffer ()
  "Setup hooks when loading a Seam file."
  (add-hook 'write-contents-functions 'seam-save-buffer nil t)
  ;; NOTE: Needed for when note w/o using Seam commands.  Redundant otherwise.
  (seam-set-buffer-name))

(defun seam--watch-note-directory-var (_symbol newval operation _where)
  "Install necessary hooks when `seam-note-directory' is set, removing any
old ones."
  (when (member operation '(set makunbound))
    (setq dir-locals-directory-cache
          (cl-remove 'seam-note-directory dir-locals-directory-cache :key #'cadr))
    (when newval
      (dir-locals-set-directory-class newval 'seam-note-directory))))

(defun seam--delete-note (file)
  (seam-get-note-type file)           ;Error if file isn't a Seam note.
  (let ((to-update (delete-dups
                    (append
                     (seam-get-links-to-file file)
                     (seam-get-links-from-file file)))))
    (delete-file file t)
    (seam-delete-html-files-for-note file)
    (mapc #'seam-export-note to-update)))

;;;###autoload
(defun seam-delete-note (file &optional interactive)
  "Delete Seam note FILE.  Error if file is not a Seam note.
`delete-by-moving-to-trash' is respected.

When called interactively, FILE is the currently visited file, and the
buffer is killed after deletion."
  (interactive
   (let ((file (buffer-file-name)))
     (seam-get-note-type file)        ;Error if file isn't a Seam note.
     (list
      (let ((incoming (length (seam-get-links-to-file file t))))
        (and (yes-or-no-p
              (format "Really %s `%s' and kill buffer%s?"
                      (if delete-by-moving-to-trash
                          "trash"
                        "delete")
                      (seam-get-title-from-buffer)
                      (if (> incoming 0)
                          (format " (breaking links from %d note%s)"
                                  incoming
                                  (if (= incoming 1) "" "s"))
                        "")))
             file))
      t)))
  (unless (and interactive (null file))
    (seam--delete-note file)
    (when interactive
      (kill-buffer))))

;;;###autoload
(defun seam-insert-link ()
  "Interactively insert an Org link at point to the given Seam note,
creating the note if it does not exist.  If any text is selected, the
link will replace it."
  (interactive)
  (cl-destructuring-bind (completion . file) (seam-read-title "Insert note link: ")
    (let* ((new-buffer
            (unless file
              (seam-create-note completion seam-default-note-type nil)))
           (selection (when (use-region-p)
                        (buffer-substring
                         (region-beginning)
                         (region-end))))
           (file (if new-buffer
                     (buffer-file-name new-buffer)
                   file))
           (slug (file-name-base file))
           (initial (or selection
                        (seam-get-title-from-file file)))
           (desc (read-string "Description: " initial)))
      (when selection
        (delete-region (region-beginning) (region-end)))
      (insert (format "[[seam:%s][%s]]" slug desc))
      (when new-buffer
        (pop-to-buffer new-buffer)))))

(defvar-keymap seam-prefix-map
  "f" #'seam-find-note
  "k" #'seam-delete-note
  "l" #'seam-insert-link
  "s" #'seam-search
  "t" #'seam-set-note-type
  "d" #'seam-toggle-draft)

(org-link-set-parameters "seam" :follow #'seam-link-open)

(dir-locals-set-class-variables
 'seam-note-directory
 '((org-mode . ((eval . (seam-setup-buffer))))))

(add-variable-watcher 'seam-note-directory #'seam--watch-note-directory-var)

;;; If `seam-note-directory' was set before loading package, ensure
;;; directory class is set up.
(when (and seam-note-directory
           (not (cl-find 'seam-note-directory dir-locals-directory-cache :key #'cadr)))
  (dir-locals-set-directory-class seam-note-directory 'seam-note-directory))

(provide 'seam)

;;; seam.el ends here
