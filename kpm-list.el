
;; kpm-list: An emacs buffer list that tries to intelligently group together buffers.
;;
;; Copyright 2011 Kevin Mahoney. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without modification, are
;; permitted provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright notice, this list of
;;       conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright notice, this list
;;       of conditions and the following disclaimer in the documentation and/or other materials
;;       provided with the distribution.

(require 'cl)
(require 'dired)

;;; ------------------------------------------------------------------
;;; Functions responsible for sorting buffers into their groups.

(defun buffer-info (buffer)
  "Collect list info for a buffer"
  (with-current-buffer buffer
    (list (buffer-name)
          (and (buffer-file-name) (file-name-directory (buffer-file-name)))
          (and (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
          (symbol-name major-mode)
          (buffer-modified-p)
          (member buffer (latest-buffers)))))

(defun all-buffers ()
  "All buffers suitable for listing"
  (remove-if
   (lambda (b)
     (or
      (string= (substring (buffer-name b) 0 1) " ")
      (string= (buffer-name b) kpm-list-buffer-name)))
   (buffer-list)))

(defun latest-buffers ()
  "Customisable list of last used buffers"
  (take kpm-list-highlight-most-recent (all-buffers)))

(defun file-buffers ()
  (remove-if-not 'buffer-file-name (all-buffers)))

(defun non-file-buffers ()
  (remove-if 'buffer-file-name (all-buffers)))

(defun filter-by-mode (buffers mode)
  (remove-if-not '(lambda (b) (string= mode (nth 3 b))) buffers))

;; buffer sorting
(defun sort-by-nth (buffers n)
  (sort buffers (lambda (a b) (string< (nth n a) (nth n b)))))
(defun sort-by-name      (buffers) (sort-by-nth buffers 0))
(defun sort-by-dir       (buffers) (sort-by-nth buffers 1))
(defun sort-by-file-name (buffers) (sort-by-nth buffers 2))
(defun sort-by-mode      (buffers) (sort-by-nth buffers 3))

(defun unique-modes (buffers)
  "A list of unique modes in buffer list"
  (sort (delete-dups (mapcar '(lambda (b) (nth 3 b)) buffers)) 'string<))

(defun is-prefix (prefix string)
  "is `prefix` the prefix of `string`"
  (and
   (<= (length prefix) (length string))
   (string= prefix (substring string 0 (length prefix)))))

(defun is-buffer-subdir (parent subdir)
  "True if subdir is a subdirectory of parent"
  (is-prefix (nth 1 parent) (nth 1 subdir)))

(defun buffer-path-difference (parent subdir)
  "Return the difference in buffer's paths as (same-part . new-part)"
  (let ((split (length (nth 1 parent)))
        (path (nth 1 subdir)))
    (cons
     (substring path 0 split)
     (substring path split))))

(defun merge-singles (groups)
  "Collect all the groups with a length of 1 into their own group"
  (remove-if-not 'identity
                 (append
                  (remove-if-not '(lambda (g) (> (length g) 1)) groups)
                  (list (apply 'append (remove-if '(lambda (g) (> (length g) 1)) groups))))))

(defun group-by-prefix (buffers &optional groups)
  "Group buffers if they are a subdirectory of the parent & add relative path to info."
  (if buffers
      (if (and groups (caar groups) (is-buffer-subdir (caar groups) (car buffers)))

          ;; append to current group
          (let* ((head-buffer (caar groups))
                 (tail-buffer (car (last (car groups))))
                 (buffer (car buffers))
                 (relative-path (buffer-path-difference
                                 (if (is-buffer-subdir tail-buffer buffer) tail-buffer head-buffer)
                                 buffer))
                 (new-buffer (append (car buffers) (list relative-path))))
            (group-by-prefix (cdr buffers)
                             (cons (append (car groups) (list new-buffer))
                                   (cdr groups))))

        ;; create new group
        (let ((new-buffer (append (car buffers) (list (cons "" (nth 1 (car buffers)))))))
          (group-by-prefix (cdr buffers)
                           (cons (list new-buffer) groups))))

    groups))

(defun get-kpm-list-buffers ()
  "Return a list of file buffers as a list of buffer groups for each mode."
  (let ((buffers (mapcar 'buffer-info (file-buffers))))
    (mapcar '(lambda (mode)
               (cons mode
                     (merge-singles
                      (group-by-prefix
                       (sort-by-dir
                        (sort-by-file-name
                         (filter-by-mode buffers mode)))))))
            (unique-modes buffers))))

(defun get-non-file-kpm-list-buffers ()
  "Return a list of non-file buffers as a list of buffers grouped by mode."
  (let ((buffers (mapcar 'buffer-info (non-file-buffers))))
    (merge-singles
     (mapcar '(lambda (mode) (sort-by-name (filter-by-mode buffers mode)))
             (unique-modes buffers)))))


;;; ------------------------------------------------------------------
;;; Functions responsible for presenting the list in a buffer.

(defun pad-to-column (col)
  (while (< (current-column) col) (insert " ")))

(defun add-line-properties (properties)
  (add-text-properties (point-at-bol) (point-at-eol) properties))

(defun insert-dir (dir &optional face)
  (insert (propertize dir
                      'face (or face 'kpm-list-directory-face)
                      'mouse-face 'highlight
                      'dir-link t)))

(defun insert-buffer-line (buffer)
  (destructuring-bind (name dir filename mode modified highlight relative) buffer
    (insert (propertize (if modified "* " "  ") 'face 'kpm-list-modified-face))
    (insert (propertize filename 'face (if highlight 'kpm-list-buffer-highlight-face 'kpm-list-buffer-face)))
    (insert (propertize " "  'display '(space . (:align-to 40))))
    (add-line-properties (list 'mouse-face 'highlight))

    (insert " ")
    (if kpm-list-highlight-relative
        (progn
          (insert-dir (car relative) 'kpm-list-old-path-face)
          (insert-dir (cdr relative)))
      (insert-dir dir))

    (add-line-properties (list 'buffer-name name 'dir-name dir))
    (insert "\n")))

(defun insert-non-file-buffer-line (buffer)
  (destructuring-bind (name dir filename mode modified highlight) buffer
    (insert "  ")
    (insert (propertize name 'face (if highlight 'kpm-list-buffer-highlight-face 'kpm-list-buffer-face)))
    (insert (propertize " "  'display '(space . (:align-to 40))))
    (insert " ")
    (insert (propertize mode 'face 'kpm-list-mode-face))
    (add-line-properties (list 'buffer-name name 'mouse-face 'highlight))
    (insert "\n")))

(defun header (title)
  (insert "\n")
  (insert "  " title (propertize " "  'display '(space . (:align-to right))))
  (add-line-properties '(face kpm-list-header-face))
  (insert "\n\n"))

(defun make-kpm-list-buffer ()
  (with-current-buffer (get-buffer-create kpm-list-buffer-name)
    (let ((buffer-read-only nil))
      (erase-buffer)

      ;; modes
      (dolist (mode (get-kpm-list-buffers))
        (header (car mode))
        ;; groups
        (dolist (group (cdr mode))
          ;; buffers
          (dolist (buffer group)
            (insert-buffer-line buffer))
          (insert "\n")))

      ;; non-file
      (header "Other Buffers")
      (dolist (mode-group (get-non-file-kpm-list-buffers))
        (dolist (buffer mode-group)
          (insert-non-file-buffer-line buffer))
        (insert "\n")))

    (kpm-list-mode)))


;;; Util -------------------------------------------------------------

(defun take (n list)
  (if (and list (> n 0)) (cons (car list) (take (- n 1) (cdr list)))))

(defun buffer-at-point ()
  (get-text-property (point) 'buffer-name))

(defun buffer-point ()
  (+ 2 (point-at-bol)))

(defun dir-at-point ()
  (get-text-property (point) 'dir-name))

(defun first-line-p ()
  (= (point-at-bol) (point-min)))

(defun last-line-p ()
  (= (point-at-eol) (point-max)))

(defun first-buffer ()
  (beginning-of-buffer)
  (kpm-list-next-buffer))

(defun goto-buffer (buffer-name)
  (if buffer-name
      (progn
        (end-of-buffer)
        (while (and (not (string= buffer-name (buffer-at-point))) (not (first-line-p)))
          (forward-line -1))
        (if (buffer-at-point)
            (setf (point) (buffer-point))
          (first-buffer)))
    (first-buffer)))

(defun next-buffer-point ()
  (save-excursion
    (forward-line)
    (while (and (not (buffer-at-point)) (not (last-line-p)))
      (forward-line))
    (and (buffer-at-point) (buffer-point))))

(defun prev-buffer-point ()
  (save-excursion
    (forward-line -1)
    (while (and (not (buffer-at-point)) (not (first-line-p)))
      (forward-line -1))
    (and (buffer-at-point) (buffer-point))))

;;; Commands ---------------------------------------------------------

(defun is-directory-link ()
  (get-text-property (point) 'dir-link))

(defun kpm-list-select-buffer ()
  (interactive)
  (if (and (dir-at-point) (is-directory-link))
      (dired (dir-at-point))
    (when (buffer-at-point) (switch-to-buffer (buffer-at-point)))))

(defun kpm-list-select-other-window ()
  (interactive)
  (if (and (dir-at-point) (is-directory-link))
      (dired-other-window (dir-at-point))
    (when (buffer-at-point) (switch-to-buffer-other-window (buffer-at-point)))))

(defun kpm-list-select-dir ()
  (interactive)
  (when (dir-at-point) (dired (dir-at-point))))

(defun kpm-list-refresh ()
  (interactive)
  (let ((buffer (or (buffer-at-point) (buffer-name (car (all-buffers))))))
    (make-kpm-list-buffer)
    (goto-buffer buffer)))

(defun kpm-list-kill-buffer ()
  (interactive)
  (let ((buffer-name (buffer-at-point)))
    (when buffer-name
      (kpm-list-next-buffer)
      (kill-buffer buffer-name)
      (kpm-list-refresh))))

(defun kpm-list-prev-buffer ()
  (interactive)
  (let ((p (prev-buffer-point)))
    (when p (setf (point) p))))

(defun kpm-list-next-buffer ()
  (interactive)
  (let ((p (next-buffer-point)))
    (when p (setf (point) p))))

(defun kpm-list ()
  (interactive)
  (make-kpm-list-buffer)
  (switch-to-buffer kpm-list-buffer-name)
  (goto-buffer (buffer-name (car (all-buffers)))))

;;; Options ----------------------------------------------------------

(defgroup kpm-list ()
  "A list of open buffers."
  :group 'tools
  :group 'convenience)

(defcustom kpm-list-highlight-relative t
  "Non-nil means to highlight changing subdirectories."
  :type 'boolean
  :group 'kpm-list)

(defcustom kpm-list-buffer-name "*Grouped Buffer List*"
  "Buffer name to use."
  :type 'string
  :group 'kpm-list)

(defcustom kpm-list-highlight-most-recent 1
  "Highlight N most recently used buffers."
  :type 'number
  :group 'kpm-list)

;;; Faces ------------------------------------------------------------

(defface kpm-list-directory-face
  '((t (:foreground "LightSkyBlue" :inherit dired-directory)))
  "*Face used for directories in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-old-path-face
  '((t (:foreground "#28A" :inherit kpm-list-directory-face)))
  "*Face used for directories in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-buffer-face
  '((t (:inherit default)))
  "*Face used for buffers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-buffer-highlight-face
  '((t (:foreground "#9E9" :inherit default)))
  "*Face used for buffers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-header-face
  '((t (:foreground "White" :background "#420" :inherit default)))
  "*Face used for headers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-mode-face
  '((t (:foreground "Orange" :inherit default)))
  "*Face used for modes in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-modified-face
  '((t (:foreground "Red" :inherit default)))
  "*Face used for modified indicator in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

;;; Keymap -----------------------------------------------------------

(defvar kpm-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "<RET>") 'kpm-list-select-buffer)
    (define-key map (kbd "<mouse-1>") 'kpm-list-select-buffer)
    (define-key map "o" 'kpm-list-select-other-window)
    (define-key map "d" 'kpm-list-select-dir)
    (define-key map "g" 'kpm-list-refresh)
    (define-key map "k" 'kpm-list-kill-buffer)
    (define-key map "p" 'kpm-list-prev-buffer)
    (define-key map "n" 'kpm-list-next-buffer)
    map)
  "Keymap for buffer list.")

;;; Mode -------------------------------------------------------------

(define-derived-mode kpm-list-mode special-mode "Grouped Buffer List"
  "Major mode for editing a list of open buffers.")


;;; Misc. ------------------------------------------------------------

(global-set-key (kbd "C-x C-b") 'kpm-list)

(provide 'kpm-list)
