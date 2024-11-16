;;; org-fc-embed.el --- Embed your Org flashcards into your notes, and update them on the fly -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Bohong Huang
;;
;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0.0
;; Homepage: https://github.com/bohonghuang/org-fc-embed
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package allows you to create and update Org flashcards inside
;;  your note Org files, sparing you the maintenance of external
;;  flashcards.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'org)
(require 'org-habit)
(require 'org-fc)

(defun org-fc-embed-string-hash (string)
  (cl-loop for char across (sha1 string nil nil t)
           for i from 6 downto 0
           sum (ash char (* i 8))))

(cl-declaim (special org-fc-embed-overlay-mode))

(cl-defun org-fc-embed-put-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  (cl-assert org-fc-embed-overlay-mode)
  (save-excursion
    (goto-char start)
    (while (re-search-forward (rx "@@fc:" (group (+? anychar)) "@@") end t)
      (let ((overlay (make-overlay
                      (match-beginning 0)
                      (match-end 0)
                      nil 'front-advance)))
        (overlay-put overlay 'category 'org-fc-cloze)
        (overlay-put overlay 'invisible nil)
        (overlay-put overlay 'display (match-string 1))))))

(cl-defun org-fc-embed-remove-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'org-fc-cloze))

(cl-defun org-fc-embed-put-meta-overlays (&optional (start (point-min)) (end (point-max)))
  (cl-assert org-fc-embed-overlay-mode)
  (save-excursion
    (goto-char start)
    (while (let ((case-fold-search t))
             (re-search-forward
              (rx (or (and (group-n 1 "@@comment:+fc_front:" (group (+? anychar)) "@@"))
                      (and bol (* blank) (group-n 1 "#+fc_front:" (+? anychar)) eol)))
              end t))
      (let ((overlay (make-overlay
                      (match-beginning 1)
                      (match-end 1)
                      nil 'front-advance)))
        (overlay-put overlay 'category 'org-fc-meta)
        (overlay-put overlay 'invisible nil)
        (overlay-put overlay 'display (propertize ":fc:" 'face 'org-habit-ready-face))))))

(cl-defun org-fc-embed-remove-meta-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'org-fc-meta))

(define-minor-mode org-fc-embed-overlay-mode
  "Minor mode for visualizing the status of embedded Org flashcards using overlays."
  :group 'org-fc-embed
  (cl-assert (eq major-mode 'org-mode))
  (if org-fc-embed-overlay-mode
      (progn
        (org-fc-embed-put-cloze-overlays)
        (org-fc-embed-put-meta-overlays))
    (org-fc-embed-remove-cloze-overlays)
    (org-fc-embed-remove-meta-overlays)))

;;;###autoload
(cl-defun org-fc-embed-cloze-dwim (&aux marker-start marker-end)
  (interactive)
  (save-excursion
    (setf marker-start (point-marker))
    (call-interactively #'org-fc-cloze-dwim)
    (re-search-forward (rx "}}") (pos-eol))
    (insert "@@")
    (setf marker-end (point-marker))
    (unless (looking-at (rx (or blank eol)) (pos-eol))
      (delete-horizontal-space)
      (insert " "))
    (or (re-search-backward (rx "}{") marker-start t) (re-search-backward (rx "}}") marker-start))
    (insert "@@fc:")
    (re-search-backward (rx "{{") (pos-bol))
    (unless (looking-back (rx (or blank bol)) (pos-bol))
      (delete-horizontal-space)
      (insert " "))
    (setf marker-start (point-marker))
    (insert "@@fc:")
    (re-search-forward (rx "{{") marker-end)
    (insert "@@")
    (when org-fc-embed-overlay-mode
      (org-fc-embed-put-cloze-overlays marker-start marker-end))))

(cl-defun org-fc-embed-fc-file-relative (&optional (org-fc-directory (cl-first org-fc-directories)))
  (let ((file (buffer-file-name (current-buffer))))
    (cl-assert file)
    (let ((relative (file-relative-name file org-directory)))
      (cl-assert (not (string-prefix-p ".." relative)))
      (let ((absolute (expand-file-name relative org-fc-directory)))
        (make-directory (file-name-directory absolute) t)
        absolute))))

(defcustom org-fc-embed-fc-file #'org-fc-embed-fc-file-relative
  "A variable that determines the file to which flashcards are exported."
  :group 'org-fc-embed
  :type 'function)

(defvar org-fc-embed-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-fc-embed-export-finalize)
    (define-key map "\C-c\C-k" #'org-fc-embed-export-kill)
    map)
  "Keymap for `org-fc-embed-export-mode', a minor mode.")

(define-minor-mode org-fc-embed-export-mode
  "Minor mode for special key bindings in an Org flashcard export buffer."
  :group 'org-fc-embed
  (cl-assert (eq major-mode 'org-mode))
  (if org-fc-embed-export-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-fc-embed-export-mode-map>Flashcard export buffer.  Finish `\\[org-fc-embed-export-finalize]', abort `\\[org-fc-embed-export-kill]'."))
    (setq-local header-line-format nil)))

(cl-defun org-fc-embed-remove-comments (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward (rx "@@comment:" (*? anychar) "@@" (* blank)) end t)
             do (replace-match "")
             sum 1)))

(cl-defun org-fc-embed-process-clozes (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop with regexp = (rx "@@fc:" (group (+? anychar)) "@@")
             with hashes = (make-hash-table)
             initially (goto-char start)
             for cloze-start = (if (not (re-search-forward regexp end t))
                                   (cl-return count)
                                 (cl-assert (string-equal (match-string 1) "{{"))
                                 (prog1 (save-excursion
                                          (goto-char (match-end 0))
                                          (point-marker))
                                   (replace-match "\\1")))
             for cloze-end = (if (not (re-search-forward regexp end t))
                                 (cl-assert nil)
                               (cl-assert (string-suffix-p "}}" (match-string 1)))
                               (prog1 (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point-marker))
                                 (replace-match "\\1")))
             for count from 0
             for cloze = (string-trim (buffer-substring-no-properties cloze-start cloze-end))
             for hash = (org-fc-embed-string-hash cloze)
             do
             (cl-assert (looking-back "}}" (pos-bol)))
             (backward-char 1)
             (cl-assert (null (gethash hash hashes)))
             (setf (gethash hash hashes) cloze)
             (insert (format "@%d" hash)))))

(defvar-local org-fc-embed-export-window-configuration nil)

(defcustom org-fc-embed-prepare-finalize-hook nil
  "Hook that is run before the finalization starts.
The flashcard export buffer is current and still narrowed."
  :group 'org-fc-embed
  :type 'hook)

(defun org-fc-embed-export-finish ()
  (cl-assert org-fc-embed-export-mode)
  (kill-local-variable 'org-fc-embed-prepare-finalize-hook)
  (widen)
  (org-fc-embed-export-mode -1)
  (set-window-configuration (cl-shiftf org-fc-embed-export-window-configuration nil)))

(defun org-fc-embed-export-finalize ()
  (interactive)
  (cl-assert org-fc-embed-export-mode)
  (run-hooks 'org-fc-embed-prepare-finalize-hook)
  (org-fc-embed-export-finish))

(defun org-fc-embed-export-kill ()
  (interactive)
  (cl-assert org-fc-embed-export-mode)
  (widen)
  (org-back-to-heading)
  (org-cut-subtree)
  (org-fc-embed-export-finish))

(cl-defgeneric org-fc-embed-export-flashcard (type props)
  (let* ((element (list type props))
         (buffer (current-buffer))
         (content (buffer-substring (org-element-begin element) (org-element-end element)))
         (window-configuration (current-window-configuration))
         (type 'org-fc-type-normal-init))
    (with-current-buffer (switch-to-buffer (find-file-noselect (funcall org-fc-embed-fc-file)))
      (setf org-fc-embed-export-window-configuration window-configuration)
      (goto-char (point-max))
      (insert "* ")
      (org-return-and-maybe-indent)
      (org-narrow-to-subtree)
      (save-excursion
        (insert content)
        (delete-blank-lines))
      (indent-region (point) (point-max))
      (when (cl-plusp (org-fc-embed-process-clozes (point)))
        (setf type #'org-fc-type-cloze-init))
      (org-back-to-heading)
      (org-end-of-line)
      (org-fc-embed-export-mode +1)
      (cl-assert (null org-fc-embed-prepare-finalize-hook))
      (cl-assert (not (cl-member 'org-fc-embed-prepare-finalize-hook (buffer-local-variables) :key #'car)))
      (add-hook
       'org-fc-embed-prepare-finalize-hook
       (letrec ((hook (lambda ()
                        (call-interactively type)
                        (let ((id (org-id-get))
                              (front (cl-fifth (org-heading-components))))
                          (cl-assert id) (cl-assert front)
                          (with-current-buffer buffer
                            (save-excursion
                              (goto-char (org-element-begin element))
                              (let ((start (point))
                                    (indentation (rx bol (* blank))))
                                (if (not (and (looking-back indentation (pos-bol)) (looking-at indentation)))
                                    (insert (format "@@comment:+FC_FRONT: [[id:%s][%s]]@@ " id front))
                                  (let ((indentation (match-string 0)))
                                    (open-line 1)
                                    (insert indentation)
                                    (insert (format "#+FC_FRONT: [[id:%s][%s]]" id front))))
                                (when org-fc-embed-overlay-mode
                                  (org-fc-embed-put-meta-overlays start (point)))))))
                        (remove-hook 'org-fc-embed-prepare-finalize-hook hook))))
         hook)
       nil t))))

(defun org-fc-embed-link-file-position ()
  (cl-assert (looking-at (rx "[" "[" (group (*? anychar)) "]" (*? anychar) "]")))
  (let ((link (match-string 1)))
    (cl-assert (string-prefix-p "id:" link))
    (let ((id (string-trim-left link (rx "id:"))))
      (cl-destructuring-bind (file . position) (org-id-find id)
        (cl-values file position)))))

(cl-defgeneric org-fc-embed-update-flashcard (type props)
  (let* ((element (list type props))
         (content (buffer-substring (org-element-begin element) (org-element-end element))))
    (cl-multiple-value-bind (file position) (org-fc-embed-link-file-position)
      (with-current-buffer (find-file-noselect file)
        (goto-char position)
        (save-restriction
          (org-narrow-to-subtree)
          (org-end-of-meta-data t)
          (delete-region (point) (point-max))
          (save-excursion (insert content))
          (indent-region (point) (point-max))
          (org-fc-embed-remove-comments (point))
          (when (cl-plusp (org-fc-embed-process-clozes (point)))
            (cl-assert (string-equal (org-entry-get nil "FC_TYPE") "cloze")))
          (goto-char (point-max)))
        (delete-blank-lines)
        (org-fc-update)))))

(defun org-fc-embed-goto-link-to-flashcard ()
  (let ((marker (point-marker))
        (current-end (org-element-end (org-element-at-point)))
        (child-start (or (ignore-errors
                           (org-down-element)
                           (org-element-begin (org-element-at-point)))
                         (point-max))))
    (cl-loop for element = (if (ignore-errors (org-backward-element) t)
                               (org-element-at-point)
                             (when fallback-start
                               (goto-char fallback-start))
                             (cl-return))
             for (element-start . element-end) = (cons (org-element-begin element) (org-element-end element))
             when (< element-end current-end) unless (org-at-keyword-p) return (goto-char element-end)
             maximize element-start into fallback-start)
    (or (let ((case-fold-search t))
          (re-search-forward (rx "+fc_front:" (* blank)) (min current-end child-start) t))
        (null (goto-char marker)))))

;;;###autoload
(defun org-fc-embed-open-flashcard ()
  (interactive)
  (cl-multiple-value-bind (file position)
      (save-excursion
        (cl-assert (org-fc-embed-goto-link-to-flashcard))
        (org-fc-embed-link-file-position))
    (find-file file)
    (goto-char position)))

;;;###autoload
(defun org-fc-embed-ensure-flashcard ()
  (interactive)
  (let ((element (org-element-at-point)))
    (cl-assert (not (cl-find (org-element-type element) '(comment keyword))))
    (save-excursion
      (if (org-fc-embed-goto-link-to-flashcard)
          (apply #'org-fc-embed-update-flashcard element)
        (apply #'org-fc-embed-export-flashcard element)))))

(provide 'org-fc-embed)
;;; org-fc-embed.el ends here
