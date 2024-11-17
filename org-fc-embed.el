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
      (let* ((overlay (make-overlay (match-beginning 0) (match-end 0) nil 'front-advance))
             (string (match-string 1))
             (padding (- (string-width (match-string 0)) (string-width (match-string 1))))
             (openp (cl-plusp (cl-loop for char across string sum (cl-case char (?{ 1) (?} -1) (t 0))))))
        (overlay-put overlay 'category 'org-fc-cloze)
        (overlay-put overlay 'invisible nil)
        (overlay-put overlay 'display (if (org-at-table-p) (string-pad string (+ (length string) padding) nil openp) string))))))

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

(cl-defun org-fc-embed-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-fc-embed-put-cloze-overlays start end)
  (org-fc-embed-put-meta-overlays start end))

(cl-defun org-fc-embed-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-fc-embed-remove-cloze-overlays start end)
  (org-fc-embed-remove-meta-overlays start end))

(cl-defun org-fc-embed-update-overlays (&optional (start (point-min)) (end (point-max)))
  (org-fc-embed-remove-overlays start end)
  (when org-fc-embed-overlay-mode
    (org-fc-embed-put-overlays start end)))

(define-minor-mode org-fc-embed-overlay-mode
  "Minor mode for visualizing the status of embedded Org flashcards using overlays."
  :group 'org-fc-embed
  (cl-assert (eq major-mode 'org-mode))
  (if org-fc-embed-overlay-mode (org-fc-embed-put-overlays) (org-fc-embed-remove-overlays)))

(cl-defun org-fc-embed-cloze-1 (&optional (cloze-function #'org-fc-cloze-dwim) &aux marker-start marker-end)
  (save-excursion
    (setf marker-start (point-marker))
    (funcall cloze-function)
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
    (org-fc-embed-update-overlays marker-start marker-end)))

(defvar org-fc-embed-cloze-interactive-p nil)

(cl-defgeneric org-fc-embed-cloze (type &optional props)
  (ignore type props)
  (org-fc-embed-cloze-1
   (if org-fc-embed-cloze-interactive-p
       (lambda () (call-interactively #'org-fc-cloze-dwim))
     #'org-fc-cloze-dwim)))

(cl-defmethod org-fc-embed-cloze :around (_type &optional _props)
  (cl-ecase org-fc-embed-cloze-interactive-p
    ((t) (let ((org-fc-embed-cloze-interactive-p 'toplevel)) (cl-call-next-method)))
    ((toplevel) (let ((org-fc-embed-cloze-interactive-p nil)) (cl-call-next-method)))
    ((nil) (cl-call-next-method))))

;;;###autoload
(cl-defun org-fc-embed-cloze-dwim ()
  (interactive)
  (let ((org-fc-embed-cloze-interactive-p (called-interactively-p 'any)))
    (if (region-active-p)
        (org-fc-embed-cloze nil)
      (apply #'org-fc-embed-cloze (org-element-at-point)))))

(cl-defun org-fc-embed-process-clozes (&optional (start (point-min)) (end (point-max)) (process-function #'cl-values))
  (save-excursion
    (cl-loop with start = (copy-marker start) and end = (copy-marker end)
             with regexp = (rx "@@fc:" (group (+? anychar)) "@@")
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
             do (funcall process-function cloze))))

(defun org-fc-embed-uncloze-1 ()
  (cl-multiple-value-bind (start end)
      (if (region-active-p)
          (cl-values (region-beginning) (region-end))
        (let ((regexp-left (rx "@@fc:{{")) (regexp-right (rx "}}@@")))
          (let ((bl (or (and (save-excursion (re-search-backward regexp-left (pos-bol) t)) (match-beginning 0)) (pos-bol)))
                (br (or (and (save-excursion (re-search-backward regexp-right (pos-bol) t)) (match-end 0)) (pos-bol)))
                (fl (or (and (save-excursion (re-search-forward regexp-left (pos-eol) t)) (match-beginning 0)) (pos-eol)))
                (fr (or (and (save-excursion (re-search-forward regexp-right (pos-eol) t)) (match-end 0)) (pos-eol))))
            (if (< (1- br) bl (point) fr (1+ fl)) (cl-values bl fr)
              (cl-assert (< (1- (point)) fl fr)) (cl-values fl fr)))))
    (let ((start (copy-marker start)) (end (copy-marker end)))
      (prog1 (org-fc-embed-process-clozes
              start end
              (lambda (cloze)
                (cl-assert (looking-back (rx "{{" (literal cloze) "}" (*? anychar) "}") (pos-bol)))
                (replace-match cloze)))
        (org-fc-embed-update-overlays start end)))))

(cl-defgeneric org-fc-embed-uncloze (type &optional props)
  (ignore type props)
  (org-fc-embed-uncloze-1))

;;;###autoload
(cl-defun org-fc-embed-uncloze-dwim ()
  (interactive)
  (if (region-active-p)
      (org-fc-embed-uncloze nil)
    (apply #'org-fc-embed-uncloze (org-element-at-point))))

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

(cl-defun org-fc-embed-export-clozes (&optional (start (point-min)) (end (point-max)))
  (org-fc-embed-process-clozes
   start end
   (let ((hashes (make-hash-table)))
     (lambda (cloze)
       (let ((hash (org-fc-embed-string-hash cloze)))
         (cl-assert (looking-back "}}" (pos-bol)))
         (backward-char 1)
         (cl-assert (null (gethash hash hashes)))
         (setf (gethash hash hashes) cloze)
         (insert (format "@%d" hash)))))))

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
      (when (cl-plusp (org-fc-embed-export-clozes (point)))
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
                                (org-fc-embed-update-overlays start (point))))))
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

(cl-defun org-fc-embed-remove-comments (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward (rx "@@comment:" (*? anychar) "@@" (* blank)) end t)
             do (replace-match "")
             sum 1)))

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
          (when (cl-plusp (org-fc-embed-export-clozes (point)))
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

(cl-defun org-fc-embed-cloze-table-fields (&optional (range '(1 . 1)))
  (cl-multiple-value-bind (row-start row-end column-start column-end)
      (cl-flet ((ensure-range (object)
                  (cl-typecase object
                    (cons (list (car object) (cdr object)))
                    (fixnum (list object most-positive-fixnum)))))
        (cl-etypecase range
          (cons (cl-values-list (nconc (ensure-range (car range)) (ensure-range (cdr range)))))
          (fixnum (cl-values-list (nconc (ensure-range range) (ensure-range 0))))
          (string
           (string-match
            (rx-let ((ref (or (+ (char "<>")) (+ digit))))
              (rx bos "@" (group ref) "$" (group ref) ".." (optional "@" (group ref) "$" (group ref)) eos))
            range)
           (let ((row-start (match-string 1 range))
                 (column-start (match-string 2 range))
                 (row-end (match-string 3 range))
                 (column-end (match-string 4 range)))
             (cl-multiple-value-bind (rows columns)
                 (cl-loop for line in (org-table-to-lisp)
                          count (listp line) into rows
                          when (listp line)
                          maximize (length line) into columns
                          finally (cl-return (cl-values rows columns)))
               (cl-flet ((parse-ref (desc count)
                           (or (ignore-errors (cl-parse-integer desc))
                               (let ((desc (cl-loop for char across desc sum (cl-case char (?< 1) (?> -1) (t 0)))))
                                 (when (cl-plusp desc) (cl-decf desc))
                                 (mod desc count)))))
                 (cl-values
                  (parse-ref row-start rows)
                  (or (and row-end (parse-ref row-end rows)) (1- rows))
                  (parse-ref column-start columns)
                  (or (and column-end (parse-ref column-end columns)) (1- columns)))))))))
    (save-excursion
      (set-mark (org-table-begin))
      (goto-char (org-table-end))
      (org-fc-embed-uncloze nil)
      (cl-loop initially (goto-char (org-table-begin))
               for line in (org-table-to-lisp)
               when (listp line)
               do (cl-loop for field in line
                           for column from 0
                           do (org-table-next-field)
                           when (and (<= row-start row row-end) (<= column-start column column-end))
                           unless (string-empty-p (org-table-get nil nil))
                           do
                           (org-table-end-of-field 1)
                           (cl-assert (looking-back (rx (literal field)) (pos-bol)))
                           (set-mark (match-beginning 0))
                           (apply #'org-fc-embed-cloze (org-element-at-point)))
               count (listp line) into row
               finally (org-table-align))
      (org-fc-embed-update-overlays (org-table-begin) (org-table-end)))))

(cl-defmethod org-fc-embed-cloze ((_type (eql 'table)) &optional _props)
  (apply #'org-fc-embed-cloze-table-fields
         (when org-fc-embed-cloze-interactive-p
           (list (read-string "Range: " "@<<$<<..@>$>")))))

(cl-defmethod org-fc-embed-uncloze ((type (eql 'table)) &optional props)
  (save-excursion
    (let ((element (list type props)))
      (goto-char (org-element-begin element))
      (set-mark (org-element-end element))
      (cl-call-next-method)
      (org-table-align))))

(provide 'org-fc-embed)
;;; org-fc-embed.el ends here
