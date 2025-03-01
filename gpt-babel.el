;;; gpt-babel.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: March 01, 2025
;; Modified: March 01, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/gpt-babel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'general)
(require 'gptel)

(defun gptel-babel/copy-org-babel-block-and-results ()
  "Copy the current org babel source block and its results."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (results (org-babel-where-is-src-block-result))
           (end (if results
                    (save-excursion
                      (goto-char results)
                      (forward-line)
                      (org-babel-result-end))
                  (org-element-property :end src-block))))
      (copy-region-as-kill
       (org-element-property :begin src-block)
       end)
      (message "Copied source block and results"))))

(defun gptel-babel/init-cell-errors-gptel ()
  "Create a new gptel buffer for cell errors."
  (interactive)
  (funcall-interactively #'gptel "*CELL ERRORS*"))

(defvar custom-block-failure-prompt-template
  "Please explain the issue, and please provide a corrected version of this, in a #+begin_src %s block.\n"
  "Template for the failure prompt, where %s is replaced with the language.")

(defun gptel-babel/get-second-src-block (lang)
  "Get content of second source block of type LANG."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (and (< count 2)
                  (re-search-forward (format "^[ \t]*#\\+begin_src %s" lang) nil t))
        (setq count (1+ count)))
      (when (= count 2)
        (let ((start (point)))
          (re-search-forward "^[ \t]*#\\+end_src" nil t)
          (buffer-substring-no-properties start (match-beginning 0)))))))

(defun gptel-babel/2nd-block ()
  (interactive)
  (message (get-second-python-block)))


(defun gptel-babel/org-babel-get-src-block-end ()
  "Get the end position of the current source block."
  (save-excursion
    (org-babel-goto-src-block-head)
    (re-search-forward "#\\+end_src" nil t)
    (line-beginning-position)))

(defun gptel-babel/send-block-to-gptel ()
  "Send org babel block with results to new gptel buffer without affecting current buffer."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (lang (org-element-property :language src-block))
           (results (org-babel-where-is-src-block-result))
           (end (if results
                    (save-excursion
                      (goto-char results)
                      (forward-line)
                      (org-babel-result-end))
                  (org-element-property :end src-block)))
           (content (buffer-substring-no-properties
                     (org-element-property :begin src-block)
                     end))

           (prompt (format custom-block-failure-prompt-template lang))

           (buf (or (get-buffer "*CELL ERRORS*")
                    (progn (gptel-babel/init-cell-errors-gptel)
                           (get-buffer "*CELL ERRORS*")))))

      (with-current-buffer buf
        (erase-buffer)
        (insert prompt content)
        (gptel-send)
        (pop-to-buffer buf)))))

(defun gptel-babel/diff-strings (str1 str2)
  "Return a diff between STR1 and STR2 as a string."
  (with-temp-buffer
    (let ((file1 (make-temp-file "diff1"))
          (file2 (make-temp-file "diff2")))
      (with-temp-file file1 (insert str1))
      (with-temp-file file2 (insert str2))
      (call-process "diff" nil t nil "-u" file1 file2)
      (delete-file file1)
      (delete-file file2)
      (buffer-string))))

(defun gptel-babel/patch-gptel-blocks ()
  "Send block to gptel and show diff with accept option."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (lang (org-element-property :language (org-element-context)))
         (original-content (when (org-in-src-block-p)
                             (org-babel-get-src-block-info 'light)
                             (let ((start (save-excursion
                                            (org-babel-goto-src-block-head)
                                            (forward-line 1)
                                            (point)))
                                   (end (gptel-babel/org-babel-get-src-block-end)))
                               (buffer-substring-no-properties start end))))
         (gpt-block (with-current-buffer "*CELL ERRORS*"
                      (gptel-babel/get-second-src-block lang)))
         (diff-buffer (get-buffer-create "*GPT Block Diff*"))
         (map (make-sparse-keymap)))
    (with-current-buffer diff-buffer
      (erase-buffer)
      (insert (gptel-babel/diff-strings original-content gpt-block))
      (diff-mode)
      (define-key map (kbd "C-c C-c")
                  (lambda ()
                    (interactive)
                    (with-current-buffer original-buffer
                      (goto-char original-point)
                      (org-babel-goto-src-block-head)
                      (org-babel-remove-result)
                      (re-search-forward "#\\+begin_src.*$")
                      (forward-line 1)
                      (delete-region (point) (gptel-babel/org-babel-get-src-block-end))
                      (insert gpt-block)
                      (quit-window)
                      (pop-to-buffer original-buffer)
                      )))
      (define-key map (kbd "C-c C-k")
                  (lambda ()
                    (interactive)
                    (quit-window)
                    (pop-to-buffer original-buffer)))
      (use-local-map (make-composed-keymap map (current-local-map)))
      (display-buffer diff-buffer)
      (pop-to-buffer diff-buffer))))

(defvar gptel-babel/gptel-fix-block-buffer nil
  "Buffer to patch after GPT response.")

(defun gptel-babel/gptel-fix-block-response (beg end)
  "Handle GPT response for block fixing."
  (when gptel-babel/gptel-fix-block-buffer
    (with-current-buffer gptel-babel/gptel-fix-block-buffer
      (gptel-babel/patch-gptel-blocks))
    (pop-to-buffer "*GPT Block Diff*")
    (setq gptel-babel/gptel-fix-block-buffer nil)))

(add-hook 'gptel-post-response-functions #'gptel-babel/gptel-fix-block-response)

(defun gptel-babel/gptel-fix-block ()
  "Send block to GPT and patch when response is received."
  (interactive)
  (setq gptel-babel/gptel-fix-block-buffer (current-buffer))
  (gptel-babel/send-block-to-gptel))


;;; Automation:

(defvar org-babel-error-patterns
  '((python . "Traceback \\|(most recent call last)")
    (ipython . "Traceback \\|(most recent call last)")
    (jupyter-python . "Traceback \\|(most recent call last)")
    (shell . "Shell Error")
    (bash . "Shell Error")))

(defun gptel-babel/org-src-block-results-end (src-block)
  (save-excursion
    (goto-char (org-element-begin src-block))
    (when-let (results-loc (org-babel-where-is-src-block-result))
      (goto-char results-loc)
      (goto-char (org-element-end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (point))))

(defun gptel-babel/check-babel-result-error ()
  "Check if current src block results contain an error pattern."
  (interactive)
  (save-excursion
    (let* ((src-block (org-element-at-point))
           (lang (org-element-property :language src-block))
           (error-pattern (cdr (assoc (intern lang) org-babel-error-patterns)))
           (results-start (org-babel-where-is-src-block-result))
           (results-end (gptel-babel/org-src-block-results-end src-block)))
      (when (and results-start error-pattern)
        (goto-char results-start)
        (re-search-forward error-pattern results-end t)))))


(defun gptel-babel/test-check-traceback ()
  "Test the traceback detection function."
  (interactive)
  (let ((has-traceback (gptel-babel/check-babel-result-error )))
    (message (if has-traceback "Traceback detected" "No traceback found"))))

(setq gptel-babel/auto-send-on-traceback t)

(defun gptel-babel/test-for-errors (orig-fun params &rest var )
  (if (and (fboundp 'gptel-babel/send-block-to-gptel) gptel-babel/auto-send-on-traceback)
      (if (gptel-babel/check-babel-result-error)
          (gptel-babel/send-block-to-gptel) ())))


(advice-add 'org-babel-insert-result :after #'gptel-babel/test-for-errors)


;;;; Shell
;;  Some means to get a recognizable error string in the shell output, for automatic traceback.
;; TODO Currently broken

(defun gptel-babel/add-error-trap (orig-fun body params)
  "Add ERR trap to shell scripts for error reporting."
  ;;  TODO the problem appears to be that ' is getting converted into ’  
  (let* ((trap-cmd "trap 'rc=$?; [ $rc -ne 0 ]; then echo \"Shell Error\"; fi' ERR")
         (body_with_trap  (concat trap-cmd "\n" body))
         (_ (message (format "%s" body_with_trap))))
    (if (string= (cdr (assq :lang params)) "bash")
        (funcall orig-fun
                 body_with_trap
                 
                 
                 params)
      (funcall orig-fun body params))))

;; (advice-add 'org-babel-execute:shell :around #'gptel-babel/add-error-trap)

(provide 'gpt-babel)
;;; gpt-babel.el ends here
