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


(defun gpt-babel/fix-block-file-above ()
  (interactive)
  (gpt-babel/fix-block-with-context 'file-above))

(defun gpt-babel/fix-block-with-help ()
  (interactive)
  (gpt-babel/fix-block-with-context 'with-help))

(defun gpt-babel/python-help-clean (symbol process)
  "Get clean Python help documentation for SYMBOL using PROCESS."
  (let* ((help-command (format "
import sys
from io import StringIO
import inspect

__help_output = StringIO()
sys.stdout = __help_output
try:
    help('%s')
    help_text = __help_output.getvalue()
except:
    help_text = ''
sys.stdout = sys.__stdout__

try:
    if callable(%s) and not inspect.isbuiltin(%s):
        print('\\nSource code:\\n')
        source = inspect.getsource(%s).splitlines()[:20]
        print('\\n'.join(source))
except:
    pass

print('\\n'.join((help_text + '\\n' + __help_output.getvalue()).splitlines()[:20]))
" symbol symbol symbol symbol)))
    (with-temp-buffer
      (insert (python-shell-send-string-no-output help-command process))
      (goto-char (point-min))
      (while (re-search-forward "\\(.\\)\b\\1" nil t)
        (replace-match "\\1"))
      (buffer-string))))


(defun gpt-babel/get-context-with-help (lang block-content src-info)
  "Get help documentation for each line of BLOCK-CONTENT in LANG using PROCESS."
  (when (string= lang "python")
    (let* ((help-text "")
           (headers (nth 2 src-info))
           (session (cdr (assoc :session headers)))
           (session-buffer (get-buffer (format "*%s*" session)))
           (python-process (when session-buffer
                             (get-buffer-process session-buffer)))
           ;; TODO Would be cool to get another GPT process to decide what the important look ups are
           (python-symbol-regex "\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\(?:\\.[a-zA-Z_][a-zA-Z0-9_]*\\)*\\)"))
      (dolist (line (split-string block-content "\n"))
        (save-match-data
          (let ((pos 0))
            (while (string-match python-symbol-regex line pos)
              (let* ((symbol (match-string 0 line))
                     (_ (message "Found %s" symbol))
                     (help-result (gpt-babel/python-help-clean symbol python-process))
                     ;; TODO Truncating to prevent very large contexts
                     (truncated-help (when help-result
                                       (substring help-result 0 (min 3000 (length help-result))))))
                (when (and help-result (not (string-empty-p truncated-help)))
                  (setq help-text
                        (concat help-text
                                (format "\nHelp for %s:\n%s"
                                        symbol
                                        help-result))))
                (setq pos (match-end 0)))))))
      help-text)))

(defun gpt-babel/get-context (context-type)
  "Get context based on CONTEXT-TYPE."
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (src-info (org-babel-get-src-block-info))
           (lang (org-element-property :language src-block))
           (block-begin (org-element-property :begin src-block))
           (above-content (buffer-substring-no-properties 1 block-begin))
           (block-content (org-element-property :value src-block))
           (help-docs (when (eq context-type 'with-help)
                        (gpt-babel/get-context-with-help lang block-content src-info))))
      (if (eq context-type 'with-help)
          (format (alist-get context-type gpt-babel/context-types) 
                  above-content help-docs lang)
        (format (alist-get context-type gpt-babel/context-types) 
                above-content lang)))))





(defun gpt-babel/fix-block-with-context (context-type)
  "Fix block with specified CONTEXT-TYPE."
  (interactive (list (intern (completing-read 
                              "Context type: "
                              '("file-above" "with-help")))))
  (let ((custom-block-failure-prompt-template 
         (gpt-babel/get-context context-type)))
    (gpt-babel/fix-block)))



(defvar gpt-babel/context-types
  '((file-above . "Here's the context above this code block:\n%s\nContext ends here.\nPlease fix the code below. Return only the fixed code in a #+begin_src %s block.\n")
    (with-help . "Here's the context above this code block:\n%s\nContext ends here.\nHere's help documentation for the functions:\nDocumentation ends here.\n%s\nPlease fix the code below. Return only the fixed code in a #+begin_src %s block.\n"))
  "Templates for different context types.")


(defun gpt-babel/fix-with-instructions ()
  "Modify the current block based on comments."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((comment (read-string "Enter your modification request: "))
           (custom-block-failure-prompt-template
            (format "Please modify the code according to this request: %s\nReturn only the modified code in a #+begin_src %%s block.\n" comment)))
      (gpt-babel/fix-block))))

(defun gpt-babel/wish-complete ()
  "Turn the todos in the block into working code."
  (interactive)
  (when (org-in-src-block-p)
    (let ((custom-block-failure-prompt-template
           "Please implement the TODOs in this code. Return only the completed code in a #+begin_src %s block.\n"))
      (gpt-babel/fix-block))))

(defun gpt-babel/copy-org-babel-block-and-results ()
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

(defun gpt-babel/init-cell-errors-gptel ()
  "Create a new gptel buffer for cell errors."
  (interactive)
  (funcall-interactively #'gptel "*CELL ERRORS*"))

(defvar custom-block-failure-prompt-template
  "Please explain the issue, and please provide a corrected version of this, in a #+begin_src %s block.\n"
  "Template for the failure prompt, where %s is replaced with the language.")

(defun gpt-babel/get-last-src-block (lang)
  "Get content of last source block of type LANG."
  (save-excursion
    (goto-char (point-min))
    (let (last-block)
      (while (re-search-forward (format "^[ \t]*#\\+begin_src %s" lang) nil t)
        (let ((start (point)))
          (re-search-forward "^[ \t]*#\\+end_src" nil t)
          (setq last-block (buffer-substring-no-properties start (match-beginning 0)))))
      last-block)))

(defun gpt-babel/get-second-src-block (lang)
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

(defun gpt-babel/2nd-block ()
  (interactive)
  (message (get-second-python-block)))


(defun gpt-babel/org-babel-get-src-block-end ()
  "Get the end position of the current source block."
  (save-excursion
    (org-babel-goto-src-block-head)
    (re-search-forward "#\\+end_src" nil t)
    (line-beginning-position)))


(defun gpt-babel/send-block ()
  "Send org babel block with results to new gptel buffer without affecting current buffer."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((src-block (org-element-context))
           (lang (org-element-property :language src-block))
           (params (org-element-property :parameters src-block))
           (info (org-babel-get-src-block-info t))
           (header-args (nth 2 info))  ; Extract header args from info
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
           (home-dir (file-name-directory buffer-file-name))

           (home-file (file-name-sans-extension 
                       (file-name-nondirectory buffer-file-name)))
           (_ (message "home file %s" (file-name-directory buffer-file-name)))
           (buf (or (get-buffer "*CELL ERRORS*")
                    (progn (gpt-babel/init-cell-errors-gptel)
                           (get-buffer "*CELL ERRORS*")))))
      (with-current-buffer buf
        (setq-local org-babel-default-header-args header-args)
        (setq-local gpt-babel-home-dir home-dir)
        (setq-local gpt-babel-home-file home-file)
        (erase-buffer)
        (insert prompt content)
        (gptel-send)
        (pop-to-buffer buf)))))

(defun gpt-babel/adjust-plot-paths (orig-fun params &rest args)
  (interactive)
  "Adjust file paths in babel results to use gpt-babel-home-file."
  (let* ((options (nth 2 (car args)))
         (auto-align (if (string= "no" (cdr (assq :tables-auto-align options))) nil t))
         (result (apply orig-fun params args)))
    (if (stringp result)
        (replace-regexp-in-string
         "\\[\\[file:plots/\\([^]]+\\)\\]\\]"
         (lambda (match)
           (format "[[file:%s/plots/%s]]"
                   gpt-babel-home-file
                   (match-string 1 match)))
         result)
      result)))

(defun gpt-babel/diff-strings (str1 str2)
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

(defun gpt-babel/patch-block ()
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
                                   (end (gpt-babel/org-babel-get-src-block-end)))
                               (buffer-substring-no-properties start end))))
         (gpt-block (with-current-buffer "*CELL ERRORS*"
                      (gpt-babel/get-last-src-block lang)))
         (diff-buffer (get-buffer-create "*GPT Block Diff*"))
         (map (make-sparse-keymap)))
    (with-current-buffer diff-buffer
      (erase-buffer)
      (insert (gpt-babel/diff-strings original-content gpt-block))
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
                      (delete-region (point) (gpt-babel/org-babel-get-src-block-end))
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

(defvar gpt-babel/fix-block-buffer nil
  "Buffer to patch after GPT response.")

(defun gpt-babel/fix-block-response (beg end)
  "Handle GPT response for block fixing."
  (when gpt-babel/fix-block-buffer
    (with-current-buffer gpt-babel/fix-block-buffer
      (gpt-babel/patch-block))
    (pop-to-buffer "*GPT Block Diff*")
    (setq gpt-babel/fix-block-buffer nil)))

(add-hook 'gptel-post-response-functions #'gpt-babel/fix-block-response)

(defun gpt-babel/fix-block ()
  "Send block to GPT and patch when response is received."
  (interactive)
  (setq gpt-babel/fix-block-buffer (current-buffer))
  (gpt-babel/send-block))


;;; Automation:

(defvar org-babel-error-patterns
  '((python . "Traceback \\|(most recent call last)")
    (ipython . "Traceback \\|(most recent call last)")
    (jupyter-python . "Traceback \\|(most recent call last)")
    (shell . "Shell Error")
    (bash . "Shell Error")))

(defun gpt-babel/org-src-block-results-end (src-block)
  (save-excursion
    (goto-char (org-element-begin src-block))
    (when-let (results-loc (org-babel-where-is-src-block-result))
      (goto-char results-loc)
      (goto-char (org-element-end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (point))))

(defun gpt-babel/check-babel-result-error ()
  "Check if current src block results contain an error pattern."
  (interactive)
  (save-excursion
    (let* ((src-block (org-element-at-point))
           (lang (org-element-property :language src-block))
           (error-pattern (cdr (assoc (intern lang) org-babel-error-patterns)))
           (results-start (org-babel-where-is-src-block-result))
           (results-end (gpt-babel/org-src-block-results-end src-block)))
      (when (and results-start error-pattern)
        (goto-char results-start)
        (re-search-forward error-pattern results-end t)))))

(defun gpt-babel/test-check-traceback ()
  "Test the traceback detection function."
  (interactive)
  (let ((has-traceback (gpt-babel/check-babel-result-error )))
    (message (if has-traceback "Traceback detected" "No traceback found"))))

(defcustom gpt-babel/error-action nil
  "Action to take on code errors: nil (do nothing), 'send, or 'fix."
  :type '(choice (const :tag "None" nil)
          (const :tag "Send" send)
          (const :tag "Fix" fix))
  :group 'gpt-babel)

(defun gpt-babel/auto-process-errors (orig-fun params &rest var)
  (when (and (gpt-babel/check-babel-result-error)
             gpt-babel/error-action)
    (if (eq gpt-babel/error-action 'send)
        (gpt-babel/send-block)
      (gpt-babel/fix-block))))


(advice-add 'org-babel-insert-result :after #'gpt-babel/auto-process-errors)

;;; Keybindings

(defun gpt-babel-load-keybindings ()
  "Load alerts integrations for ob-python-extras."
  (let* ((this-file (locate-library "gpt-babel"))
         (this-dir (file-name-directory this-file)))
    (load (expand-file-name "keybindings" this-dir))))

(provide 'gpt-babel)
;;; gpt-babel.el ends here
