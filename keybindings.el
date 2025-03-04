;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: March 01, 2025
;; Modified: March 01, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/keybindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'evil)
(require 'org)

(after! (evil org)
  (defun gpt-babel/map-suggested-keyindings ()
    "Map suggested keybindings"
    (interactive)
    (general-define-key
     :states '(normal visual)
     :keymaps 'org-mode-map
     :prefix "SPC o g"
     "f" 'gpt-babel/fix-block
     "s" 'gpt-babel/send-block
     "p" 'gpt-babel/patch-block
     "i" 'gpt-babel/fix-with-instructions
     "w" 'gpt-babel/wish-complete
     "c" '(:ignore t :which-key "context")
     "c a" 'gpt-babel/fix-block-file-above
     "c h" 'gpt-babel/fix-block-with-help)))


(provide 'keybindings)
;;; keybindings.el ends here
