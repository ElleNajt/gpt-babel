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
    (evil-define-key* '(normal visual) org-mode-map
      (kbd "SPC o g f") 'gpt-babel/gptel-fix-block
      (kbd "SPC o g s") 'gpt-babel/send-block-to-gptel
      (kbd "SPC o g p") 'gpt-babel/patch-gptel-blocks
      (kbd "SPC o g i") 'gpt-babel/fix-with-instruction
      (kbd "SPC o g w") 'gpt-babel/wish-complete)))



(provide 'keybindings)
;;; keybindings.el ends here
