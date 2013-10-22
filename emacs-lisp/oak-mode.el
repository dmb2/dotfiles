;;; oak-mode.el --- Emacs interface for ROOT

;; Author: David Bjergaard <david.b@duke.edu>

;; This file is NOT part of GNU Emacs

;;; Comentary:

;; Oak provides a REPL like environment for performing data analysis
;; within C++ and ROOT. While ROOT is primarily used for High Energy
;; Physics analysis, it provides a general purpose set of C++
;; libraries for data analysis. It also includes CINT/CLING a C++
;; interpreter which in this case will act as the repl.

;;; Code: 

(require 'comint)

;;; User Variables:
(defvar oak-root-path (expand-file-name "~/root/bin/root")
  "Path to ROOT binary")

(defvar oak-root-switches "-l"
  "Command line switches to be called with ROOT binary")

;;; Internal Variables:

;;; Functions:

(defun oak-run-root ()
  "Run ROOT in a terminal emulation buffer in the same vein of eshell-visual-commands"
  (interactive)
      (switch-to-buffer "*oak-repl*")
      (make-comint "oak-repl" oak-root-path nil oak-root-switches))


(provide 'oak-mode)
;;; oak-mode.el ends here
