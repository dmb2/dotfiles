
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   '("87e00d5085192ab89975858b8d06db1ce4b1afab0b18bde0ffccc3c384b651b6" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "7366916327c60fdf17b53b4ac7f565866c38e1b4a27345fe7facbf16b7a4e9e8" "b050365105e429cb517d98f9a267d30c89336e36b109a1723d95bc0f7ce8c11d" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "261f5ddd72a1c0b47200e13d872075af5f78c3f07d2968bddc0301261934f210" "71f116ced24c4993212d555b477d28500cca2d883781efa8effad812bde6369d" "8022cea21aa4daca569aee5c1b875fbb3f3248a5debc6fc8cf5833f2936fbb22" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(display-time-default-load-average nil)
 '(display-time-format "")
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(erc-away-nickname "dmb2-AFK")
 '(erc-nick "dmb2")
 '(erc-prompt-for-password nil)
 '(inhibit-startup-screen t)
 '(jabber-autoaway-status "Away")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(package-selected-packages
   '(elpa-clone rust-mode flexoki-themes magit markdown-mode slime hydra sly-asdf sly-repl-ansi-color evil-string-inflection string-inflection gruvbox-theme org-ref evil-lispy lispy eieio web-beautify twittering-mode smex shell-switcher scratch robe rect-mark parsebib paredit paradox org-bullets notify names key-chord jabber goto-last-change flycheck f evil-tutor evil-surround evil-smartparens evil-numbers evil-nerd-commenter evil-magit evil-leader evil-ediff evil-commentary emojify emms-player-mpv drag-stuff company c-eldoc biblio bbdb))
 '(paradox-automatically-star nil)
 '(safe-local-variable-values
   '((Lowercase . T)
     (Base . 10)
     (Syntax . COMMON-LISP)
     (Package . XLIB)
     (Syntax . Common-Lisp)))
 '(send-mail-function 'mailclient-send-it)
 '(sp-base-key-bindings 'sp)
 '(syslog-debug-face
   '((t :background unspecified :foreground "#2aa198" :weight bold)))
 '(syslog-error-face
   '((t :background unspecified :foreground "#dc322f" :weight bold)))
 '(syslog-hour-face '((t :background unspecified :foreground "#859900")))
 '(syslog-info-face
   '((t :background unspecified :foreground "#268bd2" :weight bold)))
 '(syslog-ip-face '((t :background unspecified :foreground "#b58900")))
 '(syslog-su-face '((t :background unspecified :foreground "#d33682")))
 '(syslog-warn-face
   '((t :background unspecified :foreground "#cb4b16" :weight bold)))
 '(w3m-follow-redirection 100))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#fffcf0" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#CECDC3" :font "Source Sans Pro"))))
 '(tex-verbatim ((t nil)))
 '(variable-pitch ((t (:height 1.3 :family "Crimson")))))

;; Custom elisp code
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs-lisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(setq dotfiles-dir "~/.emacs-lisp/")

;; load up Org-mode and Org-babel
(require 'org)
(require 'ob-tangle)
;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file 
      (directory-files dotfiles-dir t "\\.org$"))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(put 'downcase-region 'disabled nil)
