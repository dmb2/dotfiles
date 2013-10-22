(setq user-mail-address "david.b@duke.edu")
(setq gnus-select-method 
      '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "DUKE" 
		      (nnimap-address "imap.duke.edu")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods 
	     '(nnimap "GMail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods 
	     '(nnimap "ATLAS"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods 
	     '(nnimap "CERN"
		      (nnimap-address "imap.cern.ch")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "JHU"
;;                                   (nnimap-address "jhem.johnshopkins.edu")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "FNAL"
;;                                   (nnimap-address "imapserver1.fnal.gov")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))

(setq mail-self-blind t)
(setq gnus-posting-styles
      '((".*"		        (address "david.b@duke.edu"))
	("^nnimap\\+CERN.*"	(address "david.b@cern.ch"))
	("^nnimap\\+DUKE.*"	(address "david.b@duke.edu"))
	("^nnimap\\+GMail.*"	(address "dbjergaard@gmail.com"))
	("^nnimap\\+ATLAS.*"	(address "david.bjergaard@gmail.com"))
	("^nnimap\\+GMail:JHU"	(address "davidb@jhu.edu"))
	("^nnimap\\+GMail:FNAL"	(address "davidb@cern.ch"))))

(setq nnimap-authinfo-file "~/.authinfo")

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

(gnus-add-configuration
 '(article
   (horizontal 1.0
        (vertical 33 (group 1.0))
        (vertical 1.0
    (summary 0.20 point)
    (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
        (vertical 33 (group 1.0))
        (vertical 1.0 (summary 1.0 point)))))

(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
(gnus-demon-add-handler 'gnus-group-get-new-news 5 nil)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)
(add-hook 'message-mode-hook
          '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
          'append)

;; (add-hook 'message-mode-hook
;; 	  (function (lambda() 
;; 		      (local-set-key (kbd "<tab>") 'bbdb-complete-name))))
(require 'bbdb-autoloads)
(require 'bbdb)
(load "bbdb-com" t)
(bbdb-initialize 'gnus 'message 'reportmail 'w3)

(setq
 bbdb-offer-save 1
 bbdb-use-pop-up nil
 bbdb-electric-p nil
 bbdb-popup-target-lines  1
 bbdb-dwim-net-address-allow-redundancy t
 bbdb-quiet-about-name-mismatches 2      
 bbdb-always-add-address t                
 bbbd-message-caching-enabled t         
 bbdb-use-alternate-names t )


;; Send via MSMTP
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
	(let* ((from (save-restriction
		       (message-narrow-to-headers)
		       (message-fetch-field "from")))
	       (account (cond ((string-match "david.b@cern.ch"           from) "CERN")
			      ((string-match "davidb@jhu.edu"		 from) "JHU")
			      ((string-match "dbjergaard@gmail.com"    	 from) "GMAIL")
			      ((string-match "david.bjergaard@gmail.com" from) "ATLAS")
			      ((string-match "david.b@duke.edu"		 from) "DUKE"))))
	  (setq message-sendmail-extra-arguments (list "-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)
(setq gnus-always-read-dribble-file t)

;; Mail notification configuration
(require 'mail-source)
(defvar wg/gnus-biff-groups nil
  "Groups to track within Gnus.")
(defun wg/gnus-biff ()
  "Update `mail-source-new-mail-available' with selected Gnus
newsgroups."
  (setq mail-source-new-mail-available
	(ignore-errors
	  (apply 'append
		 (mapcar 'gnus-list-of-unread-articles wg/gnus-biff-groups))))
  (display-time-event-handler))

(setq display-time-mail-function 'mail-source-new-mail-p)
(add-hook 'gnus-after-getting-new-news-hook 'wg/gnus-biff)
(add-hook 'gnus-summary-exit-hook 'wg/gnus-biff)
(setq wg/gnus-biff-groups
      '(;"nnimap+GMail:INBOX"
	;"nnimap+GMail:JHU"
	"nnimap+CERN:INBOX"
	"nnimap+DUKE:INBOX"
	"nnimap+ATLAS:INBOX")
      mail-source-flash nil)
