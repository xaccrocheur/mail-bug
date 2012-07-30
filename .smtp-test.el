;; phil.cm@aol.com
(require 'smtpmail)

(setq smtpmail-default-smtp-server "mail.gmx.com")
(load-library "smtpmail")

(show-paren-mode)

(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-debug-info t
 smtpmail-debug-verb t)

(setq user-full-name "Phil CM")
(setq user-mail-address "philcm@gmx.com")

;; (setq 
;;  starttls-use-gnutls t
;;  starttls-gnutls-program "gnutls-cli"
;;  ;; starttls-extra-arguments '("--insecure")
;; )

(setq 
 smtpmail-smtp-server "mail.gmx.com"
 smtpmail-smtp-service 465
 smtpmail-stream-type 'ssl
;; smtpmail-auth-credentials '(("mail.gmx.com" "philcm@gmx.com" 465 "Amiga260."))
;; smtpmail-auth-credentials '(("mail.gmx.com" 465 "philcm@gmx.com" "Amiga260."))
;; smtpmail-starttls-credentials '(("mail.gmx.com" 465 nil nil)))
)

;; xaccrocheur@gmail.com
;; philcm@gmx.com
;; philippe.coatmeur@gmail.com

;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
;; (setq smtpmail-auth-credentials '(("smtp.gmail.com" 587 
;; "philippe.coatmeur@gmail.com" "Amiga520")))

;; (setq smtpmail-default-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)


;; ;; Use gmx smtp server
;; (message "Using gmx smtp server")
;; (setq starttls-use-gnutls t
;;       starttls-gnutls-program "gnutls-cli"
;;       starttls-extra-arguments nil)
;; (setq smtpmail-local-domain nil
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       message-send-mail-partially-limit nil
;;       smtpmail-default-smtp-server "mail.gmx.net"
;;       smtpmail-smtp-server "mail.gmx.net"
;;       smtpmail-debug-info t
;;       smtpmail-debug-verb t
;;       smtpmail-auth-credentials 
;;       '(("mail.gmx.net" 25 "philcm@gmx.com" "Amiga260."))
;;       smtpmail-starttls-credentials 
;;       '(("mail.gmx.net" 25 nil nil))
;;       smtpmail-smtp-server "mail.gmx.net")

;; (setq smtpmail-smtp-server "imap.gmail.com"
;;       smtpmail-default-smtp-server "imap.gmail.com"
;;       smtpmail-smtp-service 465
;;       smtpmail-starttls-credentials '(("imap.gmail.com" 465 nil nil)))

