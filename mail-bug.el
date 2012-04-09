;;; mail-bug.el --- Notify of unread mails on mode line & on DBus

;; Copyright (C) 2012 Phil CM

;; Author: Phil CM <philippe.coatmeur@gmail.com>
;; Keywords: mail notification desktop
;; Version: 0.0.2
;; Url: http://github.com/xaccrocheur/mail-bug.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show unread mails count on mode line (and details in tooltip on
;; mouse over) and a desktop notification for new mails.
;; To enable it, put this in your .emacs :

;; (require 'mail-bug)
;; (mail-bug-init)
;; Fill in your ~/.authinfo.gpg according to the book, here's mine :

;; machine mail.gandi.net login LOGIN port 993 password PASSWORD
;; machine smtp.gmail.com login LOGIN port 587 password PASSWORD
;; machine imap.gmail.com login LOGIN port 993 password PASSWORD

;; and use M-x customize-group "mail-bug" RET

;;; Code:

(require 'auth-source)
(require 'dbus)

(defgroup mail-bug nil
  "Universal mail notifier."
  :prefix "mail-bug-"
  :group 'mail)

(defgroup mail-bug-account-one nil
  "Details for account one."
  :prefix "mail-bug-accounts"
  :group 'mail-bug)

(defgroup mail-bug-account-two nil
  "Details for account two."
  :prefix "mail-bug-accounts"
  :group 'mail-bug)

(defcustom mail-bug-launch-client-command "px-go-mail"
  "Mail client command.
Example : wl"
  :type 'string
  :group 'mail-bug)

(defun mail-bug-launch-client ()
  (if (string-equal mail-bug-launch-client-command "gnus")
      (gnus)
    (if (string-equal mail-bug-launch-client-command "wl")
	(wl))
    (px-go-mail)))

(defcustom mail-bug-host-one "imap.gmail.com"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bug-account-one)

(defcustom mail-bug-port-one "993"
  "Port number and (optional) protocol path.
993 IS the default IMAP port"
  :type 'string
  :group 'mail-bug-account-one)

(defcustom mail-bug-imap-box-one "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bug-account-one)

(defcustom mail-bug-host-two "mail.gandi.net"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bug-account-two)

(defcustom mail-bug-port-two "993"
  "Port number and (optional) protocol path.
993 IS the default IMAP port"
  :type 'string
  :group 'mail-bug-account-two)

(defcustom mail-bug-imap-box-two "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bug-account-two)

(defcustom mail-bug-new-mail-sound "/usr/share/sounds/KDE-Im-New-Mail.ogg"
  "Sound for new mail notification.
Any format works."
  :type 'string
  :group 'mail-bug)

(defcustom mail-bug-new-mail-icon-one "/usr/share/icons/oxygen/128x128/actions/configure.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bug-account-one)

(defcustom mail-bug-new-mail-icon-two "/usr/share/icons/Revenge/128x128/apps/emacs.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bug-account-two)

(defcustom mail-bug-icon-one
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/lisp/mail-bug/greenbug.xpm"
	    :ascent center))
  "Icon for the first account.
Must be an XPM (use Gimp)."
  :group 'mail-bug-account-one)

(defcustom mail-bug-icon-two
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/lisp/mail-bug/ladybug.xpm"
	    :ascent center))
  "Icon for the second account.
Must be an XPM (use Gimp)."
  :group 'mail-bug-account-two)

(defconst mail-bug-logo-one
  (if (and window-system
	   mail-bug-icon-two)
      (apply 'propertize " " `(display ,mail-bug-icon-one))
    mail-bug-host-one))

(defconst mail-bug-logo-two
  (if (and window-system
	   mail-bug-icon-two)
      (apply 'propertize " " `(display ,mail-bug-icon-two))
    mail-bug-host-two))

(defvar mail-bug-unseen-mails nil)
(defvar mail-bug-advertised-mails-one '())
(defvar mail-bug-advertised-mails-two '())

(defvar mail-bug-shell-script-command "mail-bug.pl"
  "Full command line. Can't touch dat.")

(defcustom mail-bug-timer-interval 200
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bug)

;;;###autoload
(defun mail-bug-init ()
  "Init"
  (interactive)
  (add-to-list 'global-mode-string
               '(:eval (mail-bug-mode-line)))
  (run-with-timer 0
		  mail-bug-timer-interval
		  'mail-bug-check-all))

(defun mail-bug-check-all ()
  "Check unread mail now."
  (interactive)
  (if (get-buffer  (concat "*mail-bug-" mail-bug-host-one "*"))
      (kill-buffer (concat "*mail-bug-" mail-bug-host-one "*")))
  (if (get-buffer  (concat "*mail-bug-" mail-bug-host-two "*"))
      (kill-buffer (concat "*mail-bug-" mail-bug-host-two "*")))
  (mail-bug-check mail-bug-host-one mail-bug-port-one mail-bug-imap-box-one)
  (mail-bug-check mail-bug-host-two mail-bug-port-two mail-bug-imap-box-two))

(defun mail-bug-check (host port box)
  "Check unread mail."
  ;; (message "%s %s %s" host protocol box)
  (mail-bug-shell-command
   (format "%s %s %s %s %s %s"
           mail-bug-shell-script-command
	   host
	   port
	   box
	   (auth-source-user-or-password "login" host port)
	   (auth-source-user-or-password "password" host port))
   'mail-bug-shell-command-callback host))

(defmacro mail-bug-shell-command (cmd callback account)
  "Run CMD asynchronously, then run CALLBACK"
  `(let* ((buf (generate-new-buffer (concat "*mail-bug-" ,account "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)
          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
		  (funcall, callback)
                (error "mail-bug error: %d" err)))))))))

(defun mail-bug-read-mail-callback ()
  "Construct the mail elements list"
  (setq this-mail (mail-bug-buffer-to-list (concat "*mail-bug-" mail-bug-host-two "*"))))

(defun mail-bug-shell-command-callback ()
  "Construct the unread mails lists"
  (setq mail-bug-unseen-mails-one (mail-bug-buffer-to-list (concat "*mail-bug-" mail-bug-host-one "*")))
  (setq mail-bug-unseen-mails-two (mail-bug-buffer-to-list (concat "*mail-bug-" mail-bug-host-two "*")))
  (mail-bug-mode-line)
  ;; (mail-bug-desktop-notify mail-bug-new-mail-icon-one)
  ;; (mail-bug-desktop-notify mail-bug-new-mail-icon-two)
  (mail-bug-desktop-notify-one)
  (mail-bug-desktop-notify-two)
  (force-mode-line-update))

(defun mail-bug-own-little-imap-client (maillist)
  (interactive)
  (princ
   (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n--------------\n%s\n"
		   (car (nthcdr 1 x))
		   ;; (nthcdr 2 x)
		   (nthcdr 2 x)
		   (car x)
		   (car (nthcdr 2 x))
		   ;; (car x)
		   )))
       tooltip-string)
     )
   maillist
   "\n xxx \n")
   (generate-new-buffer "MBOLIC"))
  (switch-to-buffer "MBOLIC"))

(defun mail-bug-mode-line ()
  "Construct an emacs modeline object"
(concat
  (if (null mail-bug-unseen-mails-one)
      (concat mail-bug-logo-one "  ")
    (let ((s
	   (format "%d" (length mail-bug-unseen-mails-one)))
          (map (make-sparse-keymap))
          (url (concat "http://" mail-bug-host-one)))

      (define-key map (vector 'mode-line 'mouse-1)
        `(lambda (e)
           (interactive "e")
	   (mail-bug-launch-client)))

      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))

      (define-key map (vector 'mode-line 'mouse-3)
        `(lambda (e)
           (interactive "e")
	   (mbolic mail-bug-unseen-mails-one)))

      (add-text-properties 0 (length s)
                           `(local-map,
			     map mouse-face mode-line-highlight
			     uri, url help-echo,
			     (concat
			      (mail-bug-tooltip "one")
			      (format "
--------------
mouse-1: View mail in %s
mouse-2: View mail on %s
mouse-3: View mail in MBOLIC" mail-bug-launch-client-command mail-bug-host-one mail-bug-host-one)))
                           s)
      (concat mail-bug-logo-one ":" s)))
" "
  (if (null mail-bug-unseen-mails-two)
      (concat mail-bug-logo-two "  ")
    (let ((s
	   (format "%d" (length mail-bug-unseen-mails-two)))
          (map (make-sparse-keymap))
          (url (concat "http://" mail-bug-host-two)))

      (define-key map (vector 'mode-line 'mouse-1)
        `(lambda (e)
           (interactive "e")
	   (mail-bug-launch-client)))

      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))

      (define-key map (vector 'mode-line 'mouse-3)
        `(lambda (e)
           (interactive "e")
	   (mbolic mail-bug-unseen-mails-two)))

      (add-text-properties 0 (length s)
                           `(local-map,
			     map mouse-face mode-line-highlight
			     uri, url help-echo,
			     (concat
			      (mail-bug-tooltip "two")
			      (format "
--------------
mouse-1: View mail in %s
mouse-2: View mail on %s
mouse-3: View mail in MBOLIC" mail-bug-launch-client-command mail-bug-host-two mail-bug-host-two)))
                           s)
      (concat mail-bug-logo-two ":" s)))))

(defun mbolic-open-mail (mail-id)
)

(defun mbolic (maillist)
  (interactive)
  (if (get-buffer "MBOLIC")
      (kill-buffer "MBOLIC"))
  (switch-to-buffer "MBOLIC")

  (let ((inhibit-read-only t))
    (erase-buffer))

  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))

  (mapcar
   (lambda (x)
     (let
	 ((tooltip-string
	   (format " %s | %s | %s (%s)"

			 (car (nthcdr 1 x)) ; date
			 (car x)	    ; from
			 (car (nthcdr 2 x)) ; subject
			 (car (nthcdr 3 x)) ; id
			 )))
       (progn
	 (widget-create 'push-button
			:notify (lambda (&rest ignore)
				  (widget-insert "\n")
				  (widget-insert "plop"))
			tooltip-string)
	 (widget-insert "\n")))
     )
   maillist)
  (use-local-map widget-keymap)
  (widget-setup))

(defun mail-bug-tooltip (list)
  "Loop through the mail headers and build the hover tooltip"
  (if (string-equal "one" list)
      (setq zelist mail-bug-unseen-mails-one)
    (setq zelist mail-bug-unseen-mails-two))
  (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n-------\n%s"
		   (car x)
		   (car (nthcdr 1 x))
		   (car (nthcdr 2 x))
		   )))
       tooltip-string)
     )
   zelist
   "\n\n"))

(defun mail-bug-desktop-notify-one ()
  (mapcar
   (lambda (x)
     (if (not (member x mail-bug-advertised-mails-one))
	 (progn
	   (mail-bug-desktop-notification
	    "<h3 style='color:palegreen;'>New mail!</h3>"
	    (format "<h4>%s</h4><h5>%s</h5><hr>%s"
		    '(car (car x))
		    '(car (nthcdr 1 x))
		    '(car (nthcdr 2 x)))
	    1 mail-bug-new-mail-icon-one)
	   (add-to-list 'mail-bug-advertised-mails-one x))))
   mail-bug-unseen-mails-one))

(defun mail-bug-desktop-notify-two ()
  (mapcar
   (lambda (x)
     (if (not (member x mail-bug-advertised-mails-two))
	 (progn
	   (mail-bug-desktop-notification
	    "<h3 style='color:red;'>New mail!</h3>"
	    (format "<h4>%s</h4><h5>%s</h5><hr>%s"
		    '(car (car x))
		    '(car (nthcdr 1 x))
		    '(car (nthcdr 2 x)))
	    1 mail-bug-new-mail-icon-two)
	   (add-to-list 'mail-bug-advertised-mails-two x))))
   mail-bug-unseen-mails-two))

(defun mail-bug-desktop-notification (summary body timeout icon)
  "Call notification-daemon method with ARGS over dbus"
  (if (window-system)
      ;; (if mail-bug-new-mail-sound
      ;;     (shell-command
      ;;      (concat "mplayer -really-quiet " mail-bug-new-mail-sound " 2> /dev/null")))
      (dbus-call-method
       :session                                 ; use the session (not system) bus
       "org.freedesktop.Notifications"          ; service name
       "/org/freedesktop/Notifications"         ; path name
       "org.freedesktop.Notifications" "Notify" ; Method
       "GNU Emacs"			       	    ; Application
       0					    ; Timeout
       icon
       summary
       body
       '(:array)
       '(:array :signature "{sv}")
       ':int32 timeout)
    (message "New mail!" )))

;; Utilities

(defun mail-bug-buffer-to-list (buf)
  "Make & return a list (of lists) LINES from lines in a buffer BUF"
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let ((lines '()))
        (while (not (eobp))
          (push (split-string
                 (buffer-substring (point) (point-at-eol)) "\|_\|")
                lines)
          (beginning-of-line 2))
	lines))))

(defun mail-bug-wordwrap (s N)
  "Hard wrap string S on 2 lines to N chars"
  (if (< N (length s))
      (concat (subseq s 0 N) "\n" (subseq s N) "...")
    s))

(defun mail-bug-format-time (s)
  "Clean Time string S"
  (subseq (car s) 0 -6))

(defun Rx ()
(interactive)
(setq mail-bug-advertised-mails-one '())
(setq mail-bug-advertised-mails-two '()))

(message "%s loaded" (or load-file-name buffer-file-name))
(provide 'mail-bug)


;; (defun dumb-f ()
;;   (message "I'm a function"))

;; (defvar my-function 'plopssss)

;; (funcall my-function)
;; ==> "I'm a function"

;; (setq zz '(("monique.marin2@free.fr" "Mon, 2 Apr 2012 13:52:39 +0000" "Départ " "1543")
;;  ("\"agnes coatmeur-marin\" <agneslcm@gmx.fr>" "Mon, 02 Apr 2012 15:20:23 +0200" "Re : Quelle tuile ce plan pourri jet4you" "1542")
;;  ("<contact@adamweb.net>" "Mon, 2 Apr 2012 14:15:42 +0200" "Re: un message en stupide français à la con" "1541")))q
