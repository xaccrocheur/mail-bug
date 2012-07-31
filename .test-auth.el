(defun nnimap-credentials ()
		(let*
						((machine (read-from-minibuffer "IMAP server host name: "))
							(port (read-from-minibuffer "IMAP server port: "))
							(auth-source-creation-prompts
								'((user  . "IMAP user at %h: ")
										(secret . "IMAP password for %u@%h: ")))
							(found (nth 0 (auth-source-search :max 1
																																									:host machine
																																									;; :port port
																																									;; :require '(:user :secret)
																																									:require '(:user :secret )
																																									:create t))))
				(if found
								(list (plist-get found :machine)
														(let ((secret (plist-get found :secret)))
																(if (functionp secret)
																				(funcall secret)
																		secret))
														(plist-get found :save-function))
						(message "not found")
						nil)))

(progn
		(funcall (nth 2 (nnimap-credentials)))
		(find-file "~/.authinfo")
		(write-file "~/.authinfo.gpg" nil)
		(kill-buffer ".authinfo.gpg")
)

(setq imapua-connection (imap-open imapua-host imapua-port 'ssl))
(imap-authenticate imapua-username imapua-password imapua-connection)
