

----------------------------------------------------------------------------> mail-bug 0.1b -->


(progn
  (hl-line-mode)
  (beginning-of-buffer)
  (insert-image (create-image "~/.emacs.d/lisp/mail-bug/lego-bug.png"))
  (animate-string (concat (string-repeat "-" (- (third (window-edges)) 25)) "> mail-bug 0.1b -->") 2 0)
  )

(insert-image (imapua-px-create-image "~/.emacs.d/lisp/mail-bug/lego-bug.xpm"))

(defconst smiley-data '(#x00 #x00 #x24 #x00 #x00 #x24 #x18 #x00))

(defun smiley-from-xbm-data ()
  ;; Make an XBM image from the smiley data
  (list 'image :type 'xbm :ascent 100 :data
        ))


(defimage test-image
	((:type xpm :file "/usr/share/pixmaps/xscreensaver.xpm")))

(defun imapua-px-insert-image (image type)
	"Make an image using the bytes directly in a string"
(insert-image (create-image image type nil :heuristic-mask t))

;; 	(list 'image :ascent 100
;; 				;; :data image
;; 				:type type
;; 				:file "/usr/share/icons/rosa/status/16/keys.xpm"
;; )
	)

(insert-image (imapua-px-image-from-string "/usr/share/apps/akonadi/contact/pics/world.jpg" "jpeg"))

(insert-image (imapua-px-image-from-string "/usr/share/pixmaps/xscreensaver.xpm" "xbm"))

(insert-image test-image)

;; this works
(insert-image (create-image "/usr/share/pixmaps/xscreensaver.xpm" 'xpm nil :heuristic-mask t))

;; this too
(insert-image (create-image "/usr/share/apps/akonadi/contact/pics/world.jpg" 'jpeg nil :heuristic-mask t))
