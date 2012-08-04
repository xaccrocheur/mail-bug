
(defcustom test-custom 't
  "yeah?"
  :type '(boolean)
  )


;; Customization for the agent's behaviour
(defcustom zplop-custom "zob"
  "The name of server to connect to"
  :type '(string)
  )


(setq test-custom nil)
(setq zplop-custom "zib")

(custom-save-all)
