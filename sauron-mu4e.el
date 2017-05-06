

;;; Code:
(require 'mu4e nil 'noerror)

(defvar sr-mu4e-important-filter nil
  "Important messages filter (should be replaced).")

(defvar sr-mu4e-running nil
  "*internal* whether sauron mu4e is running.")

(defun sauron-mu4e-start ()
  "Start watching mu4e."
  (if (not (boundp 'mu4e-mu-version))
      (progn
        (message "sauron-mu4e not available")
        nil)
    (unless sr-mu4e-running
      (add-hook 'mu4e-index-updated-hook
                'sr-mu4e-new-mail-notification)
      (setq sr-mu4e-running t))
    t))

(defun sauron-mu4e-stop ()
  "Stop watching mu4e."
  (when sr-mu4e-running
    (remove-hook 'mu4e-index-updated-hook
                 'sr-mu4e-new-mail-notification)
    (setq sr-mu4e-running nil)))


(defun check-unread-messages ()
  "Check our mail dir for 'new' messages and return the count."
  (let ((cmd (format "%s find flag:unread AND NOT flag:trashed | wc -l" mu4e-mu-binary)))
	(string-to-number (replace-regexp-in-string "![0-9]" "" (shell-command-to-string cmd)))
	)
  )

(defun check-important-messages()
  "Check our mail dir for 'new' messages and return the count."
  (if sr-mu4e-important-filter
      (let ((cmd (format "%s find %s | wc -l" mu4e-mu-binary sr-mu4e-important-filter)))
        (string-to-number (replace-regexp-in-string "![0-9]" "" (shell-command-to-string cmd)))
        )
    0))

(defun sr-mu4e-new-mail-notification ()
  "New mail notification routine for Sauron."

  (let ((unread (check-unread-messages))
        (nb_important (check-important-messages)))
    (if (> nb_important 0)
        (sauron-add-event
         'mu4e 6
         (format "You have %i unread messages including %i importants" unread nb_important))

      ;; If nothing important, still validate the number of of total messages
      (if (> unread 0)
          (sauron-add-event
           'mu4e 3
           (format "You have %i unread messages" unread)))

        )))

(provide 'sauron-mu4e)

;;; sauron-mu4e.el ends here
