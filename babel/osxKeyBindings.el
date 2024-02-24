(defun akh-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  ;; (find-file-other-window user-init-file)
  ;; (find-file-other-frame user-init-file)
  (find-file user-init-file)
  )

(cond ((eq system-type 'darwin)
       (progn
	 (if (display-graphic-p)
	     (progn
	       ;; -- place win only commands here --
	       ;; NON STANDARD KEYS ----------------------------------------------------
	       (global-set-key (kbd "s-,") #'akh-find-user-init-file) ;; edit config file
	       )
	   (progn
	     ;; -- place term only commands here --
	     )
	   )
	 ;; -- place common commands here --
	 ))
      ) ;; cond
