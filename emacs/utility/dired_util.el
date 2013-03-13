;; I usually have a site_conf.el file for each machine where location specific 
;; vars are defined. I've included my-shortcut-alist here as an example

;; Using an alist specifying shortcut=>location
;; Add as many as you want here.
(setq my-shortcuts-alist '(
			   ("dev_seamonkey" . "g:/code/projects/my-projects-pre-change/seamonkey")
			   ("dev_legacy" . "g:/code/projects/my-projects-pre-change/my-projects")
			   ("emacs_conf" . "~/.emacs.d")
			   )
      )

;; I only want this function defined if my shortcut alist exists
(if (boundp 'my-shortcuts-alist)
    (progn
      (defun dired-open-shortcut (shortcut)
	"Opens a directory referred to by a shortcut in dired"

      	;; Read the keys of the my-shortcuts-alist
	;; Applying the lambda to each list of the alist, which returns that list's car
	(interactive
	 (list
	  (completing-read "Choose shortcut: " (mapcar 'car my-shortcuts-alist))))
	
	;; Use the slected shortcut key with assoc to pull out the proper lsit from within the alist
	;; Retrieve the cdr of that list, which is the actual location to give to dired
	(dired (cdr(assoc shortcut my-shortcuts-alist)))
	)
      )
  (message "my-shortcut-alist is not defined")
  )

;;Handy keybinding
(global-set-key (kbd "C-c C-x d") 'dired-open-shortcut)
