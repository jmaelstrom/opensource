;;set the location of the diary file
(setq diary-file (concat my-emacs-org "diary_entries"))

(setq view-diary-entries-initially t
      calendar-mark-diary-entries-flag t
      number-of-diary-entries 7)

;;all google / ical calendars to import
;; get the private iCal URLs from your Google calendar settings
(setq my-ical-calendars-alist '("test", "https://www.google.com/calendar/icaltest.ics"))


;;imports all icals defined in my-ical-calendars-alist to the emacs calendar, removing duplicates!
(import-all-remote-ical my-ical-calendars-alist diary-file)

;; non-interactive methods

(defun import-all-remote-ical (my-ical-alist my-diary-file)
  "Imports all ical urls defined in ical-alist into my-diary-file, removing duplicates"
  (message (car(car my-ical-calendars-alist )))
  (dolist (url-list-item my-ical-calendars-alist) 
    (progn
      (message "Importing %s from %s" (car url-list-item) (car(cdr url-list-item)))
      (import-remote-ical (car(cdr url-list-item)) my-diary-file)
      )
    )
  )

(defun import-remote-ical (url my-diary-file)
  "Download ics file from URL and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile my-diary-file nil)
    (kill-buffer (car (last (split-string tmpfile "/"))))
    )
  (remove-calendar-duplicates my-diary-file)
  (add-ages-to-birthdays my-diary-file)

  (save-buffer)
  (kill-buffer)

  )

(defun remove-calendar-duplicates (my-diary-file)
  "Will remove all duplicate entries from the specified calender / diary file.

The argument is a full path to said diary file."
  ;;loads the selected diary file
  (find-file my-diary-file)

  ;;turns the buffer contents into a list
  (setq buf-str-list (split-string (buffer-string) "\n"))

  ;;deletes exact duplicates in the list
  (delete-dups buf-str-list)

  ;;clears the buffer and move to point-min
  (erase-buffer)
  (goto-char (point-min))

  ;;insert each line into the buffer
  ;;mapconcat runs the func (1st arg) over the list, returning a single string
  ;;identity is a built-in that returns the value unchanged
  (insert (mapconcat 'identity buf-str-list "\n"))
  )

(defun add-ages-to-birthdays (my-diary-file)
  "Will modify the diary entries that have 'birthday' in the entry and add the %d modifier"
  (find-file my-diary-file)
  (goto-char (point-min))
  (while (= 0 (forward-line)) 
    (progn
      (setq p1 (line-beginning-position) )
      (setq p2 (line-end-position) )
      (setq myLine (buffer-substring-no-properties p1 p2))
      (when (and (string-match "birthday" myLine) (not (string-match "%d" myLine)))
	(progn
	  (goto-char (line-beginning-position))
	  (kill-line)
	  (insert (concat myLine " %d"))
	  )
	)
      )	
    )	  
  )

;;interactive methods
(defun manual-remove-calender-duplcates (my-diary-file)
  "eLisp version of remove-calendar-duplicates to be run in interactive mode."
  (interactive "fDiary file: ")
  (remove-calender-duplcates my-diary-file)
  (message "Calendar file de-duplicated!")
  )


(defun manual-import-remote-ical (url-key)
  "Imports an ICS calendar from a URL into the diary"
  (interactive
   (list
    (completing-read "Choose pre-configured calendar: " (mapcar '(lambda (arg) (car arg)) my-ical-calendars-alist))))
  (setq url-key (car(assoc url-key my-ical-calendars-alist)))
  (setq url-val (car(cdr(assoc url-key my-ical-calendars-alist))))
  (if (boundp 'diary-file) 
      (progn
	(message "Calendar to import: %s from %s" url-key url-val)
	(import-remote-ical url-val diary-file)
	) 
    (message "my-diary-file is not defined - aborting import for: %s" url-key)
    )
  )

