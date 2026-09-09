;;; gcal.el --- Adding entries to Google Calendar -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; gcal.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; This is a small package to enter events into a Google calendar.

;; The main entry point is `gcal-add'.  See the doc string
;; for documentation on the date/time formats accepted.

;;; Code:

(require 'cl-lib)

(defvar gcal-name nil
  "The name of the calendar.")

(defvar gcal-days
  '((0 "sun" "dim" "søn")
    (1 "mon" "lun" "man")
    (2 "tue" "mar" "tir")
    (3 "wed" "mer" "ons")
    (4 "thu" "jeu" "tor")
    (5 "fri" "ven" "fre")
    (6 "sat" "sam" "lør"))
  "The first three letters of weekdays in the languages you want to support.")

(defvar gcal-months
  '((1  "jan" "jan"  "jan" "j")
    (2  "feb" "fev"  "feb" "f")
    (3  "mar" "mar"  "mar")
    (4  "apr" "avr"  "apr" "ap" "av")
    (5  "may" "mai"  "mai")
    (6  "jun" "juin" "jun")
    (7  "jul" "juil" "jul")
    (8  "aug" "aug"  "aug" "au")
    (9  "sep" "sep"  "sep" "s")
    (10 "oct" "oct"  "oct" "o")
    (11 "nov" "nov"  "nov" "n")
    (12 "dec" "dec"  "des" "d")))

(defun gcal-add ()
  "Prompt the WHEN and TITLE and add to the calendar.

Valid formats are:

mon 14 (the following Monday at 14:00)
vendredi 930 (the following Friday at 09:30)
aug 3 9 (the following August 3rd at 09:00)
13 9 (the following 13th in this or the next month at 09:00)"
  (interactive)
  (unless gcal-name
    (user-error
     "You have to set `gcal-name' to the calendar name first."))
  (cl-destructuring-bind (time title)
      (gcal--parse-1 (read-string "Time and event: "))
    (if (y-or-n-p (format "Add %S at %s? "
			  title (format-time-string
				 "%A %F %H:%M" (encode-time time))))
	(gcal--add title
			     (format-time-string "%FT%T" (encode-time time)))
      (message "Didn't add anything"))))

(defun gcal-parse (string)
  "Parse STRING into an ISO 8601 time string.
Valid formats are:

mon 14 (the following Monday at 14:00)
vendredi 930 (the following Friday at 09:30)
aug 3 9 (the following August 3rd at 09:00)
13 9 (the following 13th in this or the next month at 09:00)"
  (car (gcal--parse-1 string)))

(defun gcal--parse-1 (string)
  (let* ((bits (split-string (downcase string)
			     nil nil split-string-default-separators))
	 (day (cl-loop for (day . names) in gcal-days
		       when (member (string-limit (car bits) 3) names)
		       return day))
	 month)
    (cond
     (day
      ;; We have the day; find the next date and parse the rest as
      ;; the time.
      (cl-loop with target = (decode-time)
	       when (= day (decoded-time-weekday target))
	       return (list (gcal--fill-clock target (cadr bits))
			    (string-join (cddr bits) " "))
	       do
	       (setq target
		     ;; Update weekday.
		     (decode-time
		      (encode-time 
		       (decoded-time-add
			target (make-decoded-time :day 1)))
		      (decoded-time-zone target)))))
     ((string-match-p "\\`[0-9]+\\'" (car bits))
      ;; We have a numerical day-of-the-month in the current or next month.
      (cl-loop with date = (string-to-number (car bits))
	       with target = (decode-time)
	       when (= date (decoded-time-day target))
	       return (list (gcal--fill-clock target (cadr bits))
			    (string-join (cddr bits) " "))
	       do
	       (setq target (decoded-time-add
			     target (make-decoded-time :day 1)))))
     ((setq month
	    (cl-loop for (month . names) in gcal-months
		     for result =
		     (cl-loop for name in names
			      when (equal (string-limit
					   (car bits) (length name))
					  name)
			      return month)
		     when result
		     return result))
      ;; We have the month name.
      (let ((target (decode-time)))
	;; If we're in November and the string indicated February,
	;; then that's next year.
	(when (> (decoded-time-month target) month)
	  (setq target (decoded-time-add target (make-decoded-time :year 1))))
	(setf (decoded-time-month target) month)
	;; Then the next thing must be the day in that month.
	(setf (decoded-time-day target) (string-to-number (cadr bits)))
	;; Finally fill in the clock.
	(list (gcal--fill-clock target (caddr bits))
	      (string-join (cdddr bits) " "))))
     (t
      (error "Unable to parse this time: %s" string)))))

(defun gcal--fill-clock (target time)
  (let (hour (minute 0))
    (cond
     ((<= (length time) 2)
      (setq hour (string-to-number time)))
     ((= (length time) 3)
      (setq hour (string-to-number (substring time 0 1))
	    minute (string-to-number (substring time 1))))
     ((= (length time) 4)
      (setq hour (string-to-number (substring time 0 2))
	    minute (string-to-number (substring time 2))))
     (t (error "Invalid clock: %s" time)))
    (setf (decoded-time-second target) 0)
    (setf (decoded-time-minute target) minute)
    (setf (decoded-time-hour target) hour)
    ;; Recompute the day-of-week.
    (decode-time (encode-time target) (decoded-time-zone target))))

(defun gcal--add (title when &optional duration)
  (with-temp-buffer
    (call-process "gcalcli" nil t nil
		  "add"
		  "--noprompt"
		  "--calendar" gcal-name
		  "--title" title
		  "--when" when
		  "--duration" (format "%s" (or duration "60")))
    (if (zerop (buffer-size))
	(message "Added %s at %s to the calendar" title when)
      (message "Error when adding: %s" (buffer-string)))))

(provide 'gcal)

;;; gcal.el ends here.
