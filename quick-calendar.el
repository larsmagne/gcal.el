;;; quick-calendar.el --- Adding entries to Google Calendar -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; quick-calendar.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:


;;; Code:

(require 'cl-lib)

(defvar quick-calendar-days
  '((0 "sun" "dim" "søn")
    (1 "mon" "lun" "man")
    (2 "tue" "mar" "tir")
    (3 "wed" "mer" "ons")
    (4 "thu" "jeu" "tor")
    (5 "fri" "ven" "fre")
    (6 "sat" "sam" "lør"))
  "The first three letters of weekdays in the languages you want to support.")

(defvar quick-calendar-months
  '((1  "jan" "jan" "jan")
    (2  "feb" "fev" "feb")
    (3  "mar" "mar" "mar")
    (4  "apr" "avr" "apr")
    (5  "may" "mai" "mai")
    (6  "jun" "juin" "jun")
    (7  "jul" "juil" "jul")
    (8  "aug" "aug" "aug")
    (9  "sep" "sep" "sep")
    (10 "oct" "oct" "oct")
    (11 "nov" "nov" "nov")
    (12 "dec" "dec" "dev")))

(defun quick-calendar-parse (string)
  "Parse STRING into an ISO 8601 time string.
Valid formats are:

mon 14 (the following Monday at 14:00)
vendredi 930 (the following Friday at 09:30)
aug 3 9 (the following August 3rd at 09:00)"
  (let* ((bits (split-string string nil nil split-string-default-separators))
	 (day (cl-loop for (day . names) in quick-calendar-days
		       when (member (string-limit string 3) names)
		       return day)))
    (if day
	;; We have the day; parse the rest as the time.
	(cl-loop with target = (decode-time)
		 when (= day (decoded-time-weekday target))
		 return (quick-calendar--fill-clock target (cadr bits))
		 do
		 (setq target
		       ;; Update weekday.
		       (decode-time
			(encode-time 
			 (decoded-time-add
			  target (make-decoded-time :day 1))))))
      )))

(defun quick-calendar--fill-clock (target time)
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
    target))

(provide 'quick-calendar)

;;; quick-calendar.el ends here.
