;;; ls-util.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

;; Author: zbelial(zjyzhaojiyang@gmail.om)
;; Maintainer: zbelial
;; Version: 0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/zbelial/language-chunk
;; Keywords: english


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'cl-macs)
(require 'lc-const)
(require 'lc-struct)

(defsubst lc--current-time()
  ""
  (time-convert nil 'integer))

(defsubst lc--card-id ()
  "Note id."
  (+ (* (lc--current-time) 1000) (random 999)))

(defun lc--time-from-str (str)
  (let ((tm (parse-time-string str))
        lc-time)
    (setq lc-time (make-lc-time :second (or (nth 0 tm) 0)
                                :minute (or (nth 1 tm) 0)
                                :hour (or (nth 2 tm) 0)
                                :day (or (nth 3 tm) 1)
                                :month (or (nth 4 tm) 1)
                                :year (nth 5 tm)
                                :dow (nth 6 tm)
                                :dst (nth 7 tm)
                                :utcoff (nth 8 tm)
                                ))
    (unless (lc-time-year lc-time)
      (error (format "Invalid time string %s" str)))
    lc-time))

(defun lc--datetime(&optional timestamp)
  (let ((timestamp (or timestamp (time-convert nil 'integer))))
    (format-time-string LC-DATETIME-FORMAT timestamp)))

(defun lc--date(&optional timestamp)
  (let ((timestamp (or timestamp (time-convert nil 'integer))))
    (format-time-string LC-DATE-FORMAT timestamp)))

(defun lc--parse-datetime(dt-str)
  "dt-str is in the format of yyyy-MM-dd HH:mm:ss."
  (let ((dt (parse-time-string dt-str)))
    (time-convert (encode-time dt) 'integer)))

(cl-defun lc--date-plus(date &key year month day hour minute second)
  (if (or year month day hour minute second)
      (let* ((tm (lc--time-from-str date))
             ts new-date
             )
        (setq ts (time-convert (encode-time (+ (lc-time-second tm) (or second 0))
                                            (+ (lc-time-minute tm) (or minute 0))
                                            (+ (lc-time-hour tm) (or hour 0))
                                            (+ (lc-time-day tm) (or day 0))
                                            (+ (lc-time-month tm) (or month 0))
                                            (+ (lc-time-year tm) (or year 0)))))
        (setq new-date (format-time-string LC-DATETIME-FORMAT ts))
        (setq new-date (substring new-date 0 (length date)))
        new-date)
    date))

(defun lc--make-string (element count)
  (let ((str "")
        (idx 0))
    (while (< idx count)
      (setq str (concat str element)
            idx (1+ idx)))
    str))

(provide 'lc-util)

;;; ls-util.el ends here
