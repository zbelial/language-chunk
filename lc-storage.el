;;; lc-storage.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

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

(require 'lc-db)

(defun lc-storage--save-new-card (card card-sm2)
  "Save a new card to storage."
  (lc-db--new-card card card-sm2 t))

(defun lc-storage--load-cards-to-review (limit)
  "Load all cards from storage to review.
`limit' limits how many to load. If it's not positive, loads all."
  (lc-db--load-cards-to-review limit t))

(defun lc-storage--load-cards ()
  "Load all cards from storage."
  (lc-db--load-cards t))

(defun lc-storage--load-cards-today ()
  "Load all cards from storage."
  (let ((start-time (concat (lc--date) " 00:00:00"))
        (end-time (lc--datetime)))
    (lc-db--load-cards-in-period start-time end-time t)))

(defun lc-storage--save-review-history (review-info)
  "Save review information into storage."
  ;; 避免重复保存
  (if (lc-db--query-card-sm2 (lc-card-sm2-review-info-card-id review-info)
                             (lc-card-sm2-review-info-repetition review-info))
      (user-error "Card review history has been saved.")
    (lc-db--save-review-info review-info t)))


(provide 'lc-storage)

;;; lc-storage.el ends here
