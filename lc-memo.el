;;; lc-memo.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

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

;; 目前使用sm2算法 https://super-memory.com/english/ol/sm2.htm
(require 'cl-lib)
(eval-when-compile
  (require 'cl-macs))
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'lc-const)

(defcustom lc-memo-batch-count 50
  "How many chunks to remember everyday.
A non-positive number means to remember all of what should be remembered."
  :type 'integer
  :group 'language-chunk
  )

(defface lc-memo--button-default-face
  '((t (:inherit button :height 1.0 :underline nil :weight normal :box (:style released-button))))
  "")

(defface lc-memo--button-selected-face
  '((t (:inherit button :height 1.0 :box (:color "orange" :style pressed-button) :underline t :weight bold)))
  "")

(defun lc-memo-sm2-calc-efactor (old-ef q)
  "Calculate EFactor based on current EFactor `old-ef' and response quanlity `q'."
  (let (ef)
    (setq ef (+ old-ef -0.8 (* 0.28 q) (- (* 0.02 q q))))
    (when (< ef LC-MIN-EFACTOR)
      (setq ef LC-MIN-EFACTOR))
    (when (> ef LC-MAX-EFACTOR)
      (setq ef LC-MAX-EFACTOR))
    ef))

(defvar lc-memo-review-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (suppress-keymap map t)
    
    (define-key map (kbd "C-c n") #'lc-memo--next-card)
    (define-key map (kbd "C-c p") #'lc-memo--previous-card)
    (cl-dolist (k '("0" "1" "2" "3" "4" "5"))
      (define-key map (kbd k) #'lc-memo--grade))

    map))

(define-minor-mode lc-memo-review-mode
  "Card review minor mode."
  :keymap lc-memo-review-mode-map
  :lighter " LC"
  (if lc-memo-review-mode
      (progn
        (display-line-numbers-mode -1)
        )
    )
  )

(defun lc-memo--grade ()
  (interactive)
  (let ((keys (this-command-keys))
        grade)
    (message "keys %s" keys)
    (if lc-memo--review-local-grade-widget
        (progn
          (setq grade (format "%s" keys))
          (if (member grade '("0" "1" "2" "3" "4" "5"))
              (progn
                (setq lc-memo--review-local-grade (string-to-number grade))
                (widget-value-set lc-memo--review-local-grade-widget lc-memo--review-local-grade)
                (widget-setup))
            (user-error "Grade must be in [0-5].")))
      (user-error "Input your answer before grading."))))

(defun lc-memo--calc-next-interval (repetition last-interval e-factor)
  (let (interval)
    (setq interval (cond
                    ((<= repetition 1)
                     1)
                    ((= repetition 2)
                     6)
                    (t
                     (round (* last-interval e-factor))
                     )))
    interval))

(defun lc-memo--save-review-info (review-info)
  (lc-storage--save-review-history review-info))

(defun lc-memo--change-card(&optional forwardp)
  (let ((current-card (gethash lc-memo--current-reviewed-card-idx lc-memo--cards-map))
        target-card
        reviewed last-repetition last-e-factor last-interval
        e-factor grade interval)
    (setq reviewed (lc-card-sm2-review-info-reviewed current-card)
          last-repetition (lc-card-sm2-review-info-last-repetition current-card)
          last-e-factor (lc-card-sm2-review-info-last-e-factor current-card)
          last-interval (lc-card-sm2-review-info-last-interval current-card)
          grade lc-memo--review-local-grade)
    (setq e-factor (lc-memo-sm2-calc-efactor last-e-factor grade)
          interval (lc-memo--calc-next-interval (1+ last-repetition) last-interval e-factor))
    (unless reviewed
      (setf (lc-card-sm2-review-info-reviewed current-card) t)
      (setf (lc-card-sm2-review-info-answer current-card) lc-memo--review-local-answer)
      (setf (lc-card-sm2-review-info-grade current-card) lc-memo--review-local-grade)
      (setf (lc-card-sm2-review-info-repetition current-card) (1+ last-repetition))
      (setf (lc-card-sm2-review-info-review-time current-card) (lc--date))
      (setf (lc-card-sm2-review-info-e-factor current-card) e-factor)
      (setf (lc-card-sm2-review-info-interval current-card) interval)
      (setf (lc-card-sm2-review-info-next-review-time current-card) (lc--date-plus (lc--date) :day interval))

      (puthash lc-memo--current-reviewed-card-idx current-card lc-memo--cards-map)
      (lc-memo--save-review-info current-card))
    (cond
     (forwardp
      (if (= lc-memo--current-reviewed-card-idx (- lc-memo--card-total-count 1))
          (message "No more cards to review.")
        (setq lc-memo--current-reviewed-card-idx (+ lc-memo--current-reviewed-card-idx 1))
        (setq target-card (gethash lc-memo--current-reviewed-card-idx lc-memo--cards-map))))
     (t
      (if (= lc-memo--current-reviewed-card-idx 0)
          (message "No more cards to review.")
        (setq lc-memo--current-reviewed-card-idx (- lc-memo--current-reviewed-card-idx 1))
        (setq target-card (gethash lc-memo--current-reviewed-card-idx lc-memo--cards-map)))))
    target-card))

(defun lc-memo--next-card ()
  (interactive)
  (let (target-card)
    (if (null lc-memo--review-local-grade)
        (message "Please input answer and then grade the answer first.")
      (setq target-card (lc-memo--change-card t))
      (when target-card
        (lc-memo--review-show-card target-card)))))

(defun lc-memo--previous-card ()
  (interactive)
  (let (target-card)
    (if (null lc-memo--review-local-grade)
        (message "Please input answer and then grade the answer first.")
      (setq target-card (lc-memo--change-card nil))
      (when target-card
        (lc-memo--review-show-card target-card)))))

(defun lc-memo--load-cards ()
  (lc-storage--load-cards-to-review lc-memo-batch-count))

(defvar lc-memo--buffer-name "*lc memo*")
(defvar lc-memo--current-reviewed-card-idx nil "当前正在复习的卡片")
(defvar lc-memo--card-total-count nil)
(defvar lc-memo--cards-map (make-hash-table :test #'equal) "所有待复习卡片的hash。索引是数字。")

(defvar-local lc-memo--review-local-card nil)
(defvar-local lc-memo--review-local-grade nil)
(defvar-local lc-memo--review-local-answer nil)
(defvar-local lc-memo--review-local-e-factor nil)
(defvar-local lc-memo--review-local-grade-widget nil)
(defvar-local lc-memo--review-local-content-widget nil)
(defvar-local lc-memo--review-local-answer-widget nil)

(defun lc-memo--widget-move (orig-fn arg &optional suppress-echo)
  (ignore-errors
    (apply orig-fn arg suppress-echo)))
(advice-add #'widget-move :around #'lc-memo--widget-move)

(cl-defun lc-memo--review-show-card (card)
  (unless card
    (message "No more cards.")
    (cl-return-from lc-memo--review-show-card nil))
  
  (let ((label-length 16)
        card-id reviewed grade answer
        meaning context content
        at-answer-widget-p)
    (setq card-id (lc-card-sm2-review-info-card-id card)
          meaning (lc-card-sm2-review-info-meaning card)
          context (lc-card-sm2-review-info-context card)
          content (lc-card-sm2-review-info-content card)
          reviewed (lc-card-sm2-review-info-reviewed card)
          grade (lc-card-sm2-review-info-grade card)
          answer (lc-card-sm2-review-info-answer card))
    (with-current-buffer (get-buffer-create lc-memo--buffer-name)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)

      (setq lc-memo--review-local-card card
            lc-memo--review-local-grade nil
            lc-memo--review-local-answer nil
            lc-memo--review-local-e-factor nil)
      (goto-char (point-min))
      (widget-insert (format "\t\t\t\t卡片复习 (%d/%d)."
                             lc-memo--current-reviewed-card-idx lc-memo--card-total-count))
      (widget-insert "\n\n\n\n")
      (widget-insert (format "%s : %s" (truncate-string-to-width "含义" label-length nil ?\s) meaning))
      (widget-insert "\n\n")
      (widget-insert (format "%s : %s" (truncate-string-to-width "上下文" label-length nil ?\s) context))
      (widget-insert "\n\n")
      (if (not reviewed)
          (progn
            (setq lc-memo--review-local-answer-widget
                  (widget-create 'editable-field
                                 :size 64
                                 :format (concat (truncate-string-to-width "回答" label-length nil ?\s) " : %v") ; Text after the field!
                                 :action (lambda (widget &rest args)
                                           (setq lc-memo--review-local-answer (widget-value widget))
                                           (when (eq (compare-strings lc-memo--review-local-answer nil nil content nil nil t) t)
                                             (widget-value-set lc-memo--review-local-grade-widget 5)
                                             (widget-value-set lc-memo--review-local-content-widget content)
                                             (setq lc-memo--review-local-grade 5))
                                           (widget-setup))))
            (widget-insert "\n\n")
            (setq lc-memo--review-local-content-widget
                  (widget-create 'item
                                 :format (concat (truncate-string-to-width "原内容" label-length nil ?\s) " : %v" )
                                 :value ""))
            (widget-insert "\n\n")
            (setq lc-memo--review-local-grade-widget
                  (widget-create 'item
                                 :format (concat (truncate-string-to-width "自我评分(0-5)" label-length nil ?\s) " : %v")
                                 :value -1)))
        (setq lc-memo--review-local-answer-widget
              (widget-create 'editable-field
                             :size 64
                             :format (concat (truncate-string-to-width "回答" label-length nil ?\s) " : %v") ; Text after the field!
                             :value answer))
        (widget-insert "\n\n")
        (widget-insert (format "%s : %s"
                               (truncate-string-to-width "原内容" label-length nil ?\s)
                               content))
        (widget-insert "\n\n")
        (setq lc-memo--review-local-grade-widget
              (widget-create 'item
                             :format (concat (truncate-string-to-width "自我评分(0-5)" label-length nil ?\s) " : %v")
                             :value grade))
        )
      (use-local-map widget-keymap)
      (widget-setup)      
      (lc-memo-review-mode))
    (switch-to-buffer lc-memo--buffer-name)
    (while (not at-answer-widget-p)
      (widget-forward 1)
      (when (eq (widget-at) lc-memo--review-local-answer-widget)
        (setq at-answer-widget-p t)))))

(cl-defun lc-memo-review ()
  "Start to review cards."
  (interactive)
  (setq lc-memo--current-reviewed-card-idx 0)
  (clrhash lc-memo--cards-map)
  (let ((cards (lc-memo--load-cards))
        (idx 0)
        current-card
        memo-buf)
    (when (length= cards 0)
      (message "No cards need to review.")
      (cl-return-from lc-memo-review nil))
    (setq lc-memo--card-total-count (length cards))
    (cl-dolist (c cards)
      (puthash idx c lc-memo--cards-map)
      (setq idx (1+ idx)))
    (setq current-card (gethash lc-memo--current-reviewed-card-idx lc-memo--cards-map))
    (setq memo-buf (get-buffer-create lc-memo--buffer-name))
    (lc-memo--review-show-card current-card)
    )
  )

(provide 'lc-memo)

;;; lc-memo.el ends here
