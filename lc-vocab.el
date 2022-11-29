;;; ls-vocab.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

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

(require 'thingatpt)
(require 'rx)
(require 'button)
(require 'lc-util)
(require 'lc-struct)
(require 'lc-storage)

(defcustom lc-vocab-sentence-abbrevs '("i.e." "etc." "U.S.")
  "Prevent to incorrectly determine sentence end."
  :type '(repeat string)
  :group 'language-chunk
  )

(defcustom lc-vocab-eww-sentence-ends (rx (or
                                           (and "." (or " " eol))
                                           (and "?" (or " " eol))
                                           (and "!" (or " " eol))
                                           (and ";" (or " " eol))
                                           "\n\n"))
  "A regexp used to determine where is the end of a sentence in eww."
  :type 'string
  :group 'language-chunk
  )

(defun lc-vocab--string-ends-with-any (str patterns)
  (cl-dolist (p patterns)
    (when (string-suffix-p p str t)
      (cl-return t))))

(defun lc-vocab--eww-sentence ()
  (let ((sentence-ends lc-vocab-eww-sentence-ends)
        (point (point))
        (stop nil)
        start end)
    (save-excursion
      (while (not stop)
        (setq end (search-forward-regexp sentence-ends nil t))
        ;; (message "end: %s" end)
        (if (not end)
            (setq end (point-max)
                  stop t)
          (unless (lc-vocab--string-ends-with-any (buffer-substring-no-properties point (- end 1)) lc-vocab-sentence-abbrevs)
            (setq stop t))))

      (setq stop nil)
      (goto-char point)
      (while (not stop)
        (setq start (search-backward-regexp sentence-ends nil t))
        ;; (message "start: %s" start)
        (if (not start)
            (setq start (point-min)
                  stop t)
          (unless (lc-vocab--string-ends-with-any (buffer-substring-no-properties (point-at-bol) (1+ start)) lc-vocab-sentence-abbrevs)
            (setq stop t)
            (setq start (1+ start))))))
    (string-trim (buffer-substring-no-properties start end))))

;;;###autoload
(defun lc-vocab-sentence ()
  "Used to test `lc-vocab--sentence'."
  (interactive)
  (message "%s" (lc-vocab--sentence)))

(defun lc-vocab--sentence ()
  (let (sentence)
    (cond
     ((derived-mode-p 'eww-mode)
      (setq sentence (lc-vocab--eww-sentence)))
     (t
      (setq sentence (thing-at-point 'sentence t))))
    sentence))

(defface lc-vocab--button-default-face
  '((t (:inherit button :height 1.2 :underline nil :weight normal :box (:style released-button))))
  "")

(defface lc-vocab--button-selected-face
  '((t (:inherit button :height 1.2 :box (:color "orange" :style pressed-button) :underline t :weight bold)))
  "")

(defun lc-vocab--button-callback (but)
  (let ((selected (button-get but 'selected)))
    (if selected
        (progn
          (button-put but 'selected nil)
          (button-put but 'face 'lc-vocab--button-default-face))
      (button-put but 'selected t)
      (button-put but 'face 'lc-vocab--button-selected-face))))

;;;###autoload
(defun lc-vocab-capture-card ()
  "Create a new card."
  (interactive)
  (let ((sentence (lc-vocab--sentence))
        orig-content content context meaning)
    (setq sentence (read-string "确认句子: " sentence))
    (when sentence
      (cond
       ((region-active-p)
        (setq orig-content (buffer-substring-no-properties (region-beginning) (region-end)))
        (setq context (string-replace orig-content (string-trim-right (lc--make-string "[...] " (length (split-string orig-content)))) sentence))
        (setq content (read-string "确认词伙: " orig-content))
        (setq meaning (read-string (format "输入词组含义 [%s]: " content)))
        (if (and meaning
                 (length> meaning 0)
                 content
                 (length> content 0))
            (progn
              (setq card (make-lc-card :id (lc--card-id) :content content :meaning meaning :context context :orig-context sentence :create-time (lc--datetime)))
              (lc-vocab--save-new-card card))
          (user-error "No content confirmed/meaning input, cancelling."))
        (deactivate-mark))
       (t
        (lc-vocab--show-in-carve-buffer sentence))))))

(defun lc-vocab--save-new-card (card)
  ;; (message "lc-vocab--save-new-card card: %s" card)
  (let (card-sm2)
    (setq card-sm2 (make-lc-card-sm2 :card-id (lc-card-id card)
                                     :repetition 1
                                     :e-factor 2.5
                                     :interval 1
                                     :review-time (lc--date)
                                     :next-review-time (lc--date-plus (lc--date) :day 1)))
    (lc-storage--save-new-card card card-sm2)))

(defvar lc-vocab--carve-buffer-name "*lc carve*")
(defun lc-vocab--show-in-carve-buffer (sentence)
  (let (carve-buf
        parts)
    (setq carve-buf (get-buffer-create lc-vocab--carve-buffer-name))
    (setq parts (split-string sentence))
    (with-current-buffer carve-buf
      (kill-all-local-variables)

      (make-local-variable 'orig-context)
      (setq orig-context sentence)
      
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (read-only-mode 0)
      (goto-char (point-min))
      (insert "\n")
      (insert "Click on a word or move onto it and press Enter to select it.\n")
      (insert (substitute-command-keys
               "\\<lc-vocab-carve-mode-map>Press \
`\\[lc-vocab-carve-next-button]' to move to the next word, press `\\[lc-vocab-carve-previous-button]' to move the the previous one."))
      ;; (insert "Press n to move to the next word. Press p to move to the preview word.\n")
      (insert "\n\n")
      (cl-dolist (p parts)
        (setq but (insert-button p 'action #'lc-vocab--button-callback 'face 'lc-vocab--button-default-face 'selected nil))
        (insert " "))
      (goto-char (point-min))
      (lc-vocab-carve-next-button)
      (read-only-mode 1)
      (lc-vocab-carve-mode 1))
    (pop-to-buffer carve-buf)))

;;;###autoload
(defun lc-vocab-carve-confirm ()
  "Confirm what you have selected."
  (interactive)
  ;; (message "lc-vocab-carve-confirm")
  (let ((forward t)
        but selected text orig-context context orig-content content meaning card)
    (save-excursion
      (goto-char (point-min))
      (while forward
        (setq but (forward-button 1 nil nil t))
        (if but
            (progn
              (setq selected (button-get but 'selected)
                    text (button-label but))
              (if selected
                  (progn
                    (setq orig-content (concat orig-content text " ")
                          context (concat context "[...] ")))
                (setq context (concat context text " ")))
              (setq orig-context (concat orig-context text " ")))
          (setq forward nil))))
    (setq orig-context (string-trim-right orig-context)
          orig-content (string-trim-right orig-content)
          context (string-trim-right context))
    (setq content (read-string "确认词伙: " orig-content))
    (setq meaning (read-string (format "输入词组含义 [%s]: " content)))
    (if (and meaning
             (length> meaning 0)
             content
             (length> content 0))
        (progn
          (setq card (make-lc-card :id (lc--card-id) :content content :meaning meaning :context context :orig-context orig-context :create-time (lc--datetime)))
          (lc-vocab--save-new-card card))
      (user-error "No content confirmed/meaning input, cancelling."))
    (kill-buffer-and-window)))

;;;###autoload
(defun lc-vocab-carve-cancel ()
  "Cancel carving and kill the buffer."
  (interactive)
  ;; (message "lc-vocab-carve-cancel")
  (kill-buffer-and-window))

;;;###autoload
(defun lc-vocab-carve-next-button ()
  "Move to the next button."
  (interactive)
  (forward-button 1 nil nil t))

;;;###autoload
(defun lc-vocab-carve-previous-button ()
  "Move to the previous button."
  (interactive)
  (backward-button 1 nil nil t))

(defvar lc-vocab-carve-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    
    (define-key map (kbd "C-c C-c") #'lc-vocab-carve-confirm)
    (define-key map (kbd "C-c C-k") #'lc-vocab-carve-cancel)
    (define-key map (kbd "<tab>") #'lc-vocab-carve-next-button)
    (define-key map (kbd "n") #'lc-vocab-carve-next-button)
    (define-key map (kbd "<backtab>") #'lc-vocab-carve-previous-button)
    (define-key map (kbd "p") #'lc-vocab-carve-previous-button)
    map))

(define-minor-mode lc-vocab-carve-mode
"carve minor mode."
:keymap lc-vocab-carve-mode-map
(if lc-vocab-carve-mode
    (setq-local header-line-format
                (substitute-command-keys
                 "\\<lc-vocab-carve-mode-map>Save the selected words with \
`\\[lc-vocab-carve-confirm]', or cancel with `\\[lc-vocab-carve-cancel]'."))
  (setq-local header-line-format nil)))

(provide 'lc-vocab)

;;; ls-vocab.el ends here
