;;; lc-vocab.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

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

(defvar lc-vocab-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode lc-vocab-mode
  "Vocab minor mode that shows meanings of unknown word for you in a buffer."
  :keymap lc-vocab-mode-map
  :lighter " LC"
  (if lc-vocab-mode
      )
  )

(provide 'lc-vocab)

;;; lc-vocab.el ends here
