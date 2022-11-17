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

(defvar lc-memo-review-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    
    (define-key map (kbd "C-c C-c") nil)
    (define-key map (kbd "C-c C-k") nil)
    map))

(provide 'lc-memo)

;;; lc-memo.el ends here
