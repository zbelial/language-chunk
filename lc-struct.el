;;; lc-struct.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'cl-generic)

;; 添加的卡片
(cl-defstruct lc-card
  (id)
  (content) ;; 待记忆内容，以英语为例，就是单词或词伙 
  (meaning) ;; content的意思
  (context) ;; content出现的上下文句子（抠除content之后）
  (orig-context) ;; 未抠除content的上下文句子
  (create-time) ;; 条目创建时间
  )

(cl-defstruct lc-card-sm2
  (card-id)
  )

(cl-defstruct ls-card-review-history
  (card-id) ;; id in `ls-card'
  (review-time) ;; 
  )

(provide 'lc-struct)

;;; lc-struct.el ends here