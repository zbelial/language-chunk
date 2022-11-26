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
  (create-time) ;; 条目创建时间 yyyy-MM-dd
  )

;; 卡片的sm2相关信息
(cl-defstruct lc-card-sm2
  (card-id)
  (repetition) ;; 记录当前的repetition轮次
  (e-factor)
  (review-time) ;; 复习时间 yyyy-MM-dd
  (interval)
  (next-review-time) ;; 计算出来的下次复习时间
  )

(cl-defstruct lc-card-sm2-review-history
  (id)
  (card-id) ;; id in `lc-card'
  (last-repetition)
  (last-e-factor)
  (last-review-time)
  (last-interval) ;; 间隔天数
  (answer) ;; 复习时的回答
  (grade) ;; 本次复习的质量 0-5
  (repetition) ;; 本次的repetition
  (e-factor) ;; 本次的e-factor
  (review-time) ;; 复习时间 yyyy-MM-dd
  (interval) ;; 本次计算的interval
  )

;; 
(cl-defstruct lc-card-sm2-review-info
  (card-id) ;; id in `lc-card'
  (content) ;; content in `lc-card'
  (meaning) ;; meaning in `lc-card'
  (context) ;; context in `lc-card'
  (orig-context) ;; orig-context in `lc-card'
  (last-repetition) ;; 上次轮次 取自 `lc-card-sm2'
  (last-e-factor) ;; 上次的efactor 取自 `lc-card-sm2'
  (last-review-time) ;; 上次复习时间 取自 `lc-card-sm2'
  (last-interval) ;; 间隔天数 取自 `lc-card-sm2'
  (review-time) ;; 本次复习时间 yyyy-MM-dd to `lc-card-sm2-review-history'
  (answer) ;; 复习时的回答 to `lc-card-sm2-review-history'
  (grade) ;; 本次复习的质量 0-5 本次复习结果 to `lc-card-sm2-review-history'
  (repetition) ;; 本次轮次 to `lc-card-sm2-review-history'
  (e-factor) ;; 本次的efactor to `lc-card-sm2-review-history'
  (interval) ;; 本次的interval to `lc-card-sm2'
  (next-review-time) ;; 计算出来的下次复习时间 to `ls-card'
  (reviewed) ;; 是否已经复习过
  )

(cl-defstruct lc-time
  (second)
  (minute)
  (hour)
  (day)
  (month)
  (year)
  (dow)
  (dst)
  (utcoff))

(provide 'lc-struct)

;;; lc-struct.el ends here
