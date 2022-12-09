;;; lc-db.el --- Package used to help capture and remember English words and chunks. -*- lexical-binding: t -*-

;; Author: zbelial(zjyzhaojiyang@gmail.om)
;; Maintainer: zbelial
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))
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
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'lc-struct)

;;;; Options
(defcustom lc-db-location (expand-file-name "lc.db" user-emacs-directory)
  "The full path to file where the language chunk database is stored.
If this is non-nil, the language chunk sqlite database is saved here."
  :type 'string
  :group 'language-chunk)

(defconst lc-db--version 1)

(defvar lc-db--connection nil
  "Database connection to language chunk database.")

(defun lc-db--get-connection ()
  "Return the database connection, if any."
  lc-db--connection)

(defun lc-db ()
  "Entrypoint to the language chunk sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (lc-db--get-connection)
               (emacsql-live-p (lc-db--get-connection)))
    (let ((init-db (not (file-exists-p lc-db-location))))
      (make-directory (file-name-directory lc-db-location) t)
      (let ((conn (emacsql-sqlite3 lc-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (setq lc-db--connection conn)
        (lc-db--init conn))))
  (lc-db--get-connection))

;;;; Entrypoint: (lc-db-exec)
(defun lc-db-exec (sql &rest args)
  "Run SQL query on language chunk database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (let ((db (lc-db)))
    (unless db
      (user-error "db is nil."))
    (if  (stringp sql)
        (emacsql db (apply #'format sql args))
      (apply #'emacsql db sql args))))


;;;; Schema
(defconst lc-db--table-schema
  '(
    (tbl_card
     [
      (id :primary-key)
      (content) ;; 待记忆内容，以英语为例，就是单词或词伙 
      (meaning) ;; content的意思
      (context) ;; content出现的上下文句子（抠除content之后）
      (orig_context) ;; 未抠除content的上下文句子
      (create_time) ;; 条目创建时间 yyyy-MM-dd HH:mm:ss
      ]
     )

    (tbl_card_sm2
     [
      (card_id :unique :primary-key)
      (repetition)
      (e_factor)
      (review_time) ;; 复习时间 yyyy-MM-dd HH:mm:ss
      (interval) ;; 间隔天数
      (next_review_time) ;; 计算出来的下次复习时间      
      ]
     )

    (tbl_card_sm2_review_history
     [
      (id integer :primary-key :autoincrement)
      (card_id) ;; id in `ls-card'
      (last_repetition)
      (last_e_factor)
      (last_review_time)
      (last_interval) ;; 间隔天数
      (answer)
      (grade) ;; 本次复习的质量 0-5
      (repetition)
      (e_factor)
      (review_time) ;; 复习时间 yyyy-MM-dd HH:mm:ss
      (interval)
      ]
     )

    (tbl_vocab
     [
      (id :primary-key)
      (vocab) ;; 词汇
      (meaning) ;; vocab的意思 多个释义用分号(;)分割
      (create_time) ;; 创建时间 yyyy-MM-dd HH:mm:ss
      (status) ;; 0 生词 1 熟词
      ]
     )))

(defun lc-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) lc-db--table-schema)
      (emacsql db [:create-table :if-not-exists $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" lc-db--version))))

(defun lc-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `stock-directory'."
  (let ((db (or db (lc-db--get-connection))))
    (when (and db (emacsql-live-p db))
      (emacsql-close db))
    (setq lc-db--connection nil)))

(defun lc-db--new-card (card card-sm2 &optional close)
  (emacsql-with-transaction (lc-db)
    (lc-db--insert-card (lc-card-id card)
                        (lc-card-content card)
                        (lc-card-meaning card)
                        (lc-card-context card)
                        (lc-card-orig-context card)
                        (lc-card-create-time card))
    (lc-db--insert-card-sm2 (lc-card-sm2-card-id card-sm2)
                            (lc-card-sm2-repetition card-sm2)
                            (lc-card-sm2-e-factor card-sm2)
                            (lc-card-sm2-review-time card-sm2)
                            (lc-card-sm2-interval card-sm2)
                            (lc-card-sm2-next-review-time card-sm2)))
  (when close
    (lc-db--close)))

(defun lc-db--insert-card (card-id content meaning context orig-context create-time &optional close)
  (lc-db-exec [:insert :into tbl_card [id content meaning context orig_context create_time] :values $v1]
              (vector card-id content meaning context orig-context create-time))
  (when close (lc-db--close)))

(defun lc-db--insert-card-sm2 (card-id repetition e-factor review-time interval next-review-time &optional close)
  (lc-db-exec [:insert :into tbl_card_sm2 [card_id repetition e_factor review_time interval next_review_time] :values $v1]
              (vector card-id repetition e-factor review-time interval next-review-time))
  (when close
    (lc-db--close)))

(defun lc-db--update-card-sm2 (card-id repetition e-factor review-time interval next-review-time &optional close)
  (lc-db-exec [:update tbl_card_sm2 :set [(= repetition $s1) (= e_factor $s2) (= review_time $s3) (= interval $s4) (= next_review_time $s5)]
                       :where (= card_id $s6)]
              repetition e-factor review-time interval next-review-time card-id)
  (when close
    (lc-db--close)))

(defun lc-db--query-card-sm2 (card-id repetition &optional close)
  (let (cards-sm2
        card-sm2)
    (setq cards-sm2 (lc-db-exec [:select [card_id repetition e_factor review_time interval next_review_time] :from tbl_card_sm2 :where (and (= card_id $s1) (= repetition $s2))] card-id repetition))
    (when cards-sm2
      (setq card-sm2 (lc-db--card-sm2-to-struct (nth 0 cards-sm2))))
    (when close
      (lc-db--close))
    card-sm2))

(defun lc-db--save-review-info (review-info &optional close)
  (emacsql-with-transaction (lc-db)
    (lc-db--update-card-sm2 (lc-card-sm2-review-info-card-id review-info)
                            (lc-card-sm2-review-info-repetition review-info)
                            (lc-card-sm2-review-info-e-factor review-info)
                            (lc-card-sm2-review-info-review-time review-info)
                            (lc-card-sm2-review-info-interval review-info)
                            (lc-card-sm2-review-info-next-review-time review-info))
    (lc-db--insert-card-sm2-review-history (lc-card-sm2-review-info-card-id review-info)
                                           (lc-card-sm2-review-info-last-repetition review-info)
                                           (lc-card-sm2-review-info-last-e-factor review-info)
                                           (lc-card-sm2-review-info-last-review-time review-info)
                                           (lc-card-sm2-review-info-last-interval review-info)
                                           (lc-card-sm2-review-info-answer review-info)
                                           (lc-card-sm2-review-info-grade review-info)
                                           (lc-card-sm2-review-info-repetition review-info)
                                           (lc-card-sm2-review-info-e-factor review-info)
                                           (lc-card-sm2-review-info-review-time review-info)
                                           (lc-card-sm2-review-info-interval review-info)))
  (when close
    (lc-db--close)))

(defun lc-db--insert-card-sm2-review-history (card-id last-repetition last-e-factor last-review-time last-interval answer grade repetition e-factor review-time interval &optional close)
  (lc-db-exec [:insert :into tbl_card_sm2_review_history [card_id last_repetition last_e_factor last_review_time last_interval answer grade repetition e_factor review_time interval] :values $v1]
              (vector card-id last-repetition last-e-factor last-review-time last-interval answer grade repetition e-factor review-time interval))
  (when close
    (lc-db--close)))

(defun lc-db--card-sm2-to-struct (card-sm2)
  (make-lc-card-sm2 :card-id (nth 0 card-sm2)
                    :repetition (nth 1 card-sm2)
                    :e-factor (nth 2 card-sm2)
                    :review-time (nth 3 card-sm2)
                    :interval (nth 4 card-sm2)
                    :next-review-time (nth 5 card-sm2)))

(defun lc-db--load-card-sm2-to-review (date limit &optional close)
  (let (cards-sm2)
    (if (<= limit 0)
        (setq cards-sm2 (lc-db-exec [:select [card_id repetition e_factor review_time interval next_review_time] :from tbl_card_sm2 :where (<= next_review_time $s1) :order-by [(asc next_review_time)]] date))
      (setq cards-sm2 (lc-db-exec [:select [card_id repetition e_factor review_time interval next_review_time] :from tbl_card_sm2 :where (<= next_review_time $s1) :order-by [(asc next_review_time)] :limit $s2] date limit)))
    (when close
      (lc-db--close))
    (when cards-sm2
      (mapcar #'lc-db--card-sm2-to-struct cards-sm2))))

(defun lc-db--card-to-struct (card)
  (make-lc-card :id (nth 0 card)
                :content (nth 1 card)
                :meaning (nth 2 card)
                :context (nth 3 card)
                :orig-context (nth 4 card)
                :create-time (nth 5 card)))

(defun lc-db--load-card (card-id &optional close)
  (let (card)
    (setq card (lc-db-exec [:select [id content meaning context orig_context create_time] :from tbl_card :where (= id $s1)] card-id))
    (when close
      (lc-db--close))
    (and card (lc-db--card-to-struct (nth 0 card)))))

(defun lc-db--load-cards-to-review (limit &optional close)
  (let ((today (lc--date))
        cards-sm2 card review-info review-infos
        card-id)
    (setq cards-sm2 (lc-db--load-card-sm2-to-review today limit))
    (when cards-sm2
      (cl-dolist (card-sm2 cards-sm2)
        (setq card (lc-db--load-card (lc-card-sm2-card-id card-sm2)))
        (when card
          (setq review-info (make-lc-card-sm2-review-info :card-id (lc-card-id card)
                                                          :content (lc-card-content card)
                                                          :meaning (lc-card-meaning card)
                                                          :context (lc-card-context card)
                                                          :orig-context (lc-card-orig-context card)
                                                          :last-repetition (lc-card-sm2-repetition card-sm2)
                                                          :last-e-factor (lc-card-sm2-e-factor card-sm2)
                                                          :last-interval (lc-card-sm2-interval card-sm2)
                                                          :last-review-time (lc-card-sm2-review-time card-sm2)
                                                          :review-time (lc--date)))
          (cl-pushnew review-info review-infos))))
    (when close
      (lc-db--close))
    (nreverse review-infos)))

(provide 'lc-db)

;;; lc-db.el ends here
