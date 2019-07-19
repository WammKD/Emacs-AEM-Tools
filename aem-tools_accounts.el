;;; aem-tools_accounts.el --- functions for interfacing with AEM's JCR API
;; Copyright (C) 2019

;; Author: Jonathan Schmeling <jaft.r@outlook.com>
;; Keywords: api, aem, jcr
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program; see the file LICENSE.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'seq)
(require 'url)


;; Accounts
(defconst aem--accounts                (list)
  "List of AEM server credentials.")
(defconst aem--accounts-current-active (car aem--accounts)
  "AEM server credentials to use when using AEM tools.")


  ; Account Struct.
(cl-defstruct (aem--account (:constructor aem--account-create)
                            (:conc-name   aem--account-get-))
  (domain                        "http://localhost")
  (port                                      "4502")
  (auth-base64 (base64-encode-string "admin:admin"))
  (alias                         "localhost_author"))

(defun aem--account-get-uri (account)
  "Create a proper form for a URI from the domain and port of an account."

  (concat (aem--account-get-domain account) ":" (aem--account-get-port account)))


  ; User tools to handle Accounts
(defun aem-account-create (domain port username password alias)
  "Create an account for use with AEM tools from AEM server credentials."

  (let ((account (aem--account-create :domain      domain
                                      :port        port
                                      :auth-base64 (base64-encode-string
                                                     (concat username ":" password))
                                      :alias       alias)))
    (add-to-list 'aem--accounts account)

    (setq url-http-real-basic-auth-storage (cons
                                             (list
                                               (replace-regexp-in-string
                                                 "^http://"
                                                 ""
                                                 (aem--account-get-uri account))
                                               (cons
                                                 (aem--account-get-alias account)
                                                 (aem--account-get-auth-base64 account)))
                                             url-http-real-basic-auth-storage))))

(defun aem-account-choose-active ()
  ""
  (interactive)

  (let ((chosen (ido-completing-read "Choose account: " (mapcar
                                                          'aem--account-get-alias
                                                          aem--accounts))))
    (setq aem--accounts-current-active (car (seq-filter
                                              (lambda (alias)
                                                (string-equal
                                                  (aem--account-get-alias alias)
                                                  chosen))
                                              aem--accounts)))))
