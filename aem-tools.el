;;; aem-tools.el --- functions for interfacing with AEM's JCR API
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
(require 'aem-tools_accounts)
(require 'aem-tools_bundles)
(require 'aem-tools_crxde)
(require 'aem-tools_groovy)
(require 'aem-tools_packages)
(require 'aem-tools_users)


;; Accounts
  ; Create default local accounts
(aem-account-create "http://localhost" "4502" "admin" "admin" "localhost_author")
(aem-account-create "http://localhost" "4503" "admin" "admin" "localhost_publish")

(provide 'aem-tools)

;;; aem-tools.el ends here
