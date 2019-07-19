;;; aem-tools_users.el --- functions for interfacing with AEM's JCR API
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
(require 'bui)
(require 'seq)

; Internal Files
(require 'aem-tools_accounts)
(require 'aem-tools_http)


;; Users
  ; Utility
(defun aem--add-groups-to-user (users user groups)
  ""

  (let ((userUUID (cdr-assoc 'jcr:uuid user)))
    (cons
      (cons 'groups (cons
                      (seq-filter
                        (lambda (group)
                          (seq-contains (cdr-assoc 'rep:members group) userUUID))
                        groups)
                      (cons users groups)))
      user)))


  ; Entries
(defun aem--users-get-entries (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (let* ((domain (aem--account-get-uri aem--accounts-current-active))
                      (groups                             (aem-get-groups domain))
                      (users                              (aem-get-users  domain)))
                 (mapcar
                   (lambda (user)
                     (let ((wGroups (aem--add-groups-to-user users user groups)))
                       (cons (cons 'id wGroups) wGroups)))
                   users)))
    (otherwise (error "Unknown search type: %S" searchType))))

(bui-define-entry-type aem:users
  :titles               '((jcr:path           .            "Node Path")
                          (rep:principalName  .             "Username")
                          (jcr:uuid           .                 "UUID")
                          (jcr:mixinTypes     .          "Mixin Types")
                          (jcr:created        .       "JCR Created"   )
                          (jcr:createdBy      .       "JCR Created By")
                          (jcr:lastModified   . "JCR Last Modified"   )
                          (jcr:lastModifiedBy . "JCR Last Modified By")
                          (cq:lastModified    .  "CQ Last Modified"   )
                          (cq:lastModifiedBy  .  "CQ Last Modified By"))
  :get-entries-function #'aem--users-get-entries)


  ; Info.
(defun aem--link-to-group (name entry)
  ""
  (let ((groupData (bui-entry-value entry 'groups)))
    (seq-do
      (lambda (group)
        (insert-text-button
          (cdr-assoc 'rep:authorizableId group)
          'group-props group
          'users       (cadr groupData)
          'groups      (cddr groupData)
          'action      (lambda (btn)
                         (aem--groups-list-describe
                           (aem--add-users-to-group
                             (button-get btn 'groups)
                             (button-get btn 'group-props)
                             (button-get btn 'users)))))

        (insert ", "))
      (car groupData))))

(bui-define-interface aem:users info
  :format '((rep:principalName  format           (format))
            (jcr:uuid           format           (format))
            (groups             format aem--link-to-group)
            (jcr:mixinTypes     format           (format))
            (jcr:path           format           (format))
            nil
            (jcr:created        format             (time))
            (jcr:createdBy      format           (format))
            nil
            (jcr:lastModified   format             (time))
            (jcr:lastModifiedBy format           (format))
            nil
            (cq:lastModified    format             (time))
            (cq:lastModifiedBy  format           (format))))

(defun aem--users-list-describe (&rest users)
  "Display 'info' buffer for USERS."

  (bui-get-display-entries 'aem:users 'info (cons 'id users)))


  ; List
(bui-define-interface aem:users list
  :buffer-name       "*Users*"
  :describe-function 'aem--users-list-describe
  :format            '((rep:principalName  nil 32 t)
                       (jcr:created        nil 32 t)
                       (jcr:createdBy      nil 25 t)
                       (jcr:lastModified   nil 32 t)
                       (jcr:lastModifiedBy nil 25 t))
  :sort-key          '(rep:principalName))


  ; Interactive Function to Call
(defun aem-users ()
  "Display a list of AEM users for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:users))






;; Groups
  ; Utility
(defun aem--add-users-to-group (groups group users)
  ""

  (cons
    (cons 'users (cons
                   (seq-filter
                     (lambda (user) (seq-contains
                                      (cdr-assoc 'rep:members group)
                                      (cdr-assoc 'jcr:uuid     user)))
                     users)
                   (cons groups users)))
    group))


  ; Entries
(defun aem--groups-get-entries (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (let* ((domain (aem--account-get-uri aem--accounts-current-active))
                      (users                              (aem-get-users  domain))
                      (groups                             (aem-get-groups domain)))
                 (mapcar
                   (lambda (group)
                     (let ((wUsers (aem--add-users-to-group groups group users)))
                       (cons (cons 'id wUsers) wUsers)))
                   groups)))
    (otherwise (error "Unknown search type: %S" searchType))))

(bui-define-entry-type aem:groups
  :titles               '((jcr:path           .            "Node Path")
                          (rep:principalName  .           "Group name")
                          (jcr:uuid           .                 "UUID")
                          (jcr:mixinTypes     .          "Mixin Types")
                          (rep:members        .         "User Members")
                          (jcr:created        .       "JCR Created"   )
                          (jcr:createdBy      .       "JCR Created By")
                          (jcr:lastModified   . "JCR Last Modified"   )
                          (jcr:lastModifiedBy . "JCR Last Modified By")
                          (cq:lastModified    .  "CQ Last Modified"   )
                          (cq:lastModifiedBy  .  "CQ Last Modified By"))
  :get-entries-function #'aem--groups-get-entries)


  ; Info.
(defun aem--link-to-user (name entry)
  ""

  (let ((userData (bui-entry-value entry 'users)))
    (seq-do
      (lambda (user)
        (insert-text-button
          (cdr-assoc 'rep:authorizableId user)
          'user-props user
          'groups     (cadr userData)
          'users      (cddr userData)
          'action     (lambda (btn)
                        (aem--users-list-describe
                          (aem--add-groups-to-user
                            (button-get btn 'users)
                            (button-get btn 'user-props)
                            (button-get btn 'groups)))))

        (insert ", "))
      (car userData))))

(bui-define-interface aem:groups info
  :format '((rep:principalName  format          (format))
            (jcr:uuid           format          (format))
            (users              format aem--link-to-user)
            (jcr:mixinTypes     format          (format))
            (jcr:path           format          (format))
            nil
            (jcr:created        format            (time))
            (jcr:createdBy      format          (format))
            nil
            (jcr:lastModified   format            (time))
            (jcr:lastModifiedBy format          (format))
            nil
            (cq:lastModified    format            (time))
            (cq:lastModifiedBy  format          (format))))

(defun aem--groups-list-describe (&rest groups)
  "Display 'info' buffer for GROUPS."

  (bui-get-display-entries 'aem:groups 'info (cons 'id groups)))


  ; List
(bui-define-interface aem:groups list
  :buffer-name       "*Groups*"
  :describe-function 'aem--groups-list-describe
  :format            '((rep:principalName  nil 34 t)
                       (jcr:path           nil 55 t)
                       (jcr:lastModified   nil 32 t)
                       (jcr:lastModifiedBy nil 25 t))
  :sort-key          '(rep:principalName))
  :sort-key          '(rep:principalName))


  ; Interactive Function to Call
(defun aem-groups ()
  "Display a list of AEM groups for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:groups))
