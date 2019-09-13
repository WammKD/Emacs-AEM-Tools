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

(defun aem-users-create (username password)
  "Create an AEM user for an instance."
  (interactive (list
                 (read-string "New user's username: ")
                 (read-passwd "New user's password: ")))

  (let ((tbody  (assoc
                  'tbody
                  (assoc
                    'table
                    (assoc 'body (assoc 'html (aem-create-user
                                                (aem--account-get-uri
                                                  aem--accounts-current-active)
                                                username
                                                password))))))
        (get-td (lambda (label body)
                  (cadr (cdr-assoc
                          'div
                          (assoc
                            'td
                            (cdr (cdddar (seq-filter
                                           (lambda (tr)
                                             (and
                                               (listp tr)
                                               (string-equal
                                                 (caddr (cadddr tr))
                                                 label)))
                                           body)))))))))
    (pcase (funcall get-td "Status" tbody)
      ("409"     (message "Username already exists!"))
      ("201"     (pcase major-mode
                   ('aem:users-list-mode (revert-buffer nil t)

                                         (search-forward username)
                                         (move-beginning-of-line 1)

                                         (message (concat
                                                    "User "
                                                    username
                                                    " created!")))
                   ('aem:users-info-mode (bui-get-display-entries
                                           'aem:users
                                           'info
                                           (cons 'id (list
                                                       (aem-get-subnodes
                                                         (aem--account-get-uri
                                                           aem--accounts-current-active)
                                                         (funcall get-td "Path" tbody))))))
                   (otherwise            (message (concat
                                                    "User created at "
                                                    (funcall get-td "Path" tbody)
                                                    "!")))))
      (otherwise (message (concat
                            "Unknown error: "
                            (funcall get-td "Path" tbody)
                            "!"))))))

(define-key aem:users-list-mode-map (kbd "c") 'aem-users-create)
(define-key aem:users-info-mode-map (kbd "c") 'aem-users-create)


(defun aem-users-set-profile (users)
  "Set profile information (such as first/last name, E-mail, etc.) for USERS."
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let ((s (seq-reduce
             (lambda (result user)
               (let ((username (cdr-assoc 'rep:authorizableId user))
                     (path     (cdr-assoc 'jcr:path           user)))
                 (let ((settings (seq-reduce
                                   (lambda (result setting)
                                     (if (funcall (cadr setting) (caddr setting))
                                         (cons
                                           (cons (car setting) (caddr setting))
                                           result)
                                       result))
                                   (list
                                     (list
                                       "givenName"
                                       (lambda (elem) (not (string-empty-p elem)))
                                       (read-string (concat
                                                      username
                                                      "'s first name to set "
                                                      "(leave blank to skip): ")))
                                     (list
                                       "familyName"
                                       (lambda (elem) (not (string-empty-p elem)))
                                       (read-string (concat
                                                      username
                                                      "'s last name to set "
                                                      "(leave blank to skip): ")))
                                     (list
                                       "email"
                                       (lambda (elem) (not (string-empty-p elem)))
                                       (read-string (concat
                                                      username
                                                      "'s E-mail to set "
                                                      "(leave blank to skip): ")))
                                     (list
                                       "age"
                                       (lambda (elem)
                                         (and (> elem 0) (integerp elem)))
                                       (read-number (concat
                                                      username
                                                      "'s age to set (enter "
                                                      "zero, negative, or "
                                                      "decimal to skip): "))))
                                   '())))
                   (if (null settings)
                       result
                     (aem-set-user-profile
                       (aem--account-get-uri aem--accounts-current-active)
                       path
                       settings)

                     (concat ", \""  username "\"" result)))))
             users
             "")))
    (when (> (length s) 2)
      (when (eq major-mode 'aem:users-list-mode)
        (revert-buffer nil t))

      (message (concat "Updated the profile(s) of " (substring s 2) "!")))))

(define-key aem:users-list-mode-map (kbd "S") 'aem-users-set-profile)


(defun aem-users-add-to-group (users)
  "Add USERS to group in AEM (list of groups provided by the function)."
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let* ((uri    (aem--account-get-uri aem--accounts-current-active))
         (groups (mapcar
                   (lambda (group)
                     (cons
                       (cdr-assoc 'rep:authorizableId group)
                       (cdr-assoc 'jcr:path           group)))
                   (aem-get-groups uri)))
         (names  (mapcar #'car groups))
         (listOf (seq-reduce
                   (lambda (result user)
                     (let* (( username (cdr-assoc 'rep:authorizableId user))
                            (groupName (ido-completing-read
                                         (concat "Add " username " to which Group: ")
                                         names)))
                       (aem-add-user-to-group uri username (cdr-assoc
                                                             groupName
                                                             groups))

                       (cons username (concat ", \"" username "\"" (cdr result)))))
                   users
                   '("" . ""))))
    (when (> (length (cdr listOf)) 2)
      (when (eq major-mode 'aem:users-list-mode)
        (revert-buffer nil t)

        (search-forward (car listOf))
        (move-beginning-of-line 1))

      (message
        (concat "Updated group with user(s) " (substring (cdr listOf) 2) "!")))))

(define-key aem:users-list-mode-map (kbd "a") 'aem-users-add-to-group)
      






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


  ; Interactive Function to Call
(defun aem-groups ()
  "Display a list of AEM groups for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:groups))

(provide 'aem-tools_users)

;;; aem-tools_users.el ends here
