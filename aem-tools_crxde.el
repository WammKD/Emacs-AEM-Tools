;;; aem-tools_crxde.el --- functions for interfacing with AEM's JCR API
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
(require 'hierarchy)
(require 'seq)
(require 'subr-x)

; Internal Files
(require 'aem-tools_accounts)
(require 'aem-tools_http)


;; CRXde
  ; Build Hierarchy
(defun aem-get-node-hierarchy-elem (path)
  ""

  (when path
    (let ((refined-path (directory-file-name path)))
      (cons
        (intern (file-name-base refined-path))
        (cons
          refined-path
          (aem--get-node-properties
            (aem-get-subnodes (aem--account-get-uri
                                aem--accounts-current-active) refined-path)))))))

(defun aem-build-node-hierarchy (path &optional hierarchyToUse)
  ""

  (let ((hierarchy  (or hierarchyToUse (hierarchy-new)))
        (  parentfn (lambda (hier-elem)
                      (when hier-elem
                        (aem-get-node-hierarchy-elem
                          (file-name-directory
                            (directory-file-name (cadr hier-elem)))))))
        (childrenfn (lambda (hier-elem)
                      (when hier-elem
                        (let ((path (directory-file-name (cadr hier-elem))))
                          (mapcar
                            (lambda (subnode)
                              (let ((name (car subnode)))
                                (cons name (cons
                                             (concat
                                               path
                                               (if (string-equal "/" path) "" "/")
                                               (symbol-name name))
                                             (cdr subnode)))))
                            (aem--get-node-subnodes
                              (aem-get-subnodes
                                (aem--account-get-uri aem--accounts-current-active)
                                path))))))))
    (hierarchy-add-tree hierarchy (aem-get-node-hierarchy-elem path)
                        nil       childrenfn                         nil t)

    hierarchy))


  ; Entries
(defun aem--node-properties-get-entries (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       (car searchValues))
    (otherwise (error "Unknown search type: %S" searchType))))

(bui-define-entry-type aem:node-properties
  :get-entries-function #'aem--node-properties-get-entries)


  ; List
(bui-define-interface aem:node-properties list
  :buffer-name "*Node Properties*"
  :format      '((name  nil 49 t)
                 (value nil 97 5))
  :sort-key    '(name))


  ; Interactive Functions to Call
(defun aem--crxde-open-in-browser (nodeProps)
  ""

  (browse-url (concat
                (aem--account-get-uri aem--accounts-current-active)
                "/crx/de/index.jsp#"
                (replace-regexp-in-string ":" "%3A" (cdr-assoc 'path nodeProps)))))
(defun aem--crxde-open-site-in-browser (nodeProps)
  ""

  (let* ((path                                 (cdr-assoc 'path nodeProps))
         (pagePath (substring path 0 (string-match-p "/jcr:content" path))))
    (if (equal
          (cdr-assoc
            'jcr:primaryType
            (aem-get-subnodes (aem--account-get-uri
                                aem--accounts-current-active) pagePath))
          "cq:Page")
        (browse-url (concat
                      (aem--account-get-uri aem--accounts-current-active)
                      "/sites.html"
                      pagePath))
      (message "There's no page anywhere on the path of this node!"))))
(defun aem--crxde-open-properties-in-browser (nodeProps)
  ""

  (let* ((path                                 (cdr-assoc 'path nodeProps))
         (pagePath (substring path 0 (string-match-p "/jcr:content" path))))
    (if (equal
          (cdr-assoc
            'jcr:primaryType
            (aem-get-subnodes (aem--account-get-uri
                                aem--accounts-current-active) pagePath))
          "cq:Page")
        (browse-url (concat
                      (aem--account-get-uri aem--accounts-current-active)
                      "/mnt/overlay/wcm/core/content/sites/properties.html?item="
                      (url-hexify-string pagePath)))
      (message "There's no page anywhere on the path of this node!"))))
(defun aem--crxde-open-page-in-browser (nodeProps)
  ""

  (let* ((path                                 (cdr-assoc 'path nodeProps))
         (pagePath (substring path 0 (string-match-p "/jcr:content" path))))
    (if (equal
          (cdr-assoc
            'jcr:primaryType
            (aem-get-subnodes (aem--account-get-uri
                                aem--accounts-current-active) pagePath))
          "cq:Page")
        (browse-url (concat
                      (aem--account-get-uri aem--accounts-current-active) "/editor.html"
                      pagePath                                            ".html"))
      (message "There's no page anywhere on the path of this node!"))))
(defun aem--crxde-view-page-as-published-in-browser (nodeProps)
  ""

  (let* ((path                                 (cdr-assoc 'path nodeProps))
         (pagePath (substring path 0 (string-match-p "/jcr:content" path))))
    (if (equal
          (cdr-assoc
            'jcr:primaryType
            (aem-get-subnodes (aem--account-get-uri
                                aem--accounts-current-active) pagePath))
          "cq:Page")
        (browse-url (concat
                      (aem--account-get-uri aem--accounts-current-active)
                      pagePath
                      ".html?wcmmode=disabled"))
      (message "There's no page anywhere on the path of this node!"))))
(defun aem--crxde-create-node (nodeProps)
  ""

  (let ((path (cdr-assoc 'path nodeProps)))
    (let ((nodeName (read-string (concat
                                   "What name for the new node that will be created at "
                                   path
                                   "? "))))
      (aem-create-or-update-node
        (aem--account-get-uri aem--accounts-current-active)
        (concat path "/" nodeName)
        '((jcr:primaryType . nt:unstructured)))

      (aem-crxde path))))
(defun aem--crxde-update-node-property (nodeProps)
  ""

  (let ((path         (cdr-assoc 'path  nodeProps))
        (name         (cdr-assoc 'name  nodeProps))
        (oldValue     (cdr-assoc 'value nodeProps))
        (updateVector (lambda (ovIndex ovLength result)
                        (let ((value (read-string
                                       (concat
                                         "Array value (leave blank to stop "
                                         "adding values to the updated array): ")
                                       (when (< ovIndex ovLength)
                                         (aref oldValue ovIndex)))))
                          (if (string-empty-p value)
                              (reverse result)
                            (funcall updateVector
                                      (1+ ovIndex)
                                      ovLength
                                      (cons value result)))))))
    (let ((newValues (if (stringp oldValue)
                         (list (read-string (concat
                                              "Replacement value for "
                                              (symbol-name name)
                                              ": ")                     oldValue))
                       (funcall updateVector 0 (length oldValue) '()))))
      (aem-create-or-update-node
        (aem--account-get-uri aem--accounts-current-active)
        path
        (mapcar (lambda (elem) (cons name elem)) newValues))

      (aem-crxde path)

      (forward-button 1)
      (let ((b (button-at (point))))
        (move-beginning-of-line nil)
        (button-activate b)
        (search-forward (symbol-name name))
        (move-beginning-of-line nil)))))
(defun aem--crxde-delete-node-property (nodeProps)
  ""

  (let ((path (cdr-assoc 'path nodeProps))
        (name (cdr-assoc 'name nodeProps)))
    (when (yes-or-no-p (concat
                         "Are you sure you want to delete the property of node "
                         path
                         "? "))
      (aem-delete-node-property
        (aem--account-get-uri aem--accounts-current-active)
        path
        (list (symbol-name name)))

      (aem-crxde path)

      (forward-button 1)
      (let ((b (button-at (point))))
        (move-beginning-of-line nil)
        (button-activate b)))))
(defun aem--crxde-delete-node (nodeProps)
  ""

  (let ((path (cdr-assoc 'path nodeProps)))
    (when (yes-or-no-p (concat
                         "Are you sure you want to delete the node at path "
                         path
                         "? "))
      (aem-delete-node (aem--account-get-uri aem--accounts-current-active) path)

      (aem-crxde (file-name-directory path)))))

(defun aem--crxde-run-operation-on-node-properties (operation)
  ""

  (dolist (node (or (bui-list-get-marked-id-list) (list (bui-list-current-id))))
    (funcall operation node)))

(defun aem-node-properties-open-in-browser ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-open-in-browser))

(defun aem-node-properties-view-page-as-published-in-browser ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-view-page-as-published-in-browser))

(defun aem-node-properties-open-site-in-browser ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-open-site-in-browser))

(defun aem-node-properties-open-properties-in-browser ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-open-properties-in-browser))

(defun aem-node-properties-open-page-in-browser ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-open-page-in-browser))


(defun aem-crxde-view-delete ()
  ""
  (interactive)

  (when-let ((windows (seq-filter
                        (lambda (window)
                          (string-match-p
                            (concat
                              "^*AEM View, "
                              (aem--account-get-alias aem--accounts-current-active)
                              ": ")
                            (buffer-name (window-buffer window))))
                        (window-list))))
    (  quit-window nil (car windows))
    (delete-window (car windows))))

(defun aem-node-properties-create-node ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-create-node))
(defun aem-node-properties-update-node-property ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-update-node-property))
(defun aem-node-properties-delete-node-property ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-delete-node-property))
(defun aem-node-properties-kill-node ()
  ""
  (interactive)

  (aem--crxde-run-operation-on-node-properties 'aem--crxde-delete-node))

(define-key aem:node-properties-list-mode-map (kbd "e")   'aem-node-properties-update-node-property)
(define-key aem:node-properties-list-mode-map (kbd "k")   'aem-node-properties-delete-node-property)
(define-key aem:node-properties-list-mode-map (kbd "N +") 'aem-node-properties-create-node)
(define-key aem:node-properties-list-mode-map (kbd "N k") 'aem-node-properties-kill-node)
(define-key aem:node-properties-list-mode-map (kbd "N o") 'aem-node-properties-open-in-browser)
(define-key aem:node-properties-list-mode-map (kbd "N b") 'aem-node-properties-open-site-in-browser)
(define-key aem:node-properties-list-mode-map (kbd "N P") 'aem-node-properties-open-properties-in-browser)
(define-key aem:node-properties-list-mode-map (kbd "N e") 'aem-node-properties-open-page-in-browser)

(defun aem-crxde (path)
  "Display content subnodes."
  (interactive (list (completing-read
                       "Path to retrieve: "
                       (completion-table-dynamic
                         (lambda (curr)
                           (if (string-empty-p curr)
                               '("/")
                             (let* ((rev  (reverse curr))
                                    (base (reverse
                                            (substring rev (string-match-p "/" rev)))))
                               (seq-filter
                                 (lambda (elem) (string-prefix-p curr elem))
                                 (mapcar
                                   (lambda (elem) (concat base (symbol-name (car elem)) "/"))
                                   (aem--get-node-subnodes
                                     (aem-get-subnodes (aem--account-get-uri
                                                         aem--accounts-current-active) base)))))))))))

  (switch-to-buffer (hierarchy-tree-display
                      (aem-build-node-hierarchy path)
                      (lambda (item _)
                        (insert-text-button
                          (symbol-name (car item))
                          'properties (let ((p (cadr item)))
                                        (mapcar
                                          (lambda (prop)
                                            (let ((pa `((name  . ,(car prop))
                                                        (value . ,(cdr prop))
                                                        (path  .          ,p))))
                                              `((id . ,pa) . ,pa)))
                                          (aem--get-node-properties (cddr item))))
                          'action     (lambda (b)
                                        (bui-list-get-display-entries
                                          'aem:node-properties
                                          'id
                                          (button-get b 'properties)))))))

  (local-set-key (kbd "l") 'recenter-top-bottom)
  (local-set-key (kbd "n") 'widget-forward)
  (local-set-key (kbd "p") 'widget-backward)
  (local-set-key (kbd "i") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (button-activate b))))
  (local-set-key (kbd "o") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-open-in-browser
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "v") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-view-page-as-published-in-browser
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "b") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-open-site-in-browser
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "P") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-open-properties-in-browser
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "e") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-open-page-in-browser
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "+") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-create-node
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "k") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (if (string= (what-line) "Line 1")
                                    (move-beginning-of-line nil)
                                  (widget-backward 1))

                                (aem--crxde-delete-node
                                  (car (button-get b 'properties))))))
  (local-set-key (kbd "g") '(lambda ()
                              (interactive)

                              (beginning-of-buffer)
                              (forward-button 1)

                              (let ((path (cdr-assoc
                                            'path
                                            (car (button-get
                                                   (button-at (point))
                                                   'properties)))))
                                (kill-buffer)

                                (aem-crxde path)))))

(provide 'aem-tools_crxde)

;;; aem-tools_crxde.el ends here
