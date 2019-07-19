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

; Internal Files
(require 'aem-tools_accounts)
(require 'aem-tools_http)


;; CRXde
  ; Utility
(defun aem--get-node-properties (node-info)
  ""

  (seq-filter #'(lambda (elem) (not (listp (cdr elem)))) node-info))
(defun aem--get-node-subnodes (node-info)
  ""

  (seq-filter #'(lambda (elem) (listp (cdr elem))) node-info))


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
            (aem--get-subnodes (aem--account-get-uri
                                 aem--accounts-current-active) refined-path)))))))

(defun aem-build-node-hierarchy (path)
  ""

  (let ((hierarchy  (hierarchy-new))
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
                                             (concat path "/" (symbol-name name))
                                             (cdr subnode)))))
                            (aem-get-node-subnodes
                              (aem-get-subnodes
                                (aem--account-get-uri         aem--accounts-current-active)
                                path))))))))
    (hierarchy-add-tree hierarchy (aem-get-node-hierarchy-elem path)
                        nil       childrenfn                         nil t)

    hierarchy))


  ; Interactive Functions to Call
(defun aem-crxde (path)
  "Display content subnodes."
  (interactive (list (read-string "Path to retrieve: ")))

  (switch-to-buffer (hierarchy-tree-display
                      (aem-build-node-hierarchy path)
                      (lambda (item _)
                        (insert-text-button
                          (symbol-name (car item))
                          'properties (cdr item)
                          'action     (lambda (b)
                                        (message (format "%s" (aem-get-node-properties (cdr (button-get b 'properties))))))))))

  (local-set-key (kbd "l") 'recenter-top-bottom)
  (local-set-key (kbd "n") 'widget-forward)
  (local-set-key (kbd "p") 'widget-backward)
  (local-set-key (kbd "i") '(lambda ()
                              (interactive)

                              (forward-button 1)

                              (let ((b (button-at (point))))
                                (widget-backward 1)

                                (button-activate b)))))
