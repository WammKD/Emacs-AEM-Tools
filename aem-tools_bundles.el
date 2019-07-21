;;; aem-tools_bundles.el --- functions for interfacing with AEM's JCR API
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
(require 'json)

; Internal Files
(require 'aem-tools_accounts)
(require 'aem-tools_http)


;; Bundles
  ; Entries
(defun aem--bundles-get-entries (&optional searchType &rest searchValues)
  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (mapcar
                 (lambda (bundle-data)
                   (let ((obj (mapcar
                                (lambda (bundle-datum)
                                  (cons
                                    (if (eq (car bundle-datum) 'id)
                                        'realID
                                      (car bundle-datum))
                                    (if (eq (cdr bundle-datum) :json-false)
                                        nil
                                      (cdr bundle-datum))))
                                bundle-data)))
                     (cons (cons 'id obj) obj)))
                 (cdr-assoc 'data (aem-get-bundles
                                    (aem--account-get-uri
                                      aem--accounts-current-active)))))
    (otherwise (error "Unknown search type: %S" searchType))))

(bui-define-entry-type aem:bundles
  :titles               '((realID       .            "ID")
                          (stateRaw     .     "Raw State")
                          (symbolicName . "Symbolic Name"))
  :get-entries-function #'aem--bundles-get-entries)


  ; Info.
(bui-define-interface aem:bundles info
  :format '((realID       format (format))
            nil
            (name         format (format))
            (symbolicName format (format))
            (version      format (format))
            nil
            (fragment     format (format))
            nil
            (state        format (format))
            (stateRaw     format (format))
            nil
            (category     format (format))))

(defun aem--bundles-list-describe (&rest bundles)
  "Display 'info' buffer for BUNDLES."

  (bui-get-display-entries 'aem:bundles 'info (cons 'id bundles)))


  ; List
(bui-define-interface aem:bundles list
  :buffer-name       "*Bundles*"
  :describe-function 'aem--bundles-list-describe
  :format            '((realID   nil  8 bui-list-sort-numerically-1 :right-align t)
                       (name     nil 72 t)
                       (version  nil 20 t)
                       (category nil 37 t)
                       (state    nil  9 t))
  :sort-key          '(name))


  ; Interactive Function to Call
(defun aem-bundles-start-or-stop-bundle (bundles)
  "Start or stop the bundle at point or all marked, depending on the state."
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let ((ss (seq-reduce
              (lambda (result bundle)
                (let ((name                            (cdr-assoc 'name   bundle))
                      (id                              (cdr-assoc 'realID bundle))
                      (domain (aem--account-get-uri aem--accounts-current-active)))
                  (if (= (cdr-assoc 'stateRaw bundle) 32)
                      (if (not (yes-or-no-p
                                 (concat "Really stop bundle \"" name "\"? ")))
                          result
                        (aem-stop-bundle domain (number-to-string id))

                        (cons (car result) (concat ", \"" name "\"" (cdr result))))
                    (aem-start-bundle domain (number-to-string id))

                    (cons (concat ", \"" name "\"" (car result)) (cdr result)))))
              bundles
              '("" . ""))))
    (revert-buffer nil t)

    (message (let* ((startList             (car ss))
                    ( stopList             (cdr ss))
                    (startLength (length startList))
                    ( stopLength (length  stopList)))
               (concat
                 (if (< startLength 2)
                     ""
                   (concat "Starting bundle(s) " (substring startList 2) "…"))
                 (if (or (< startLength 2) (< stopLength 2)) "" "\n\n")
                 (if (< (length stopList) 2)
                     ""
                   (concat "Stopping bundle(s) " (substring stopList 2) "…")))))))

(define-key aem:bundles-list-mode-map (kbd "S") 'aem-bundles-start-or-stop-bundle)


(defun aem-bundles ()
  "Display a list of AEM bundles for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:bundles))

(provide 'aem-tools_bundles)

;;; aem-tools_bundles.el ends here
