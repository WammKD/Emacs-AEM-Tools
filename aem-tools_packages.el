;;; aem-tools_packages.el --- functions for interfacing with AEM's JCR API
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


;; Packages
  ; Entries
(defun aem--packages-get-entries (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (mapcar
                 (lambda (packageData)
                   (let ((wStat (cons
                                  (cons
                                    'status
                                    (let ((b (cdr-assoc 'buildCount    packageData))
                                          (c (cdr-assoc 'created       packageData))
                                          (m (cdr-assoc 'lastModified  packageData))
                                          (u (cdr-assoc 'lastUnpacked  packageData))
                                          (w (cdr-assoc 'lastUnwrapped packageData)))
                                      (cond
                                       ((and b w (not u) (> b 0))       "Install")
                                       ((and (not u) (not c))             "Build")
                                       ((and c m (< c m))               "Rebuild")
                                       ((and (not u) (not (equal c m))) "Install")
                                       (t                                 "Share"))))
                                  packageData)))
                   (cons (cons 'id wStat) wStat)))
                 (aem-get-packages (aem--account-get-uri
                                     aem--accounts-current-active))))
    (otherwise (error "Unknown search type: %S" searchType))))
(defun aem--packages-get-entries-extensive (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (seq-reduce
                 (lambda (result packageData)
                   (if (string-match-p
                         "/\\.snapshot/"
                         (cdr-assoc 'jcr:path packageData))
                       result
                     (let* ((content   (cdr-assoc 'jcr:content    packageData))
                            (def       (cdr-assoc 'vlt:definition     content))
                            (filts     (cdr-assoc 'filter                 def))
                            (sShots    (cdr-assoc 'screenshots            def))
                            (created   (assoc     'jcr:created            def))
                            (modified  (assoc     'jcr:lastModified       def))
                            (unpacked  (assoc     'lastUnpacked           def))
                            (final     `(,(assoc 'jcr:uuid               content)
                                         ,(assoc 'group                      def)
                                         ,(assoc 'name                       def)
                                         ,(assoc 'version                    def)
                                         ,(assoc ':jcr:data              content)
                                         ,(assoc 'jcr:path           packageData)
                                         ,(assoc 'description                def)
                                         ,(assoc 'fixedBugs                  def)
                                         ,(assoc 'testedWith                 def)
                                         ,(assoc 'builtWith                  def)
                                         ,(assoc 'buildCount                 def)
                                         ,(assoc 'providerName               def)
                                         ,(assoc 'providerUrl                def)
                                         ,(assoc 'providerLink               def)
                                         ,created
                                         ,(assoc 'jcr:createdBy              def)
                                         ,modified
                                         ,(assoc 'jcr:lastModifiedBy         def)
                                         ,unpacked
                                         ,(assoc 'lastUnpackedBy             def)
                                         ,(assoc 'lastWrapped                def)
                                         ,(assoc 'lastWrappedBy              def)
                                         ,(assoc 'lastUnwrapped              def)
                                         ,(assoc 'lastUnwrappedBy            def)
                                         ,(cons 'status (let ((c (cdr  created))
                                                              (m (cdr modified))
                                                              (u (cdr unpacked)))
                                                          (cond
                                                           ((and (not u) (not c))  "Build")
                                                           ((and
                                                              c
                                                              m
                                                              (time-less-p
                                                                (date-to-time c)
                                                                (date-to-time m))) "Rebuild")
                                                           ((and
                                                              (not u)
                                                              (not (equal c m)))   "Install")
                                                           (t                      "Share"))))
                                         ,(cons 'filters (reverse (seq-reduce
                                                                    (lambda (result filter)
                                                                      (if (not (listp (cdr filter)))
                                                                          result
                                                                        (cons (cdr filter) result)))
                                                                    filts
                                                                    '()))))))
                       (cons (cons (cons 'id final) final) result))))
                 (aem-get-packages-extensive (aem--account-get-uri
                                               aem--accounts-current-active))
                 '()))
    (otherwise (error "Unknown search type: %S" searchType))))
(defun aem--packages-get-entries-simplified (&optional searchType &rest searchValues)
  ""

  (pcase (or searchType 'all)
    ('id       searchValues)
    ('all      (mapcar
                 (lambda (packageData)
                   (let ((packageFinal (mapcar
                                         (lambda (info)
                                           (cons (car info) (caddr info)))
                                         (cddr
                                           (seq-remove 'stringp packageData)))))
                     (cons (cons 'id packageFinal) packageFinal)))
                 (seq-filter
                   'listp
                   (cdddr
                     (assoc
                       'packages
                       (assoc
                         'data
                         (assoc
                           'response
                           (assoc 'crx (aem-get-packages
                                         (aem--account-get-uri
                                           aem--accounts-current-active))))))))))
    (otherwise (error "Unknown search type: %S" searchType))))


(bui-define-entry-type aem:packages
  :titles               '((pid                . "ID")
                          (groupTitle         . "Group Title")
                          (fixedBugs          . "Fixed Bugs")
                          (size               . "File Size")
                          (builtWith          . "Built With")
                          (buildCount         . "Build Count")
                          (providerName       . "Provider Name")
                          (providerUrl        . "Provider URL")
                          (providerLink       . "Provider Link")
                          (testedWith         . "Tested With")
                          (created            . "Last Created")
                          (createdBy          . "Last Created By")
                          (lastModified       . "Last Modified")
                          (lastModifiedBy     . "Last Modified By")
                          (lastUnpacked       . "Last Unpacked")
                          (lastUnpackedBy     . "Last Unpacked By")
                          (lastWrapped        . "Last Wrapped")
                          (lastWrappedBy      . "Last Wrapped By")
                          (lastUnwrapped      . "Last Unwrapped")
                          (lastUnwrappedBy    . "Last Unwrapped By"))
  :get-entries-function #'aem--packages-get-entries)
(bui-define-entry-type aem:packages-extensive
  :titles               '((jcr:uuid           . "UUID")
                          (fixedBugs          . "Fixed Bugs")
                          (:jcr:data          . "File Size")
                          (jcr:path           . "Path")
                          (builtWith          . "Built With")
                          (buildCount         . "Build Count")
                          (providerName       . "Provider Name")
                          (providerUrl        . "Provider URL")
                          (providerLink       . "Provider Link")
                          (testedWith         . "Tested With")
                          (jcr:created        . "Last Created")
                          (jcr:createdBy      . "Last Created By")
                          (jcr:lastModified   . "Last Modified")
                          (jcr:lastModifiedBy . "Last Modified By")
                          (lastUnpacked       . "Last Unpacked")
                          (lastUnpackedBy     . "Last Unpacked By")
                          (lastWrapped        . "Last Wrapped")
                          (lastWrappedBy      . "Last Wrapped By")
                          (lastUnwrapped      . "Last Unwrapped")
                          (lastUnwrappedBy    . "Last Unwrapped By"))
  :get-entries-function #'aem--packages-get-entries-extensive)
(bui-define-entry-type aem:packages-simplified
  :titles               '((downloadName   . "Download Name")
                          (createdBy      . "Created By")
                          (lastModified   . "Last Modified")
                          (lastModifiedBy . "Last Modified By")
                          (lastUnpacked   . "Last Unpacked")
                          (lastUnpackedBy . "Last Unpacked By"))
  :get-entries-function #'aem--packages-get-entries-simplified)


  ; Info.
(bui-define-interface aem:packages info
  :format '((group              format (format))
            (groupTitle         format (format))
            nil
            (name               format (format))
            (version            format (format))
            (size               format (format))
            (path               format (format))
            nil
            (description        format (format))
            (fixedBugs          format (format))
            (testedWith         format (format))
            nil
            (builtWith          format (format))
            (buildCount         format (format))
            nil
            (providerName       format (format))
            (providerUrl        format (format))
            (providerLink       format (format))
            nil
            (created            format   (time))
            (createdBy          format (format))
            nil
            (lastModified       format   (time))
            (lastModifiedBy     format (format))
            nil
            (lastUnpacked       format   (time))
            (lastUnpackedBy     format (format))
            nil
            (lastWrapped        format   (time))
            (lastWrappedBy      format (format))
            (lastUnwrapped      format   (time))
            (lastUnwrappedBy    format (format))))
(bui-define-interface aem:packages-extensive info
  :format '((jcr:uuid           format (format))
            nil
            (group              format (format))
            (name               format (format))
            (version            format (format))
            (:jcr:data          format (format))
            (jcr:path           format (format))
            nil
            (description        format (format))
            (fixedBugs          format (format))
            (testedWith         format (format))
            nil
            (builtWith          format (format))
            (buildCount         format (format))
            nil
            (providerName       format (format))
            (providerUrl        format (format))
            (providerLink       format (format))
            nil
            (jcr:created        format   (time))
            (jcr:createdBy      format (format))
            nil
            (jcr:lastModified   format   (time))
            (jcr:lastModifiedBy format (format))
            nil
            (lastUnpacked       format   (time))
            (lastUnpackedBy     format (format))
            nil
            (lastWrapped        format   (time))
            (lastWrappedBy      format (format))
            (lastUnwrapped      format   (time))
            (lastUnwrappedBy    format (format))))
(bui-define-interface aem:packages-simplified info
  :format '((group          format (format))
            (name           format (format))
            (version        format (format))
            (downloadName   format (format))
            (size           format (format))
            nil
            (created        format   (time))
            (createdBy      format (format))
            nil
            (lastModified   format   (time))
            (lastModifiedBy format (format))
            nil
            (lastUnpacked   format   (time))
            (lastUnpackedBy format (format))))


(defun aem--packages-list-describe (&rest packages)
  "Display 'info' buffer for PACKAGES."

  (bui-get-display-entries 'aem:packages 'info (cons 'id packages)))
(defun aem--packages-extensive-list-describe (&rest packages)
  "Display 'info' buffer for PACKAGES."

  (bui-get-display-entries 'aem:packages-extensive  'info (cons 'id packages)))
(defun aem--packages-simplified-list-describe (&rest packages)
  "Display 'info' buffer for PACKAGES."

  (bui-get-display-entries 'aem:packages-simplified 'info (cons 'id packages)))


  ; List
(bui-define-interface aem:packages list
  :buffer-name       "*Packages*"
  :describe-function 'aem--packages-list-describe
  :format            '((name           nil 52 t)
                       (version        nil 24 t)
                       (size           nil 10 bui-list-sort-numerically-3 :right-align t)
                       (lastModified   nil 32 t)
                       (lastModifiedBy nil 21 t)
                       (status         nil  7 t))
  :sort-key          '(lastModified))
(bui-define-interface aem:packages-extensive list
  :buffer-name       "*Packages, Extensive*"
  :describe-function 'aem--packages-extensive-list-describe
  :format            '((name               nil 52 t)
                       (version            nil 24 t)
                       (:jcr:data          nil 10 bui-list-sort-numerically-3 :right-align t)
                       (jcr:lastModified   nil 32 t)
                       (jcr:lastModifiedBy nil 21 t)
                       (status             nil  7 t))
  :sort-key          '(jcr:lastModified))
(bui-define-interface aem:packages-simplified list
  :buffer-name       "*Packages, Simplified*"
  :describe-function 'aem--packages-simplified-list-describe
  :format            '((name           nil 55 t)
                       (version        nil 24 t)
                       (size           nil 10 bui-list-sort-numerically-3 :right-align t)
                       (lastModified   nil 32 t)
                       (lastModifiedBy nil 25 t))
  :sort-key          '(jcr:lastModified))


  ; Interactive Functions to Call
(defun aem-packages-list-download-packages (downloadLocation packages)
  "Download the PACKAGES at point, or all marked, to path DOWNLOADLOCATION."
  (interactive (list
                 (read-directory-name "Download Package(s) to: ")
                 (or (bui-list-get-marked-id-list) (list (bui-list-current-id)))))

  (dolist (package packages)
    (aem-download-package
      (aem--account-get-uri aem--accounts-current-active)
      downloadLocation
      :path (cdr-assoc 'path package))))
(defun aem-packages-extensive-list-download-packages (downloadLocation packages)
  "Download the PACKAGES at point, or all marked, to path DOWNLOADLOCATION."
  (interactive (list
                 (read-directory-name "Download Package(s) to: ")
                 (or (bui-list-get-marked-id-list) (list (bui-list-current-id)))))

  (dolist (package packages)
    (aem-download-package
      (aem--account-get-uri aem--accounts-current-active)
      downloadLocation
      :path (cdr-assoc 'path package))))""
(defun aem-packages-simplified-list-download-packages (downloadLocation)
  "Download the package at point or all marked to path DOWNLOADLOCATION."
  (interactive (list (read-directory-name "Download Package(s) to: ")))

  (dolist (package (or (bui-list-get-marked-id-list) (list (bui-list-current-id))))
    (aem-download-package
      (aem--account-get-uri
        aem--accounts-current-active)         downloadLocation
      :name (cdr-assoc 'downloadName package) :group (cdr-assoc 'group package))))

(define-key aem:packages-list-mode-map (kbd "d")
  'aem-packages-list-download-packages)
(define-key aem:packages-extensive-list-mode-map (kbd "d")
  'aem-packages-extensive-list-download-packages)
(define-key aem:packages-simplified-list-mode-map (kbd "d")
  'aem-packages-simplified-list-download-packages)


(defun aem-packages-open-in-browser (packages)
  ""
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (dolist (package packages)
    (browse-url (concat
                  (aem--account-get-uri aem--accounts-current-active)
                  "/crx/packmgr/index.jsp#"
                  (replace-regexp-in-string ":" "%3A" (cdr-assoc 'path package))))))

(define-key aem:packages-list-mode-map (kbd "o") 'aem-packages-open-in-browser)

(defun aem-packages-open-browser ()
  ""
  (interactive)

  (browse-url (concat
                (aem--account-get-uri aem--accounts-current-active)
                "/crx/packmgr/index.jsp")))

(define-key aem:packages-list-mode-map (kbd "O") 'aem-packages-open-browser)


(defun aem-packages-build (packages)
  ""
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let ((r (seq-reduce
             (lambda (result package)
               (let ((name (cdr-assoc 'name package)))
                 (aem-build-package
                   (aem--account-get-uri aem--accounts-current-active)
                   (cdr-assoc 'path package))

                 (cons name (concat ", \"" name "\"" (cdr result)))))
             packages
             '("" . ""))))
    (when (> (length (cdr r)) 2)
      (when (eq major-mode 'aem:packages-list-mode)
        (revert-buffer nil t)

        (search-forward (car r))
        (move-beginning-of-line 1)

        (message (concat "Built package(s) " (substring (cdr r) 2) "!"))))))

(define-key aem:packages-list-mode-map (kbd "b") 'aem-packages-build)


(defun aem-packages-install (packages)
  ""
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let ((r (seq-reduce
             (lambda (result package)
               (let ((name (cdr-assoc 'name package)))
                 (if (not (yes-or-no-p
                            (concat "Really install package " name "? ")))
                     result
                   (aem-install-package
                     (aem--account-get-uri aem--accounts-current-active)
                     (cdr-assoc 'path package))

                   (cons name (concat ", \"" name "\"" (cdr result))))))
             packages
             '("" . ""))))
    (when (> (length (cdr r)) 2)
      (when (eq major-mode 'aem:packages-list-mode)
        (revert-buffer nil t)

        (search-forward (car r))
        (move-beginning-of-line 1))

      (message (concat "Installed package(s) " (substring (cdr r) 2) "!")))))

(define-key aem:packages-list-mode-map (kbd "I") 'aem-packages-install)


(defun aem-packages-delete (packages)
  ""
  (interactive (list (or
                       (bui-list-get-marked-id-list)
                       (list (bui-list-current-id)))))

  (let ((s (seq-reduce
             (lambda (result package)
               (let ((name (cdr-assoc 'name package)))
                 (if (not (yes-or-no-p
                            (concat "Really delete package " name "? ")))
                     result
                   (aem-delete-package
                     (aem--account-get-uri aem--accounts-current-active)
                     (cdr-assoc 'path package))

                   (concat ", \"" name "\"" result))))
             packages
             "")))
    (when (> (length s) 2)
      (when (eq major-mode 'aem:packages-list-mode)
        (revert-buffer nil t)

        (message (concat "Deleted package(s) " (substring s 2) "!"))))))

(define-key aem:packages-list-mode-map (kbd "k") 'aem-packages-delete)


(defun aem-packages-add-filter (packages filterPath)
  ""
  (interactive (list
                 (or (bui-list-get-marked-id-list) (list (bui-list-current-id)))
                 (read-string "The path you want to set as a filter: ")))

  (dolist (package packages)
    (aem-add-filter-to-package
      (aem--account-get-uri aem--accounts-current-active)
      package
      filterPath)))

(define-key aem:packages-list-mode-map (kbd "a") 'aem-packages-add-filter)


(defun aem-packages ()
  "Display a list of AEM packages for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:packages))
(defun aem-packages-extensive ()
  "Display a list of AEM packages for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:packages-extensive))
(defun aem-packages-simplified ()
  "Display a list of AEM packages for an instance."
  (interactive)

  (bui-list-get-display-entries 'aem:packages-simplified))

(provide 'aem-tools_packages)

;;; aem-tools_packages.el ends here
