;;; aem-tools_http.el --- functions for interfacing with AEM's JCR API
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
(require 'json)
(require 'seq)
(require 'url)


(defconst aem--REQUEST_GET    "GET"
  "String constant to represent choosing a GET HTTP request.")
(defconst aem--REQUEST_PUT    "PUT"
  "String constant to represent choosing a PUT HTTP request.")
(defconst aem--REQUEST_POST   "POST"
  "String constant to represent choosing a POST HTTP request.")
(defconst aem--REQUEST_PATCH  "PATCH"
  "String constant to represent choosing a PATCH HTTP request.")
(defconst aem--REQUEST_DELETE "DELETE"
  "String constant to represent choosing a DELETE HTTP request.")


;; Utility
(defun cdr-assoc (key assoc-list)
  "A simple utility function to run (cdr (assoc KEY ASSOC-LIST))."

  (cdr (assoc key assoc-list)))

(defun aem--get-node-properties (node-info)
  ""

  (seq-filter #'(lambda (elem) (not (listp (cdr elem)))) node-info))
(defun aem--get-node-subnodes (node-info)
  ""

  (seq-filter #'(lambda (elem) (listp (cdr elem))) node-info))



;; Functions Related to Making HTTP Calls
(defun aem--create-URI (domain &rest rest)
  "Basically `concat' but strips any forward slash off of the end of DOMAIN."

  (apply 'concat (cons (directory-file-name domain) rest)))

(defun aem--concat-amps (args)
  "Converts ARGS, a list of pairs, to a string of \"key1=val1&key2=val2\"."

  (seq-reduce (lambda (result elem)
                (concat
                  result
                  (if (string-empty-p result) "" "&")
                  (url-hexify-string (format "%s" (car elem)))
                  "="
                  (url-hexify-string (format "%s" (cdr elem))))) args ""))

(defun aem--process (buffer url type)
  "Handles and parses the returned string from a call to `url-retrieve' or
`url-retrieve-synchronously'."

  (with-temp-buffer
    (url-insert-buffer-contents buffer url)

    (pcase type
      ('json     (json-read-from-string (buffer-string)))
      ('xml                           (xml-parse-region))
      (otherwise                         (buffer-string)))))

(defun aem--request (reqMeth finalDomain headers data responseType async-p)
  "Puts together the various elements needed to make various HTTP calls
using the `url.el' package."

  (let ((url-request-method                        reqMeth)
        (url-request-extra-headers                 headers)
        (url-request-data          (aem--concat-amps data)))
    (if async-p
        (url-retrieve finalDomain `(lambda (status)
                                     (funcall ,async-p (aem--process
                                                         (current-buffer)
                                                         ,finalDomain
                                                         ',responseType))))
      (aem--process (url-retrieve-synchronously
                      finalDomain)               finalDomain responseType))))



;; Bundles
(defun aem-get-bundles (domain &optional callback)
  ""
  (aem--request
    aem--REQUEST_GET
    (aem--create-URI domain "/system/console/bundles.json")
    '()
    '()
    'json
    callback))

(defun aem-stop-bundle (domain bundle-name &optional callback)
  ""
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/system/console/bundles/" bundle-name)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '((action . stop))
    'dunno
    callback))

(defun aem-start-bundle (domain bundle-name &optional callback)
  "Port should be 4505?"
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/system/console/bundles/" bundle-name)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '((action . start))
    'dunno
    callback))

(defun aem-uninstall-bundle (domain bundle-name &optional callback)
  "Port should be 4505?"
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/system/console/bundles/" bundle-name)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '((action . uninstall))
    'dunno
    callback))



;; Packages
(defun aem-get-packages (domain &optional callback)
  ""

  (cdr-assoc 'results (aem--request
                        aem--REQUEST_GET
                        (aem--create-URI domain "/crx/packmgr/list.jsp")
                        '()
                        '()
                        'json
                        callback)))
(defun aem-get-packages-extensive (domain &optional callback)
  ""

  (cdr-assoc 'hits (aem-query domain '((path           .   /etc/packages)
                                       (property       . jcr:primaryType)
                                       (property.value .         nt:file)
                                       (nodename       .           *.zip)
                                       (p.limit        .              -1)
                                       (p.hits         .            full)
                                       (p.nodedepth    .              -1)))))
(defun aem-get-packages-simplified (domain &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/crx/packmgr/service.jsp?cmd=ls")
    '()
    '()
    'xml
    callback))

(defun aem-download-package (domain localPath &rest args)
  ""

  (let ((path  (plist-get args :path ))
        (group (plist-get args :group))
        (name  (plist-get args :name )))
    (url-copy-file
      (aem--create-URI domain (if path path (concat
                                              "/etc/packages/"
                                              (if group (concat group "/") "")
                                              name)))
      (concat
        (directory-file-name localPath)
        "/"
        (if path (file-name-nondirectory path) name)))))

(defun aem-build-package (domain packagePath &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/crx/packmgr/service/.json" packagePath "?cmd=build")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '()
    'dunno
    callback))

(defun aem-install-package (domain packagePath &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/crx/packmgr/service/.json" packagePath)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '((cmd . install))
    'dunno
    callback))

(defun aem-delete-package (domain packagePath &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/crx/packmgr/service/.json" packagePath)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    '((cmd . delete))
    'dunno
    callback))

(defun aem-add-filter-to-package (domain packagePath filterPath &optional callback)
  ""

  (let ((path (concat packagePath "/jcr:content/vlt:definition")))
    (when (not (assoc 'filter (aem--get-node-subnodes (aem-get-subnodes domain path))))
      (let ((json (aem-create-or-update-node
                    domain
                    (concat path "/filter")
                    '((jcr:primaryType . nt:unstructured)))))
        (when (not (= (cdr-assoc 'status.code json) 201))
          (error (cdr-assoc 'status.message json)))))

    (let ((lst (mapcar
                 (lambda (elem)
                   (string-to-number (substring (symbol-name (car elem)) 1)))
                 (aem--get-node-subnodes
                   (aem-get-subnodes
                     (aem--account-get-uri aem--accounts-current-active)
                     (concat path "/filter"))))))
      (aem-create-or-update-node
        domain
        (concat path "/filter/f" (number-to-string (if (null lst)
                                                       0
                                                     (1+ (seq-max lst)))))
        `((jcr:primaryType . nt:unstructured)
          (mode            .         replace)
          (root            .     ,filterPath))))))



;; Queries
(defun aem-query (domain queries &optional callback)
  ""

  (aem--request
    aem--REQUEST_GET
    (aem--create-URI domain "/bin/querybuilder.json?" (if (consp queries)
                                                          (aem--concat-amps queries)
                                                        queries))
    '()
    '()
    'json
    callback))




;; Users
(defun aem-get-users (domain &optional callback)
  ""

  (cdr-assoc 'hits (aem-query domain '((path           .     /home/users)
                                       (property       . jcr:primaryType)
                                       (property.value .        rep:User)
                                       (p.limit        .              -1)
                                       (p.hits         .            full)))))

(defun aem-create-user (domain username password &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/libs/granite/security/post/authorizables")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    `((createUser . "") (authorizableId . ,username) (rep:password . ,password))
    'xml
    callback))

(defun aem-set-user-profile (domain path settings &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain path ".rw.html")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    (mapcar (lambda (setting)
              (cons (concat "profile/" (car setting)) (cdr setting))) settings)
    'xml
    callback))

(defun aem-add-user-to-group (domain username groupPath &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain groupPath ".rw.html")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    `((addMembers . ,username))
    'xml
    callback))



;; Groups
(defun aem-get-groups (domain &optional callback)
  ""

  (cdr-assoc 'hits (aem-query domain '((path           .    /home/groups)
                                       (property       . jcr:primaryType)
                                       (property.value .       rep:Group)
                                       (p.limit        .              -1)
                                       (p.hits         .            full)))))



;; Workflows
(defun aem-get-workflows (domain &optional callback)
  ""

  (cdr-assoc 'hits (aem-query domain '((path           . /var/workflow/models)
                                       (property       .      jcr:primaryType)
                                       (property.value .     cq:WorkflowModel)
                                       (p.limit        .                   -1)
                                       (p.hits         .                 full)
                                       (p.nodedepth    .                   -1)))))



;; Groovy
(defun aem-groovy-run-script (domain script &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/bin/groovyconsole/post.json")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    `((script . ,script))
    'json
    callback))

(defun aem-groovy-run-file (domain filePath &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/bin/groovyconsole/post.json")
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    `((script . ,(with-temp-buffer
                   (insert-file-contents filePath)

                   (buffer-string))))
    'json
    callback))



;; CRXde
(defun aem-get-node (domain path &optional callback)
  ""
  (aem--request
    aem--REQUEST_GET
    (aem--create-URI domain path ".json")
    '()
    '()
    'json
    callback))

(defun aem-get-subnodes (domain path &optional callback)
  ""
  (aem--request
    aem--REQUEST_GET
    (aem--create-URI domain path ".1.json")
    '()
    '()
    'json
    callback))

(defun aem-create-or-update-node (domain path properties &optional callback)
  ""
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain path)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    properties
    'xml
    callback))

(defun aem-delete-node (domain path &optional callback)
  ""
  (aem--request
    aem--REQUEST_DELETE
    (aem--create-URI domain path)
    '()
    '()
    'dunno
    callback))

(defun aem-delete-node-property (domain path propertyNames &optional callback)
  ""
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain path)
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    (mapcar (lambda (propertyName)
              (cons (concat propertyName "@Delete") "")) propertyNames)
    'xml
    callback))


(provide 'aem-tools_http)

;;; aem-tools_http.el ends here
