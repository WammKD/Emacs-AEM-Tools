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
    '()
    '(("action" . "stop"))
    'dunno
    callback))

(defun aem-start-bundle (domain bundle-name &optional callback)
  "Port should be 4505?"
  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/system/console/bundles/" bundle-name)
    '()
    '(("action" . "start"))
    'dunno
    callback))



;; Packages
(defun aem-get-packages (domain &optional callback)
  ""

  (aem--request
    aem--REQUEST_POST
    (aem--create-URI domain "/crx/packmgr/service.jsp?cmd=ls")
    '()
    '()
    'xml
    callback))

(defun aem-download-package (domain path downloadName &optional group callback)
  ""

  (url-copy-file
    (aem--create-URI domain (concat "/etc/packages/" (if group
                                                         (concat group "/")
                                                       "") downloadName))
    (concat (directory-file-name path) "/" downloadName)))



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



;; Groups
(defun aem-get-groups (domain &optional callback)
  ""

  (cdr-assoc 'hits (aem-query domain '((path           .    /home/groups)
                                       (property       . jcr:primaryType)
                                       (property.value .       rep:Group)
                                       (p.limit        .              -1)
                                       (p.hits         .            full)))))



;; Groovy
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
(defun aem-get-subnodes (domain path &optional callback)
  ""
  (aem--request
    aem--REQUEST_GET
    (aem--create-URI domain path ".1.json")
    '()
    '()
    'json
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

(provide 'aem-tools_http)

;;; aem-tools_http.el ends here
