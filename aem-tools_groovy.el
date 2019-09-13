;;; aem-tools_groovy.el --- functions for interfacing with AEM's JCR API
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
(require 'aem-tools_http)


;; Groovy Console
  ; Minor Mode
(define-minor-mode groovy-console-mode
  ""
  :lighter (:eval (concat
                    " GroovyConsole::["
                    (aem--account-get-alias aem--accounts-current-active)
                    "]"))
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c C-c") 'aem-groovy-execute-console)

             map))

  ; Interactive Function to Call
(defun aem-groovy-run-console ()
  ""
  (interactive)

  (delete-other-windows)

  (let ((output  (get-buffer-create "*Groovy Console Output*"))
        (console (get-buffer-create "*Groovy Console*"       )))
    (split-window-below)
    (other-window 1)

    (switch-to-buffer output)
    (font-lock-mode)
    (other-window 1)

    (switch-to-buffer console)
    (groovy-mode)
    (groovy-console-mode)))

(defun aem-groovy-execute-console ()
  ""
  (interactive)

  (let ((console (get-buffer "*Groovy Console*")))
    (if (or console (get-buffer "*Groovy Console Output*"))
        (with-current-buffer console
          (aem-groovy-run-script
            (aem--account-get-uri aem--accounts-current-active)
            (buffer-string)
            (lambda (result)
              (with-current-buffer (get-buffer "*Groovy Console Output*")
                (erase-buffer)

                (let ((error-p (format "%s" (cdr-assoc 'exceptionStackTrace result))))
                  (if (not (string-empty-p error-p))
                      (insert (propertize error-p 'font-lock-face
                                                  '(:foreground "red")))
                    (insert (propertize
                              (format "%s" (cdr-assoc 'output result))
                              'font-lock-face '(:foreground "green")))

                    (insert (propertize "===> " 'font-lock-face
                                                '(:foreground "purple")))
                    (insert (propertize
                              (format "%s" (cdr-assoc 'result result))
                              'font-lock-face '(:foreground "purple")))))))))
      (message "Groovy console or output buffer doesn't exist!"))))

(provide 'aem-tools_groovy)

;;; aem-tools_groovy.el ends here
