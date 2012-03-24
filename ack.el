;;; ack.el --- Emacs interface to ack

;; Copyright (C) 2012  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, processes, convenience
;; Created: 2012-03-24
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ack is a tool like grep, designed for programmers with large trees
;; of heterogeneous source code - http://betterthangrep.com/.

;;; Code:

(require 'compile)
(require 'ansi-color)

(defgroup ack nil
  "Run `ack' and display the results."
  :group 'tools
  :group 'processes)

;; Used implicitly by `define-compilation-mode'
(defcustom ack-scroll-output nil
  "Similar to `compilation-scroll-output' but for the *Ack* buffer."
  :type 'boolean
  :group 'ack)

(defcustom ack-command "ack --nogroup -- "
  "The default ack command for \\[ack].

Make sure --nogroup is present otherwise some commands may not
work as expected.

Note also options to ack can be specified in ACK_OPTIONS
environment variable and ~/.ackrc, which you can disable by the
--noenv switch."
  :type 'string
  :group 'ack)

;;; ======== END of USER OPTIONS ========

(defvar ack-history nil "History list for ack.")

(defun ack-filter ()
  "Handle match highlighting escape sequences inserted by the ack process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (when face
               (ansi-color-apply-overlay-face beg end face)
               (put-text-property beg end 'ack-color t)))))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;; Used implicitly by `define-compilation-mode'
(defvar ack-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 'compilation-error)
    ;; remove match from ack-regexp-alist before fontifying
    ("^Ack \\(?:started\\|finished at\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^Ack \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'compilation-error)
     (2 'compilation-error nil t)))
  "Additional things to highlight in ack output.
This gets tacked on the end of the generated expressions.")

(defconst ack-regexp-alist
  '(("^\\(.+?\\):\\([1-9][0-9]*\\):\\(?:\\([1-9][0-9]*\\):\\)?"
     1 2 ((lambda ()
            (let* ((beg (match-end 0))
                   (end (save-excursion
                          (goto-char beg)
                          (line-end-position)))
                   (mbeg (text-property-any beg end 'ack-color t)))
              (when mbeg (- mbeg beg))))
          .
          (lambda ()
            (let* ((beg (match-end 0))
                   (end (save-excursion
                          (goto-char beg)
                          (line-end-position)))
                   (mbeg (text-property-any beg end 'ack-color t))
                   (mend (and mbeg (next-single-property-change
                                    mbeg 'ack-color nil end))))
              (when mend (- mend beg))))))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Ack version of `compilation-error-regexp-alist' (which see).")

(define-compilation-mode ack-mode "Ack"
  "A compilation mode tailored for ack."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-face)
       'compilation-info)
  (set (make-local-variable 'compilation-error-regexp-alist)
       ack-regexp-alist)
  (add-hook 'compilation-filter-hook 'ack-filter nil t))

;;;###autoload
(defun ack (command-args)
  "Run ack using COMMAND-ARGS and collect output in a buffer.

\\{ack-mode-map}"
  (interactive
   (list (read-from-minibuffer "Run ack (like this): "
                               ack-command nil nil 'ack-history)))
  (compilation-start command-args 'ack-mode))

(provide 'ack)
;;; ack.el ends here
