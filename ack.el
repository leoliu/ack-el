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
(autoload 'shell-completion-vars "shell")

(defgroup ack nil
  "Run `ack' and display the results."
  :group 'tools
  :group 'processes)

;; Used implicitly by `define-compilation-mode'
(defcustom ack-scroll-output nil
  "Similar to `compilation-scroll-output' but for the *Ack* buffer."
  :type 'boolean
  :group 'ack)

(defcustom ack-command "ack "
  "The default ack command for \\[ack].

Note also options to ack can be specified in ACK_OPTIONS
environment variable and ~/.ackrc, which you can disable by the
--noenv switch."
  :type 'string
  :group 'ack)

;;; ======== END of USER OPTIONS ========

(defvar ack-history nil "History list for ack.")

;; Used implicitly by `define-compilation-mode'
(defvar ack-first-column 0
  "Value to use for `compilation-first-column' in ack buffers.")

;; Used implicitly by `define-compilation-mode'
(defvar ack-error "ack match"
  "Stem of message to print when no matches are found.")

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
  '(("^--$" 0 'shadow)
    ;; Command output lines.
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

(defun ack--column-start ()
  (let* ((beg (match-end 0))
         (end (save-excursion
                (goto-char beg)
                (line-end-position)))
         (mbeg (text-property-any beg end 'ack-color t)))
    (when mbeg (- mbeg beg))))

(defun ack--column-end ()
  (let* ((beg (match-end 0))
         (end (save-excursion
                (goto-char beg)
                (line-end-position)))
         (mbeg (text-property-any beg end 'ack-color t))
         (mend (and mbeg (next-single-property-change
                          mbeg 'ack-color nil end))))
    (when mend (- mend beg))))

(defun ack--file ()
  (let (file)
    (save-excursion
      (while (progn
               (forward-line -1)
               (looking-at-p "^--$")))
      (setq file (or (get-text-property (line-beginning-position) 'ack-file)
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
    (put-text-property (line-beginning-position)
                       (1+ (line-end-position)) 'ack-file file)
    (list file)))

(defconst ack-regexp-alist
  '(;; grouping line (--group or --heading)
    ("^\\([1-9][0-9]*\\)\\(:\\|-\\)\\(?:[1-9][0-9]*\\2\\)?"
     ack--file 1 (ack--column-start . ack--column-end))
    ;; none grouping line (--nogroup or --noheading)
    ("^\\(.+?\\)\\(:\\|-\\)\\([1-9][0-9]*\\)\\2\\(?:\\([1-9][0-9]*\\)\\2\\)?"
     1 3 (ack--column-start . ack--column-end))
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

(defun ack-skel-file ()
  "Insert a template for case-insensitive filename search."
  (interactive)
  (delete-minibuffer-contents)
  (skeleton-insert '(nil "ack -g '(?i:" _ ")'")))

(defvar ack-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'completion-at-point)
    (define-key map "\M-I" 'ack-skel-file)
    (define-key map "'" 'skeleton-pair-insert-maybe)
    map)
  "Keymap used for reading `ack' command and args in minibuffer.")

;;;###autoload
(defun ack (command-args &optional directory)
  "Run ack using COMMAND-ARGS and collect output in a buffer.
With prefix, ask for the DIRECTORY to run ack.

The following keys are available while reading from the
minibuffer:

\\{ack-minibuffer-local-map}"
  (interactive
   (list (minibuffer-with-setup-hook 'shell-completion-vars
           (read-from-minibuffer "Run ack (like this): "
                                 ack-command ack-minibuffer-local-map
                                 nil 'ack-history))
         (and current-prefix-arg
              (read-directory-name "In directory: " nil nil t))))
  (let ((default-directory (expand-file-name
                            (or directory default-directory))))
    (compilation-start command-args 'ack-mode)))

(provide 'ack)
;;; ack.el ends here
