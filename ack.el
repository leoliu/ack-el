;;; ack.el --- Emacs interface to ack

;; Copyright (C) 2012  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, processes, convenience
;; Created: 2012-03-24
;; Version: 0.7

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
(when (>= emacs-major-version 24)
  (autoload 'shell-completion-vars "shell"))

(defgroup ack nil
  "Run `ack' and display the results."
  :group 'tools
  :group 'processes)

(defcustom ack-project-pattern-list
  (list (concat "\\`" (regexp-quote dir-locals-file) "\\'")
        "\\`Project\\.ede\\'"
        "\\.xcodeproj\\'"         ; xcode
        "\\`\\.ropeproject\\'"    ; python rope
        ;; ".git" ".svn" ".hg" ".bzr" ".CVS"
        "\\`\\.\\(?:CVS\\|bzr\\|git\\|hg\\|svn\\)\\'")
  "A list of regexps that match files in a project root."
  :type '(repeat string)
  :group 'ack)

(defcustom ack-vc-grep-commands
  '((".git" . "git --no-pager grep -i -n --color")
    ;; (".bzr" . "bzr grep")
    (".hg" . "hg grep -i -n"))
  "An alist of vc grep commands for `ack-skel-vc-grep'.
Each element is of the form (VC_DIR . CMD)."
  :type '(repeat (cons string string))
  :group 'ack)

;; Used implicitly by `define-compilation-mode'
(defcustom ack-scroll-output nil
  "Similar to `compilation-scroll-output' but for the *Ack* buffer."
  :type 'boolean
  :group 'ack)

(defcustom ack-command
  ;; Note: on GNU/Linux ack may be renamed to ack-grep
  (concat (file-name-nondirectory (or (executable-find "ack-grep")
                                      (executable-find "ack")
                                      "ack")) " ")
  "The default ack command for \\[ack].

Note also options to ack can be specified in ACK_OPTIONS
environment variable and ~/.ackrc, which you can disable by the
--noenv switch."
  :type 'string
  :group 'ack)

;;; ======== END of USER OPTIONS ========

(defvar ack-history nil "History list for ack.")

(defvar ack-first-column 0
  "Value to use for `compilation-first-column' in ack buffers.")

(defvar ack-error-screen-columns nil
  "Value to use for `compilation-error-screen-columns' in ack buffers.")

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

(defvar ack-mode-font-lock-keywords
  '(("^--$" 0 'shadow)
    ;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 'compilation-error)
    ;; Remove match from ack-error-regexp-alist before fontifying
    ("^Ack \\(?:started\\|finished\\) at.*"
     (0 '(face nil compilation-message nil message nil help-echo nil mouse-face nil) t))
    ("^Ack \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil message nil help-echo nil mouse-face nil) t)
     (1 'compilation-error)
     (2 'compilation-error nil t)))
  "Additional things to highlight in ack output.
This gets tacked on the end of the generated expressions.")

(when (< emacs-major-version 24)
  (defvar ack--column-start 'ack--column-start)
  (defvar ack--column-end 'ack--column-end))

(defun ack--column-start ()
  (or (let* ((beg (match-end 0))
             (end (save-excursion
                    (goto-char beg)
                    (line-end-position)))
             (mbeg (text-property-any beg end 'ack-color t)))
        (when mbeg (- mbeg beg)))
      ;; Use column number from `ack' itself if available
      (when (match-string 4)
        (1- (string-to-number (match-string 4))))))

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
                     (progn
                       (put-text-property (line-beginning-position)
                                          (line-end-position)
                                          'font-lock-face compilation-info-face)
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))))
    (put-text-property (line-beginning-position)
                       (min (1+ (line-end-position)) (point-max)) 'ack-file file)
    (list file)))

;;; For emacs < 24
(when (< emacs-major-version 24)
  (defun ack--line (file col)
    (if (string-match-p "\\`[1-9][0-9]*\\'" (car file))
        (let ((has-ansi-color (overlays-at (match-beginning 1))))
          ;; See `compilation-mode-font-lock-keywords' where there is
          ;; overriding font-locking of FILE. Thus use the display
          ;; property here to avoid being overridden.
          (put-text-property
           (match-beginning 1) (match-end 1)
           'display
           (propertize (match-string-no-properties 1)
                       'face (list (and (not has-ansi-color)
                                        compilation-line-face)
                                   :weight 'normal :inherit 'underline)))
          (list nil (ack--file)
                (string-to-number (match-string 1))
                (1- (string-to-number (match-string 3)))))
      (put-text-property (match-beginning 3)
                         (match-end 3)
                         'font-lock-face compilation-line-face)
      (list nil file
            (string-to-number (match-string 3))
            (when (match-string 4)
              (put-text-property (match-beginning 4)
                                 (match-end 4)
                                 'font-lock-face compilation-column-face)
              (1- (string-to-number (match-string 4))))))))

;;; In emacs-24 and above, `compilation-mode-font-lock-keywords' ->
;;; `compilation--ensure-parse' -> `compilation--parse-region' ->
;;; `compilation-parse-errors' -> `compilation-error-properties'.
;;; `compilation-error-properties' returns nil if a previous pattern
;;; in the regexp alist has already been applied in a region.
;;;
;;; In emacs-23, `ack-regexp-alist' is a part of `font-lock-keywords'
;;; after some transformation, so later entries can override earlier
;;; entries.
;;;
;;; The output of 'ack --group --column WHATEVER' matches both regexps
;;; in `ack-regexp-alist' and this fails emacs-23 in finding the right
;;; file. So ack--line is used to disambiguate this case.

(defconst ack-error-regexp-alist
  `(;; grouping line (--group or --heading)
    ("^\\([1-9][0-9]*\\)\\(:\\|-\\)\\(?:\\(?4:[1-9][0-9]*\\)\\2\\)?"
     ack--file 1 (ack--column-start . ack--column-end)
     nil nil (4 compilation-column-face nil t))
    ;; none grouping line (--nogroup or --noheading)
    ("^\\(.+?\\)\\(:\\|-\\)\\([1-9][0-9]*\\)\\2\\(?:\\(?4:[1-9][0-9]*\\)\\2\\)?"
     ,@(if (>= emacs-major-version 24)
           '(1 3 (ack--column-start . ack--column-end)
               nil nil (4 compilation-column-face nil t))
         '(1 ack--line 4)))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Ack version of `compilation-error-regexp-alist' (which see).")

(defvar ack--ansi-color-last-marker)

(defvar ack-process-setup-function 'ack-process-setup)

(defun ack-process-setup ()
  (when (string-match-p "^[ \t]*hg[ \t]" (car compilation-arguments))
    (setq compilation-error-regexp-alist
          '(("^\\(.+?:[0-9]+:\\)\\(?:\\([0-9]+\\):\\)?" 1 2)))
    (when (< emacs-major-version 24)
      (setq font-lock-keywords (compilation-mode-font-lock-keywords)))
    (make-local-variable 'compilation-parse-errors-filename-function)
    (setq compilation-parse-errors-filename-function
          (lambda (file)
            (save-match-data
              (if (string-match "\\(.+\\):\\([0-9]+\\):" file)
                  (match-string 1 file)
                file))))))

(define-compilation-mode ack-mode "Ack"
  "A compilation mode tailored for ack."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-face)
       'compilation-info)
  (if (>= emacs-major-version 24)
      (add-hook 'compilation-filter-hook 'ack-filter nil t)
    (set (make-local-variable 'ack--ansi-color-last-marker)
         (point-min-marker))
    (font-lock-add-keywords
     nil '(((lambda (limit)
              (let ((beg (marker-position ack--ansi-color-last-marker)))
                (move-marker ack--ansi-color-last-marker limit)
                (ansi-color-apply-on-region beg ack--ansi-color-last-marker))
              nil))))))

(defun ack-skel-file ()
  "Insert a template for case-insensitive filename search."
  (interactive)
  (delete-minibuffer-contents)
  (let ((ack (or (car (split-string ack-command nil t)) "ack")))
    (skeleton-insert '(nil ack " -g '(?i:" _ ")'"))))

(defvar project-root)                   ; dynamically bound in `ack'

(defun ack-skel-vc-grep ()
  "Insert a template for vc grep search."
  (interactive)
  (let* ((regexp (concat "\\`" (regexp-opt
                                (mapcar 'car ack-vc-grep-commands))
                         "\\'"))
         (root (or (ack-guess-project-root default-directory regexp)
                   (error "Cannot locate vc project root")))
         (which (car (directory-files root nil regexp)))
         (cmd (or (cdr (assoc which ack-vc-grep-commands))
                  (error "No command provided for `%s grep'"
                         (substring which 1)))))
    (setq project-root root)
    (delete-minibuffer-contents)
    (skeleton-insert '(nil cmd " '" _ "'"))))

(defvar ack-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" (if (>= emacs-major-version 24)
                             'completion-at-point
                           'pcomplete))
    (define-key map "\M-I" 'ack-skel-file)
    (define-key map "\M-G" 'ack-skel-vc-grep)
    (define-key map "'" 'skeleton-pair-insert-maybe)
    map)
  "Keymap used for reading `ack' command and args in minibuffer.")

(defun ack-guess-project-root (start-directory &optional regexp)
  (let ((regexp (or regexp
                    (mapconcat 'identity ack-project-pattern-list "\\|")))
        (parent (file-name-directory
                 (directory-file-name (expand-file-name start-directory)))))
    (if (directory-files start-directory nil regexp)
        start-directory
      (unless (equal parent start-directory)
        (ack-guess-project-root parent regexp)))))

;;;###autoload
(defun ack (command-args &optional directory)
  "Run ack using COMMAND-ARGS and collect output in a buffer.
With prefix, ask for the DIRECTORY to run ack; otherwise the
current project root is used.

The following keys are available while reading from the
minibuffer:

\\{ack-minibuffer-local-map}"
  (interactive
   (let ((project-root)
         ;; Disable completion cycling; see http://debbugs.gnu.org/12221
         (completion-cycle-threshold nil))
     (list (minibuffer-with-setup-hook (if (>= emacs-major-version 24)
                                           'shell-completion-vars
                                         'pcomplete-shell-setup)
             (read-from-minibuffer "Run ack (like this): "
                                   ack-command ack-minibuffer-local-map
                                   nil 'ack-history))
           (if current-prefix-arg
               (read-directory-name "In directory: " nil nil t)
             (or project-root
                 (ack-guess-project-root default-directory))))))
  (let ((default-directory (expand-file-name
                            (or directory default-directory))))
    (compilation-start command-args 'ack-mode)))

(provide 'ack)
;;; ack.el ends here
