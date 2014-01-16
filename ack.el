;;; ack.el --- Emacs interface to ack

;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.8
;; Keywords: tools, processes, convenience
;; Created: 2012-03-24
;; URL: https://github.com/leoliu/ack-el

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

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defgroup ack nil
  "Run `ack' and display the results."
  :group 'tools
  :group 'processes)

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

(defcustom ack-buffer-name-function nil
  "If non-nil, a function to compute the name of an ack buffer.
See `compilation-buffer-name-function' for details."
  :type '(choice function (const nil))
  :group 'ack)

(defcustom ack-vc-grep-commands
  '((".git" . "git --no-pager grep --color -n -i")
    (".hg" . "hg grep -n -i")
    ;; Plugin bzr-grep required for bzr < 2.6
    (".bzr" . "bzr grep --color=always -n -i"))
  "An alist of vc grep commands for `ack-skel-vc-grep'.
Each element is of the form (VC_DIR . CMD)."
  :type '(repeat (cons string string))
  :group 'ack)

(defcustom ack-default-directory-function 'ack-default-directory
  "A function to return the default directory for `ack'.
It is called with one arg, the prefix arg to `ack'."
  :type 'function
  :group 'ack)

(defcustom ack-project-root-patterns
  (list (concat "\\`" (regexp-quote dir-locals-file) "\\'")
        "\\`Project\\.ede\\'"
        "\\.xcodeproj\\'"               ; xcode
        "\\`\\.ropeproject\\'"          ; python rope
        "\\`\\.\\(?:CVS\\|bzr\\|git\\|hg\\|svn\\)\\'")
  "A list of regexps to match files in a project root.
Used by `ack-guess-project-root'."
  :type '(repeat string)
  :group 'ack)

(defcustom ack-minibuffer-setup-hook nil
  "Ack-specific hook for `minibuffer-setup-hook'."
  :type 'hook
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
  ;; Handle `hg grep' output
  (when (string-match-p "^[ \t]*hg[ \t]" (car compilation-arguments))
    (setq compilation-error-regexp-alist
          '(("^\\(.+?:[0-9]+:\\)\\(?:\\([0-9]+\\):\\)?" 1 2)))
    (when (< emacs-major-version 24)
      (setq font-lock-keywords (compilation-mode-font-lock-keywords)))
    (setq-local compilation-parse-errors-filename-function
                (lambda (file)
                  (save-match-data
                    (if (string-match "\\(.+\\):\\([0-9]+\\):" file)
                        (match-string 1 file)
                      file)))))
  ;; Handle `bzr grep' output
  (when (string-match-p "^[ \t]*bzr[ \t]" (car compilation-arguments))
    (setq-local compilation-parse-errors-filename-function
                (lambda (file)
                  (save-match-data
                    ;; 'bzr grep -r' has files like `termcolor.py~147'
                    (if (string-match "\\(.+\\)~\\([0-9]+\\)" file)
                        (match-string 1 file)
                      file))))))

(defun ack-mode-display-match ()
  "Display in another window the match in current line."
  (interactive)
  (setq compilation-current-error (point))
  (next-error-no-select 0))

(define-compilation-mode ack-mode "Ack"
  "A compilation mode tailored for ack."
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-face 'compilation-info)
  (if (>= emacs-major-version 24)
      (add-hook 'compilation-filter-hook 'ack-filter nil t)
    (setq-local ack--ansi-color-last-marker (point-min-marker))
    (font-lock-add-keywords
     nil '(((lambda (limit)
              (let ((beg (marker-position ack--ansi-color-last-marker)))
                (move-marker ack--ansi-color-last-marker limit)
                (ansi-color-apply-on-region beg ack--ansi-color-last-marker))
              nil)))))
  (define-key ack-mode-map "\C-o" #'ack-mode-display-match))

(defun ack-skel-file ()
  "Insert a template for case-insensitive file name search."
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
         (backend (downcase (substring which 1)))
         (cmd (or (cdr (assoc which ack-vc-grep-commands))
                  (error "No command provided for `%s grep'" backend))))
    (setq project-root root)
    (delete-minibuffer-contents)
    (skeleton-insert '(nil cmd " '" _ "'"))))

(defun ack-yank-symbol-at-point ()
  "Yank the symbol from the window before entering the minibuffer."
  (interactive)
  (let ((symbol (and (minibuffer-selected-window)
                     (with-current-buffer
                         (window-buffer (minibuffer-selected-window))
                       (thing-at-point 'symbol)))))
    (if symbol (insert symbol)
      (minibuffer-message "No symbol found"))))

(defvar ack-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" (if (>= emacs-major-version 24)
                             'completion-at-point
                           'pcomplete))
    (define-key map "\M-I" 'ack-skel-file)
    (define-key map "\M-G" 'ack-skel-vc-grep)
    (define-key map "\M-Y" 'ack-yank-symbol-at-point)
    (define-key map "'" 'skeleton-pair-insert-maybe)
    map)
  "Keymap used for reading `ack' command and args in minibuffer.")

(defun ack-guess-project-root (start-directory &optional regexp)
  (let ((regexp (or regexp
                    (mapconcat 'identity ack-project-root-patterns "\\|")))
        (parent (file-name-directory
                 (directory-file-name (expand-file-name start-directory)))))
    (if (directory-files start-directory nil regexp)
        start-directory
      (unless (equal parent start-directory)
        (ack-guess-project-root parent regexp)))))

(defun ack-default-directory (arg)
  "A function for `ack-default-directory-function'.
With no \\[universal-argument], return `default-directory';
With one \\[universal-argument], find the project root according to
`ack-project-root-patterns';
Otherwise, interactively choose a directory."
  (cond
   ((not arg) default-directory)
   ((= (prefix-numeric-value arg) 4)
    (or (ack-guess-project-root default-directory)
        (ack-default-directory '(16))))
   (t (read-directory-name "In directory: " nil nil t))))

(defun ack-update-minibuffer-prompt (&optional _beg _end _len)
  (when (minibufferp)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (minibuffer-prompt-end))
        (when (looking-at "\\(\\w+\\)\\s-")
          (put-text-property
           (point-min) (minibuffer-prompt-end)
           'display
           (format "Run %s in `%s': "
                   (match-string-no-properties 1)
                   (file-name-nondirectory
                    (directory-file-name project-root)))))))))

(defun ack-minibuffer-setup-function ()
  (if (>= emacs-major-version 24)
      (shell-completion-vars)
    (pcomplete-shell-setup))
  (add-hook 'after-change-functions
            #'ack-update-minibuffer-prompt nil t)
  (ack-update-minibuffer-prompt)
  (run-hooks 'ack-minibuffer-setup-hook))

;; See http://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html
(defun ack-process-output-filter (proc output)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (toggle-read-only)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          ;; remove delete-to-end-of-line escape codes
          (insert (replace-regexp-in-string "\x1B\\[K" "" output))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      (toggle-read-only))))


;;;###autoload
(defun ack (command-args &optional directory)
  "Run ack using COMMAND-ARGS and collect output in a buffer.
When called interactively, the value of DIRECTORY is provided by
`ack-default-directory-function'.

The following keys are available while reading from the
minibuffer:

\\{ack-minibuffer-local-map}"
  (interactive
   (let ((project-root (or (funcall ack-default-directory-function
                                    current-prefix-arg)
                           default-directory))
         ;; Disable completion cycling; see http://debbugs.gnu.org/12221
         (completion-cycle-threshold nil))
     (list (minibuffer-with-setup-hook 'ack-minibuffer-setup-function
             (read-from-minibuffer "Ack: "
                                   ack-command
                                   ack-minibuffer-local-map
                                   nil 'ack-history))
           project-root)))
  (let ((default-directory (expand-file-name
                            (or directory default-directory))))
    ;; Change to the compilation buffer so that `ack-buffer-name-function' can
    ;; make use of `compilation-arguments'.
    (with-current-buffer (compilation-start command-args 'ack-mode)
      (set-process-filter (get-buffer-process (current-buffer)) 'ack-process-output-filter)
      (when ack-buffer-name-function
        (rename-buffer (funcall ack-buffer-name-function "ack"))))))

(provide 'ack)
;;; ack.el ends here
