;;; pcmpl-ack.el --- completion for ack tool

;; Copyright (C) 2012  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, processes, convenience
;; Created: 2012-09-26

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

;; Provide pcompletion support for the cli tool `ack' which can be
;; downloaded from http://betterthangrep.com.
;;
;; Install:
;;   (autoload 'pcomplete/ack "pcmpl-ack")
;;
;; Usage:
;;   - To complete short options type '-' first
;;   - To complete long options type '--' first
;;   - Color name completion is also supported following
;;       --color-filename=, --color-match= and --color-lineno=.

;;; Code:

(require 'pcomplete)

(defvar pcmpl-ack-short-options
  (mapconcat (lambda (o) (substring o 1))
             '("-a" "-A" "-B" "-C" "-c" "-f" "-G" "-g"
               "-H" "-h" "-i" "-l" "-L" "-m" "-n"
               "-o" "-Q" "-r" "-R"
               "-u" "-v" "-w" "-1")
             "")
  "Short options for the `ack' command.")

(defvar pcmpl-ack-long-options
  '("--after-context="
    "--all-types"
    "--before-context="
    "--break"
    "--nobreak"
    "--color"
    "--nocolor"
    "--colour"
    "--nocolour"
    "--color-filename="
    "--color-match="
    "--color-lineno="
    "--column"
    "--context="
    "--count"
    "--env"
    "--noenv"
    "--files-with-matches"
    "--files-without-matches"
    "--flush"
    "--follow"
    "--nofollow"
    "--group"
    "--nogroup"
    "--heading"
    "--noheading"
    "--ignore-case"
    "--ignore-dir="
    "--noignore-dir="
    "--invert-match"
    "--line="
    "--literal"
    "--match"
    "--max-count="
    "--no-filename"
    "--output="
    "--pager="
    "--nopager"
    "--passthru"
    "--print0"
    "--recurse"
    "--norecurse"
    "--smart-case"
    "--nosmart-case"
    "--sort-files"
    "--type="
    "--type-add"
    "--type-set"
    "--unrestricted"
    "--with-filename"
    "--word-regexp"
    "--help"
    "--help-types"
    "--man"
    "--thpppt"
    "--version")
  "Long options for the `ack' command.")

(defvar pcmpl-ack-color-options
  '("clear"
    "reset"
    "dark"
    "bold"
    "underline"
    "underscore"
    "blink"
    "reverse"
    "concealed"
    "black"
    "red"
    "green"
    "yellow"
    "blue"
    "magenta"
    "on_black"
    "on_red"
    "on_green"
    "on_yellow"
    "on_blue"
    "on_magenta"
    "on_cyan"
    "on_white")
  "Color names for the `ack' command.")

;;;###autoload
(defun pcomplete/ack ()
  "Completion for the `ack' command.
Start an argument with '-' to complete short options and '--' for
long options."
  ;; No space after =
  (add-to-list 'pcomplete-suffix-list ?=)
  (while t
    (if (pcomplete-match "^-" 0)
        (cond
         ((pcomplete-match "^--color-\\w+=\\(\\S-*\\)" 0)
          (pcomplete-here* pcmpl-ack-color-options
                           (pcomplete-match-string 1 0) t))
         ((pcomplete-match "^--\\(?:no\\)?ignore-dir=\\(\\S-*\\)" 0)
          (pcomplete-here* (pcomplete-dirs)
                           (pcomplete-match-string 1 0) t))
         ((pcomplete-match "^--" 0)
          (pcomplete-here* pcmpl-ack-long-options))
         (t (pcomplete-opt pcmpl-ack-short-options)))
      (pcomplete-here* (pcomplete-dirs-or-entries)))))

;;;###autoload
(defalias 'pcomplete/ack-grep 'pcomplete/ack)

(provide 'pcmpl-ack)
;;; pcmpl-ack.el ends here
