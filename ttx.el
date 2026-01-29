;;; ttx.el --- TrueType/OpenType font viewer using ttx -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wmedrano

;; Author: wmedrano
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: tools, fonts
;; URL: https://github.com/wmedrano/ttx-el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode for viewing TrueType and OpenType
;; font files as XML by using the `ttx` utility from fonttools.

;;; Code:

(require 'nxml-mode)

(defgroup ttx nil
  "TrueType/OpenType font viewer."
  :group 'tools)

(defcustom ttx-command "ttx"
  "Path to the ttx executable."
  :type 'string
  :group 'ttx)

(defvar-local ttx-font-filename nil
  "The filename of the font being viewed in this buffer.")

(defun ttx--convert-to-xml (filename)
  "Convert FILENAME to XML using `ttx` asynchronously."
  (let ((inhibit-read-only t)
        (buffer (current-buffer))
        (full-path (expand-file-name filename)))
    (erase-buffer)
    (insert (format "<!-- Decompiling %s... -->\n" (file-name-nondirectory filename)))
    (let ((proc (start-process "ttx" buffer ttx-command "-q" "-o" "-" full-path)))
      (set-process-sentinel
       proc
       (lambda (p _msg)
         (when (and (memq (process-status p) '(exit signal))
                    (buffer-live-p buffer))
           (let ((exit-status (process-exit-status p)))
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (if (zerop exit-status)
                     (progn
                       ;; Remove the "Decompiling..." comment
                       (goto-char (point-min))
                       (delete-region (point-min) (1+ (line-end-position)))
                       (set-buffer-modified-p nil)
                       (message "TTX: Finished decompiling %s" filename))
                   (goto-char (point-max))
                   (insert (format "\n\nError: ttx failed with exit code %d" exit-status))
                   (error "TTX conversion failed for %s" filename)))))))))))

(defun ttx-revert-buffer (_ignore-auto _noconfirm)
  "Revert the ttx buffer by re-running ttx."
  (if ttx-font-filename
      (ttx--convert-to-xml ttx-font-filename)
    (error "No font file associated with this buffer")))

;;;###autoload
(define-derived-mode ttx-mode nxml-mode "TTX"
  "Major mode for viewing TrueType/OpenType font files as XML."
  (setq-local revert-buffer-function #'ttx-revert-buffer)
  (setq buffer-read-only t)
  (let ((filename (buffer-file-name)))
    (when (and filename (not ttx-font-filename))
      (setq-local ttx-font-filename filename)
      (ttx--convert-to-xml filename))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[ot]tf\\'" . ttx-mode))

(provide 'ttx)
;;; ttx.el ends here
