;;; ttx-mode.el --- TrueType/OpenType font viewer using ttx -*- lexical-binding: t; -*-

;; Copyright (C) 2026 wmedrano

;; Author: Will Medrano <will@wmedrano.dev>
;; Assisted-by: OpenCode:N/A
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: tools, fonts
;; URL: https://github.com/wmedrano/ttx-mode

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
;; font files as XML by using the `ttx` utility from fonttools.  WOFF2
;; files are also supported by first decompressing them with the
;; `woff2_decompress` utility.

;;; Code:

(require 'cl-lib)
(require 'nxml-mode)

(defgroup ttx nil
  "TrueType/OpenType font viewer."
  :group 'tools)

(defcustom ttx-command "ttx"
  "Path to the ttx executable."
  :type 'string
  :group 'ttx)

(defcustom ttx-woff2-decompress-command "woff2_decompress"
  "Path to the woff2_decompress executable."
  :type 'string
  :group 'ttx)

(defcustom ttx-default-tables '("head" "name")
  "List of table tags to load automatically when opening a font file.
Set to nil to start with only the skeleton."
  :type '(repeat string)
  :group 'ttx)

(defvar-local ttx-font-filename nil
  "The filename of the font being viewed in this buffer.")

(defvar-local ttx--temp-dir nil
  "Temporary directory used for woff2 decompression, cleaned up on buffer kill.")

(defvar-local ttx--available-tables nil
  "Alist of (TAG . LENGTH) for all tables in the font.")

(defvar-local ttx--loaded-tables nil
  "List of currently loaded table tags.")

(defun ttx--parse-table-list (output)
  "Parse OUTPUT from `ttx -l` into an alist of (TAG . LENGTH)."
  (cl-loop for line in (split-string output "\n" t)
           when (string-match "^\\s-*\\([A-Za-z0-9/]+\\)\\s-+0x[0-9A-Fa-f]+\\s-+\\([0-9]+\\)" line)
           collect (cons (match-string 1 line)
                         (string-to-number (match-string 2 line)))))

(defun ttx--generate-skeleton (tables)
  "Generate skeleton XML with TABLES listed as comments."
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<ttFont>\n"
          "  <!-- Available tables (use ttx-load-table to load): -->\n"
          (mapconcat (lambda (table)
                       (format "  <!-- %s (%s) -->"
                               (car table)
                               (file-size-human-readable (cdr table) 'iec)))
                     tables "\n")
          "\n</ttFont>\n"))

(defun ttx--run-command (&rest args)
  "Run `ttx-command' with ARGS and return stdout as a string.
Signals a `user-error' if the command is not found or exits non-zero."
  (with-temp-buffer
    (let ((exit-code (condition-case _
                         (apply #'call-process ttx-command nil t nil args)
                       (file-error
                        (user-error "Cannot run ttx.  Is it installed? Check ttx-command (currently: %s)"
                                    ttx-command)))))
      (unless (zerop exit-code)
        (user-error "Command ttx failed (exit %d).  Is ttx installed? Check ttx-command (currently: %s)"
                    exit-code ttx-command))
      (buffer-string))))

(defun ttx--load-table-list (filename)
  "Load table list from FILENAME using `ttx -l`."
  (ttx--parse-table-list (ttx--run-command "-l" filename)))

(defun ttx--xml-normalized-tag (table-tag)
  "Get TABLE-TAG's XML representation.

When outputting XML, ttx converts slashes to underscores in order to produce
valid XML."
  (string-replace "/" "_" table-tag))

(defun ttx--extract-table-xml (xml-output table-tag)
  "Extract just the TABLE-TAG element from XML-OUTPUT."
  (let* ((table-tag   (ttx--xml-normalized-tag table-tag))
         (start-regex (format "^\\s-*<%s\\b" (regexp-quote table-tag)))
         (end-regex   (format "^\\s-*</%s>" (regexp-quote table-tag))))
    (with-temp-buffer
      (insert xml-output)
      (goto-char (point-min))
      (when (re-search-forward start-regex nil t)
        (beginning-of-line)
        (let ((start (point)))
          (if (re-search-forward end-regex nil t)
              (buffer-substring-no-properties start (point))
            nil))))))

(defun ttx--decompress-woff2 (woff2-filename)
  "Decompress WOFF2-FILENAME resulting temp filename.
The temp directory is stored in `ttx--temp-dir' and cleaned up on buffer kill."
  (let* ((temp-dir (make-temp-file "ttx-woff2-" t))
         (base (file-name-nondirectory woff2-filename))
         (temp-woff2 (expand-file-name base temp-dir))
         (ttf-name (concat (file-name-sans-extension base) ".ttf"))
         (temp-ttf (expand-file-name ttf-name temp-dir)))
    (copy-file woff2-filename temp-woff2)
    (let ((exit-code (condition-case _
                         (call-process ttx-woff2-decompress-command nil nil nil temp-woff2)
                       (file-error
                        (delete-directory temp-dir t)
                        (user-error "Cannot run woff2_decompress — is it installed? Check ttx-woff2-decompress-command (currently: %s)"
                                    ttx-woff2-decompress-command)))))
      (unless (zerop exit-code)
        (delete-directory temp-dir t)
        (error "Command woff2_decompress failed with exit code %d for %s" exit-code woff2-filename)))
    (unless (file-exists-p temp-ttf)
      (delete-directory temp-dir t)
      (error "Command woff2_decompress did not produce %s" temp-ttf))
    (setq ttx--temp-dir temp-dir)
    (add-hook 'kill-buffer-hook #'ttx--cleanup-temp-dir nil t)
    temp-ttf))

(defun ttx--cleanup-temp-dir ()
  "Delete the temporary directory used for woff2 decompression."
  (when (and ttx--temp-dir (file-directory-p ttx--temp-dir))
    (delete-directory ttx--temp-dir t)))

(defun ttx--init-buffer (filename)
  "Initialize buffer for FILENAME with table skeleton."
  (let ((inhibit-read-only t)
        (tables (ttx--load-table-list filename)))
    (setq ttx--available-tables tables)
    (setq ttx--loaded-tables nil)
    (erase-buffer)
    (insert (ttx--generate-skeleton tables))
    (set-buffer-modified-p nil)
    (let* ((available-tags (mapcar #'car tables))
           (to-load (cl-remove-if-not (lambda (tag) (member tag available-tags))
                                      ttx-default-tables)))
      (ttx-load-tables to-load))
    (goto-char (point-min)))
  (setq-local buffer-read-only  t))

(defun ttx--available-tables-to-load ()
  "Return list of tables not yet loaded."
  (unless ttx-font-filename
    (user-error "Not in a ttx-mode buffer"))
  (cl-remove-if (lambda (table)
                  (member (car table) ttx--loaded-tables))
                ttx--available-tables))

(defun ttx-load-tables (table-tags)
  "Load TABLE-TAGS into the current buffer.
TABLE-TAGS is a list of table tag strings."
  (unless ttx-font-filename
    (user-error "No font file associated with this buffer"))
  (when table-tags
    (let* ((xml-output (apply #'ttx--run-command
                              "-q"
                              (append (cl-mapcan (lambda (tag) (list "-t" tag)) table-tags)
                                      (list "-o" "-" ttx-font-filename)))))
      (dolist (tag table-tags)
        (let ((table-xml (ttx--extract-table-xml xml-output tag)))
          (if (not table-xml)
              (message "Failed to extract table %s" tag)
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-max))
                (when (re-search-backward "</ttFont>" nil t)
                  (insert "\n\n" table-xml "\n")))
              (cl-pushnew tag ttx--loaded-tables :test #'string=)))))
      (set-buffer-modified-p nil))))

(defun ttx-load-all-tables ()
  "Load all available tables into the current buffer."
  (interactive)
  (let ((available (mapcar #'car (ttx--available-tables-to-load))))
    (if (null available)
        (message "All tables are already loaded")
      (ttx-load-tables available)
      (message "Loaded %d tables" (length available)))))

(defun ttx-load-table (table-tag)
  "Load TABLE-TAG into the current buffer.
If TABLE-TAG is \"all\", load all available tables."
  (interactive
   (let ((available (ttx--available-tables-to-load)))
     (if (null available)
         (user-error "All tables are already loaded")
       (let ((tags (mapcar #'car available)))
         (list (completing-read "Load table: "
                                (cons "all" tags)
                                nil t))))))
  (if (string= table-tag "all")
      (ttx-load-all-tables)
    (ttx-load-tables (list table-tag))
    (let ((search-tag (ttx--xml-normalized-tag table-tag)))
      (goto-char (point-max))
      (when (re-search-backward (format "^\\s-*<%s\\b" (regexp-quote search-tag)) nil t)
        (message "Loaded table: %s" table-tag)))))

(defun ttx-unload-table (table-tag)
  "Unload TABLE-TAG from the current buffer."
  (interactive
   (progn
     (unless ttx-font-filename
       (user-error "Not in a ttx-mode buffer"))
     (if (null ttx--loaded-tables)
         (user-error "No tables are loaded")
       (list (completing-read "Unload table: "
                              ttx--loaded-tables
                              nil t)))))
  (let ((inhibit-read-only t)
        ;; / is reserved in XML so we use underscore.
        (search-tag (ttx--xml-normalized-tag table-tag)))
    (goto-char (point-min))
    (let ((start-regex (format "^\\s-*<%s\\b" (regexp-quote search-tag)))
          (end-regex (format "^\\s-*</%s>" (regexp-quote search-tag))))
      (if (re-search-forward start-regex nil t)
          (progn
            (beginning-of-line)
            ;; Also delete preceding blank lines
            (while (and (not (bobp))
                        (save-excursion
                          (forward-line -1)
                          (looking-at-p "^\\s-*$")))
              (forward-line -1))
            (let ((start (point)))
              (if (re-search-forward end-regex nil t)
                  (progn
                    (forward-line 1)
                    (delete-region start (point))
                    (setq ttx--loaded-tables (delete table-tag ttx--loaded-tables))
                    (set-buffer-modified-p nil)
                    (message "Unloaded table: %s" table-tag))
                (user-error "Could not find end of table %s" table-tag))))
        (user-error "Table %s not found in buffer" table-tag)))))

(defvar ttx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'ttx-load-table)
    (define-key map (kbd "C-c C-M-l") #'ttx-load-all-tables)
    (define-key map (kbd "C-c C-k") #'ttx-unload-table)
    map)
  "Keymap for `ttx-mode'.")


(defun ttx-revert-buffer (_ignore-auto _noconfirm)
  "Revert the ttx buffer by re-initializing with table skeleton."
  (if ttx-font-filename
      (ttx--init-buffer ttx-font-filename)
    (error "No font file associated with this buffer")))

;;;###autoload
(define-derived-mode ttx-mode nxml-mode "TTX"
  "Major mode for viewing TrueType/OpenType font files as XML."
  :keymap ttx-mode-map
  (setq-local revert-buffer-function #'ttx-revert-buffer)
  (let ((filename          (buffer-file-name)))
    (when (and filename (not ttx-font-filename))
      (let ((font-filename
             (if (string-match-p "\\.woff2\\'" filename)
                 (ttx--decompress-woff2 filename)
               filename)))
        (setq-local ttx-font-filename font-filename)
        (ttx--init-buffer font-filename)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[ot]tf\\'" . ttx-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.woff2\\'" . ttx-mode))

(provide 'ttx-mode)
;;; ttx-mode.el ends here
