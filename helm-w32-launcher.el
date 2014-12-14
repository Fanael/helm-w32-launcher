;;; helm-w32-launcher.el --- Start Menu entry launcher using Helm -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/helm-w32-launcher
;; Version: 0.1.2
;; Package-Requires: ((emacs "24") (helm "1.6.5") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Launch Start Menu entries using Emacs and Helm.
;; Why?
;;  * Why not?
;;  * Because Helm is superior to the Start Menu (or Start Screen) search
;;    feature.

;;; Code:
(require 'cl-lib)
(require 'helm)

(defgroup helm-w32-launcher nil
  "Start Menu entry launcher."
  :group 'external
  :group 'helm)

(defcustom helm-w32-launcher-csc-executable (executable-find "csc")
  "The C# compiler executable.
It can be either a file name or nil, in which case auto-detection is
attempted.
It's used only once, to compile the C# helper."
  :type '(choice (file :tag "Path")
                 (const :tag "Try to guess" nil))
  :group 'helm-w32-launcher)

(defcustom helm-w32-launcher-use-cache t
  "Whether to cache the Start Menu entries.
If non-nil, the default, cache them."
  :type 'boolean
  :group 'helm-w32-launcher)

(defcustom helm-w32-launcher-fuzzy-match nil
  "Whether to enable fuzzy matching in `helm-w32-launcher'.
If non-nil, enable fuzzy matching."
  :type 'boolean
  :group 'helm-w32-launcher)

;;;###autoload
(defun helm-w32-launcher ()
  "Launch a program as if from the Start Menu.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--launch))

;;;###autoload
(defun helm-w32-launcher-elevated ()
  "Launch a program as if from the Start Menu with elevated privileges.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--launch-elevated))

;;;###autoload
(defun helm-w32-launcher-open-shortcut-directory ()
  "Open the directory of the selected shortcut.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--open-directory))

;;;###autoload
(defun helm-w32-launcher-open-shortcut-properties ()
  "Open the properties of the selected shortcut.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--open-properties))

(defvar helm-w32-launcher--entry-cache nil
  "The Start Menu entry cache, as returned by the helper program.
It's a list of (NAME . FULL-PATH-TO-LNK-FILE).")

(defun helm-w32-launcher-flush-cache ()
  "Flush the internal `helm-w32-launcher' cache."
  (interactive)
  (setq helm-w32-launcher--entry-cache nil))

(defun helm-w32-launcher--helm (action)
  "Execute the Helm source.
ACTION is the function to call upon selecting a candidate."
  (helm :buffer "*helm w32-launcher*"
        :sources
        (helm-build-sync-source "W32 Launcher"
          :candidates (helm-w32-launcher--get-entries)
          :fuzzy-match helm-w32-launcher-fuzzy-match
          :action action
          :filtered-candidate-transformer #'helm-w32-launcher--show-path)))

(defun helm-w32-launcher--get-entries ()
  "Get Start Menu entries, possibly using the cache."
  (cond
   ((not helm-w32-launcher-use-cache)
    (helm-w32-launcher--call-helper))
   (helm-w32-launcher--entry-cache
    helm-w32-launcher--entry-cache)
   (t
    (setq helm-w32-launcher--entry-cache (helm-w32-launcher--call-helper)))))

(defun helm-w32-launcher--launch (shortcut-path)
  "Open the shortcut located at SHORTCUT-PATH."
  (w32-shell-execute "open" shortcut-path))

(defun helm-w32-launcher--launch-elevated (shortcut-path)
  "Open the shortcut located at SHORTCUT-PATH with elevated privileges."
  (w32-shell-execute "runas" shortcut-path))

(defun helm-w32-launcher--open-directory (shortcut-path)
  "Open the directory of SHORTCUT-PATH."
  (w32-shell-execute "open" (file-name-directory shortcut-path)))

(defun helm-w32-launcher--open-properties (shortcut-path)
  "Open the properites of the shortcut at SHORTCUT-PATH."
  (w32-shell-execute "properties" shortcut-path))

(defun helm-w32-launcher--show-path (candidates _source)
  "Add the full paths to the displayed list of CANDIDATES."
  (mapcar (lambda (candidate)
            (cons (concat (car candidate)
                          (propertize (concat " [" (cdr candidate) "]")
                                      'face 'font-lock-comment-face))
                  (cdr candidate)))
          candidates))

(defconst helm-w32-launcher--package-directory
  (file-name-directory (or load-file-name default-directory)))
(defconst helm-w32-launcher--helper-source
  (expand-file-name "StartMenuItems.cs" helm-w32-launcher--package-directory))
(defconst helm-w32-launcher--helper-name
  (expand-file-name "StartMenuItems.exe" helm-w32-launcher--package-directory))

(defun helm-w32-launcher--call-helper ()
  "Call the helper program to get the list of Start Menu items."
  (read
   (condition-case nil
       (helm-w32-launcher--call-process helm-w32-launcher--helper-name)
     (file-error
      ;; The helper program not found, try to compile it.
      (unless helm-w32-launcher-csc-executable
        (setq helm-w32-launcher-csc-executable
              (or (helm-w32-launcher--guess-csc-executable)
                  (error "Can't guess the path to csc.exe
Please set `helm-w32-launcher-csc-executable'"))))
      (helm-w32-launcher--call-process
       helm-w32-launcher-csc-executable
       "/nologo" "/t:exe" "/debug-" "/utf8output" "/o"
       (concat "/out:" (helm-w32-launcher--slash-to-backslash
                        helm-w32-launcher--helper-name))
       (helm-w32-launcher--slash-to-backslash helm-w32-launcher--helper-source))
      ;; Compiled successfully, try to run it again.
      (helm-w32-launcher--call-process helm-w32-launcher--helper-name)))))

(defun helm-w32-launcher--slash-to-backslash (string)
  "Return a new STRING with all slashes replaced with backslashes."
  (replace-regexp-in-string (rx "/") "\\" string t t))

(put 'helm-w32-launcher-process-returned-non-zero
     'error-conditions '(helm-w32-launcher-process-returned-non-zero error))
(put 'helm-w32-launcher-process-returned-non-zero
     'error-message "Process returned non-zero")

(defun helm-w32-launcher--call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
PROGRAM and ARGS are as in `call-process'.
The PROGRAM's output, decoded using UTF-8, is returned as a string."
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (error-code (apply #'call-process program nil t nil args))
           (result (buffer-substring-no-properties 1 (1+ (buffer-size)))))
      (when (/= 0 error-code)
        (signal 'helm-w32-launcher-process-returned-non-zero
                (list program error-code result)))
      result)))

(defun helm-w32-launcher--guess-csc-executable ()
  "Try to guess the path to the newest possible C# compiler executable."
  (cl-block return
    (let* ((dotnet-dir (expand-file-name "Microsoft.NET" (getenv "WINDIR")))
           (fx-parent-dir
            ;; Prefer the 64-bit framework.
            (let ((fx64-dir (expand-file-name "Framework64" dotnet-dir)))
              (if (file-directory-p fx64-dir)
                  fx64-dir
                (let ((fx32-dir (expand-file-name "Framework" dotnet-dir)))
                  (if (file-directory-p fx32-dir)
                      fx32-dir
                    (cl-return-from return nil))))))
           (installed-fx-directories
            (sort
             (cl-remove-if-not
              (lambda (file-and-attributes)
                (and (let ((dir-or-symlink (nth 1 file-and-attributes)))
                       (or (eq dir-or-symlink t)
                           (when (stringp dir-or-symlink)
                             ;; It's a symlink, test if it points to a
                             ;; directory.
                             (file-directory-p (nth 0 file-and-attributes)))))
                     (condition-case nil
                         ;; We don't need the attributes anymore, replace them
                         ;; with version lists to save some work further down
                         ;; the road.
                         (setcdr file-and-attributes
                                 (version-to-list
                                  (substring (nth 0 file-and-attributes) 1)))
                       ;; The name is not a recognized version number, so drop
                       ;; the element.
                       (error nil))))
              (directory-files-and-attributes
               fx-parent-dir nil (rx string-start "v") t))
             (lambda (a b)
               (version-list-< (cdr b) (cdr a))))))
      (dolist (installed-fx-dir installed-fx-directories)
        (let ((csc-path (expand-file-name
                         "csc.exe"
                         (expand-file-name (car installed-fx-dir)
                                           fx-parent-dir))))
          (when (file-exists-p csc-path)
            (cl-return-from return csc-path)))))
    nil))

(provide 'helm-w32-launcher)
;;; helm-w32-launcher.el ends here
