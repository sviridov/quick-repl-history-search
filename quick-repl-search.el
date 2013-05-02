;;;; quick-repl-search.el
;;;;
;;;; quick-repl-search is available under the MIT license;
;;;; see LICENSE for details
;;;;
;;;; For a detailed introduction see: README.md
;;;;
;;;; Copyright (C) 2013 Sviridov Alexander <sviridov.vmi@gmail.com>

(require 'cl)

;;;=================================================================================================

(defgroup quick-repl-search nil
  "Quick search for any Emacs REPL"
  :group 'emacs
  :version "1.0"
  :link '(emacs-library-link :tag "Lisp File" "quick-repl-search.el"))

(defcustom quick-repl-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'quick-repl-search-next)
    (define-key map (kbd "C-r") 'quick-repl-search-previous)
    (define-key map (kbd "C-g") 'quick-repl-search-abort)
    map)
  "Keymap for the QUICK-REPL-SEARCH prompt buffers"
  :group 'quick-repl-search)

(defvar quick-repl-search--modes-table (make-hash-table)
  "TODO")

(defvar quick-repl-search-mode nil
  "Minor mode for QUICK-REPL-SEARCH prompt buffer")

(make-variable-buffer-local 'quick-repl-search-mode)

(add-to-list 'minor-mode-alist '(quick-repl-search-mode " QuickSearch"))
(add-to-list 'minor-mode-map-alist `(quick-repl-search-mode . ,quick-repl-search-mode-map))

(defvar quick-repl-search--mode-line-format
  '(" *quick-repl-search*"))

;;;=================================================================================================

(defmacro quick-repl-search-add-mode (mode history-variable &key (key #'identity)
                                                                    kill-input-function
                                                                    mode-map)
  "TODO")

;;;=================================================================================================

(defun quick-repl-search--initialize ()
  (funcall (gethash major-mode quick-repl-search--modes-table))
  (select-window (split-window-vertically -4))
  (switch-to-buffer (generate-new-buffer "*quick-repl-search*"))
  (yank))

;;;=================================================================================================

(provide 'quick-repl-search)

;;;=================================================================================================
