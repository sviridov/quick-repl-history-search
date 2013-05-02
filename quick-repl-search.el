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
  "Keymap for the quick-repl-search prompt buffers"
  :group 'quick-repl-search)

(defvar quick-repl-search--modes-table (make-hash-table)
  "Variable in which stored different information about modes.
   Information represented as a plist.
   Plist keys:
    `:get-history-function'
    `:kill-input-function'")

(defvar quick-repl-search-mode nil
  "Minor mode for quick-repl-search prompt buffer")

(make-variable-buffer-local 'quick-repl-search-mode)

(add-to-list 'minor-mode-alist '(quick-repl-search-mode " QuickSearch"))
(add-to-list 'minor-mode-map-alist `(quick-repl-search-mode . ,quick-repl-search-mode-map))

(defvar quick-repl-search--mode-line-format
  '(" *quick-repl-search*")) ;; TODO: Add more information

;;;=================================================================================================

(cl-defmacro quick-repl-search-add-mode (major-mode history-variable &key (key #'identity)
                                                                          kill-input-function
                                                                          mode-map
                                                                          (mode-map-key (kbd "C-r")))
 `(progn
    (setf (gethash ',major-mode quick-repl-search--modes-table)
          (list
           :get-history-function (lambda () (funcall #',key ',history-variable))
           :kill-input-function ,kill-input-function))

    (define-key ,mode-map ,mode-map-key 'quick-repl-search)))

;;;=================================================================================================

(defun quick-repl-search--initialize ()
  (funcall (gethash major-mode quick-repl-search--modes-table))
  (select-window (split-window-vertically -4))
  (switch-to-buffer (generate-new-buffer "*quick-repl-search*"))
  (yank))

;;;=================================================================================================

(eval-after-load "slime"
 `(quick-repl-search-add-mode slime-repl-mode slime-repl-input-history
                              :kill-input-function #'slime-repl-kill-input
                              :mode-map slime-repl-mode-map))

;;;=================================================================================================

(provide 'quick-repl-search)

;;;=================================================================================================
