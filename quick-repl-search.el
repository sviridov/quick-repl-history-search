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
    (define-key map (kbd "RET") 'quick-repl-search-complete)
    map)
  "Keymap for the quick-repl-search prompt buffers"
  :group 'quick-repl-search)

(defvar quick-repl-search--modes-table (make-hash-table)
  "Variable in which stored different information about modes.
   Information represented as a plist.
   Plist keys:
    `:get-history-function'
    `:kill-input-function'
    `:send-input-function'")

(defvar quick-repl-search-mode nil
  "Minor mode for quick-repl-search prompt buffer")

(make-variable-buffer-local 'quick-repl-search-mode)

(add-to-list 'minor-mode-alist '(quick-repl-search-mode " QuickSearch"))
(add-to-list 'minor-mode-map-alist `(quick-repl-search-mode . ,quick-repl-search-mode-map))

(defvar quick-repl-search--mode-line-format
  '(" *quick-repl-search*")) ;; TODO: Add more information

(defvar quick-repl-search--target nil
  "The target (window . buffer) which this prompt buffer is for")

(make-variable-buffer-local 'quick-repl-search--target)

(defvar quick-repl-search--history nil
  "Vector in which QUICK-REPL-SEARCH--TARGET history is stored")

(make-variable-buffer-local 'quick-repl-search--history)

(defvar quick-repl-search--history-index nil
  "Current QUICK-REPL-SEARCH--HISTORY item index")

(make-variable-buffer-local 'quick-repl-search--history-index)

;;;=================================================================================================

(cl-defmacro quick-repl-search-add-mode (major-mode history-form &key kill-input-function
                                                                      send-input-function
                                                                      mode-map
                                                                      (mode-map-key (kbd "C-r")))
 `(progn
    (setf (gethash ',major-mode quick-repl-search--modes-table)
          (list
           :get-history-function (lambda () ,history-form)
           :kill-input-function ,kill-input-function
           :send-input-function ,send-input-function))

    (define-key ,mode-map ,mode-map-key 'quick-repl-search)))

;;;=================================================================================================

(defmacro quick-repl-search--defgetter (name getter)
 `(defun ,name ()
    (funcall (getf (gethash major-mode quick-repl-search--modes-table) ,getter))))

(quick-repl-search--defgetter quick-repl-search--get-history :get-history-function)
(quick-repl-search--defgetter quick-repl-search--kill-input  :kill-input-function)
(quick-repl-search--defgetter quick-repl-search--send-input  :send-input-function)

;;;=================================================================================================

(defun quick-repl-search--initialize ()
  (end-of-buffer)
  (let ((target (cons (selected-window) (current-buffer)))
        (have-input-p (/= (point)
                          (progn
                            (quick-repl-search--kill-input)
                            (point)))))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*quick-repl-search*"))
    (setf quick-repl-search-mode t
          quick-repl-search--target target
          quick-repl-search--history (quick-repl-search--get-history)
          mode-line-format quick-repl-search--mode-line-format)
    (when have-input-p
      (yank))))

;;;=================================================================================================

(eval-after-load "slime"
 `(quick-repl-search-add-mode slime-repl-mode (coerce slime-repl-input-history 'vector)
                              :kill-input-function #'slime-repl-kill-input
                              :send-input-function #'slime-repl-return
                              :mode-map slime-repl-mode-map))

;;;=================================================================================================

(provide 'quick-repl-search)

;;;=================================================================================================
