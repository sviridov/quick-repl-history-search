;;;; quick-repl-history-search.el
;;;;
;;;; quick-repl-history-search is available under the MIT license;
;;;; see LICENSE for details
;;;;
;;;; For a detailed introduction see: README.md
;;;;
;;;; Copyright (C) 2013 Sviridov Alexander <sviridov.vmi@gmail.com>
;;;;
;;;; Change Log:
;;;;
;;;; 1.0 First released

(require 'cl)

;;;=================================================================================================

(defgroup quick-repl-history-search nil
  "Quick history search for any Emacs REPL"
  :group 'emacs
  :version "1.0"
  :link '(emacs-library-link :tag "Lisp File" "quick-repl-history-search.el"))

(defcustom quick-repl-history-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'quick-repl-history-search-next)
    (define-key map (kbd "C-r") 'quick-repl-history-search-previous)
    (define-key map (kbd "C-g") 'quick-repl-history-search-abort)
    (define-key map (kbd "RET") 'quick-repl-history-search-complete-and-send)
    (define-key map (kbd "M-RET") 'quick-repl-history-search-complete)
    map)
  "Keymap for the quick-repl-history-search prompt buffers"
  :group 'quick-repl-history-search)

(defvar quick-repl-history-search--repls-table (make-hash-table)
  "Variable in which stored different information about REPLs.
   Information represented as a plist.
   Plist keys:
    `:get-history-function'
    `:kill-input-function'
    `:send-input-function'")

(defvar quick-repl-history-search-mode nil
  "Minor mode for quick-repl-history-search prompt buffer")

(make-variable-buffer-local 'quick-repl-history-search-mode)

(add-to-list 'minor-mode-alist '(quick-repl-history-search-mode " QuickSearch"))
(add-to-list 'minor-mode-map-alist `(quick-repl-history-search-mode . ,quick-repl-history-search-mode-map))

(defvar quick-repl-history-search--mode-line-format
  '(" *quick-repl-history-search*")) ;; TODO: Add more information

(defvar quick-repl-history-search--target nil
  "The target (window . buffer) which this prompt buffer is for")

(make-variable-buffer-local 'quick-repl-history-search--target)

(defvar quick-repl-history-search--history nil
  "TODO")

(make-variable-buffer-local 'quick-repl-history-search--history)

(defvar quick-repl-history-search--history-reversed nil
  "TODO")

(make-variable-buffer-local 'quick-repl-history-search--history-reversed)

(defvar quick-repl-history-search--current-history-item nil
  "TODO")

(make-variable-buffer-local 'quick-repl-history-search--current-history-item)

(defvar quick-repl-history-search--search-direction-is-next-p nil)

(make-variable-buffer-local 'quick-repl-history-search--search-direction-is-next-p)

(defvar quick-repl-history-search--kill-ring-backup nil
  "Backup of kill ring to restore after QUICK-REPL-HISTORY-SEARCH")

(make-variable-buffer-local 'quick-repl-history-search--kill-ring-backup)

;;;=================================================================================================

(cl-defmacro quick-repl-history-search-add-repl (major-mode history-form &key kill-input-function
                                                                              send-input-function
                                                                              mode-map
                                                                              (mode-map-key (kbd "C-r")))
 `(progn
    (setf (gethash ',major-mode quick-repl-history-search--repls-table)
          (list
           :get-history-function (lambda () ,history-form)
           :kill-input-function ,kill-input-function
           :send-input-function ,send-input-function))

    (define-key ,mode-map ,mode-map-key 'quick-repl-history-search)))

;;;=================================================================================================

(defmacro quick-repl-history-search--with-target-buffer (&rest body)
  "Eval BODY with the QUICK-REPL-HISTORY-SEARCH--TARGET buffer selected"
  (let ((target (gensym "TARGET")))
    `(progn

       ;; assert that window and buffer live
       (cond ((null quick-repl-history-search--target)
              (error "quick-repl-history-search: unexpected error (quick-repl-history-search--target is nil)"))
             ((not (window-live-p (car quick-repl-history-search--target)))
              (error "quick-repl-history-search: target window is deleted"))
             ((not (buffer-live-p (cdr quick-repl-history-search--target)))
              (error "quick-repl-history-search: target buffer is killed")))

       (let ((,target quick-repl-history-search--target))
         (with-selected-window (car ,target)
           ;; if buffer is switched, switch back to the QUICK-REPL-HISTORY-SEARCH--TARGET
           (unless (eq (current-buffer) (cdr ,target))
             (switch-to-buffer (cdr ,target))
             (message "quick-repl-history-search: buffer is switched"))
           ;; eval body
           ,@body)))))

;;;=================================================================================================

(defmacro quick-repl-history-search--defgetter (name getter)
 `(defun ,name ()
    (funcall (getf (gethash major-mode quick-repl-history-search--repls-table) ,getter))))

(quick-repl-history-search--defgetter quick-repl-history-search--get-history :get-history-function)
(quick-repl-history-search--defgetter quick-repl-history-search--kill-input  :kill-input-function)
(quick-repl-history-search--defgetter quick-repl-history-search--send-input  :send-input-function)

;;;=================================================================================================

(defun quick-repl-history-search--initialize ()
  (end-of-buffer)
  (let ((kill-ring-copy (copy-list kill-ring))
        (target (cons (selected-window) (current-buffer)))
        (have-input-p (/= (point)
                          (progn
                            (quick-repl-history-search--kill-input)
                            (point)))))
    (select-window (split-window-vertically -4))
;    (select-window (minibuffer-window))
;    (read-from-minibuffer "" "" quick-repl-history-search-mode-map)
    (switch-to-buffer (generate-new-buffer "*quick-repl-history-search*"))
    (setf quick-repl-history-search--target target
          quick-repl-history-search-mode t
          quick-repl-history-search--history (quick-repl-history-search--with-target-buffer
                                              (quick-repl-history-search--get-history))
          quick-repl-history-search--history-reversed nil
          quick-repl-history-search--current-history-item nil
          quick-repl-history-search--kill-ring-backup kill-ring-copy
          mode-line-format quick-repl-history-search--mode-line-format)
    (when have-input-p
      (yank))))

(defun quick-repl-history-search--clean ()
  (let ((window (car quick-repl-history-search--target)))
    (setf quick-repl-history-search--target nil
          quick-repl-history-search--history nil
          quick-repl-history-search--history-reversed nil
          quick-repl-history-search--current-history-item nil
          kill-ring quick-repl-history-search--kill-ring-backup)
    (kill-buffer (current-buffer))
    (delete-window (selected-window))
    (select-window window)))
;    (exit-minibuffer)))

;;;=================================================================================================

(defun quick-repl-history-search--find-next (query)
  (setf quick-repl-history-search--search-direction-is-next-p t)
  (loop
   (unless quick-repl-history-search--history-reversed
     (message "No matches")
     (return))
   (push quick-repl-history-search--current-history-item quick-repl-history-search--history)
   (setf quick-repl-history-search--current-history-item (pop quick-repl-history-search--history-reversed))
   (when (string-match-p query quick-repl-history-search--current-history-item)
     (return quick-repl-history-search--current-history-item))))

(defun quick-repl-history-search--find-prev (query)
  (setf quick-repl-history-search--search-direction-is-next-p nil)
  (loop
   (unless quick-repl-history-search--history
     (message "No matches")
     (return))
   (when quick-repl-history-search--current-history-item
     (push quick-repl-history-search--current-history-item quick-repl-history-search--history-reversed))
   (setf quick-repl-history-search--current-history-item (pop quick-repl-history-search--history))
   (when (string-match-p query quick-repl-history-search--current-history-item)
     (return quick-repl-history-search--current-history-item))))

;;;=================================================================================================

;;;###autoload
(defun quick-repl-history-search ()
  (interactive)
  (quick-repl-history-search--initialize))

;;;=================================================================================================

(defun quick-repl-history-search-next ()
  (interactive)
  (let ((result (quick-repl-history-search--find-next (buffer-string))))
    (when result
      (quick-repl-history-search--with-target-buffer
       (quick-repl-history-search--kill-input)
       (insert result)))))

(defun quick-repl-history-search-previous ()
  (interactive)
  (let ((result (quick-repl-history-search--find-prev (buffer-string))))
    (when result
      (quick-repl-history-search--with-target-buffer
       (quick-repl-history-search--kill-input)
       (insert result)))))

;;;=================================================================================================

(defun quick-repl-history-search--update (&rest _)
  (when quick-repl-history-search-mode
    (unless (and quick-repl-history-search--current-history-item
                 (string-match-p (buffer-string) quick-repl-history-search--current-history-item))
      (if quick-repl-history-search--search-direction-is-next-p
          (quick-repl-history-search-next)
          (quick-repl-history-search-previous)))))

(add-hook 'after-change-functions 'quick-repl-history-search--update)

;;;=================================================================================================

(defun quick-repl-history-search-complete ()
  (interactive)
  (quick-repl-history-search--clean))

(defun quick-repl-history-search-complete-and-send ()
  (interactive)
  (quick-repl-history-search-complete)
  (quick-repl-history-search--send-input))

;;;=================================================================================================

(defun quick-repl-history-search-abort ()
  (interactive)
  (let ((query (buffer-string)))
    (quick-repl-history-search--with-target-buffer
     (quick-repl-history-search--kill-input)
     (insert query))
    (quick-repl-history-search-complete)))

;;;=================================================================================================

(eval-after-load "slime"
 `(quick-repl-history-search-add-repl slime-repl-mode slime-repl-input-history
                                      :kill-input-function #'slime-repl-kill-input
                                      :send-input-function #'slime-repl-return
                                      :mode-map slime-repl-mode-map))

;;;=================================================================================================

(provide 'quick-repl-history-search)

;;;=================================================================================================
