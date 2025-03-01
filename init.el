;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in
;; Org Mode files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;(let* ((org-dir (expand-file-name
;                 "lisp" (expand-file-name
;                         "org" (expand-file-name
;                                "src" dotfiles-dir))))
;       (org-contrib-dir (expand-file-name
;                         "lisp" (expand-file-name
;                                 "contrib" (expand-file-name
;                                            ".." org-dir))))
;       (load-path (append (list org-dir org-contrib-dir)
;                          (or load-path nil))))
;
  ;; load up Org-mode and Org-babel
;  (require 'org-install)
;  (require 'ob-tangle)
;  )

;; Load any libraries (anything in extras/*.org) first, so
;; we can use it in our own files
(setq thomanil-extras-dir (expand-file-name "extras" dotfiles-dir))
(setq thomanil-extras-files (directory-files thomanil-extras-dir t "\\.org$"))

(defun thomanil-literal-load-file (file)
  "Load an org file from ~/.emacs.d/extras - assuming it contains
code blocks which can be tangled"
  (org-babel-load-file (expand-file-name file
                                         thomanil-extras-dir)))

(mapc #'thomanil-literal-load-file thomanil-extras-files)


(add-to-list 'load-path thomanil-extras-dir)

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
(put 'upcase-region 'disabled nil)


;; Packages

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa"     . "http://melpa.milkbox.net/packages/"))


;; Server mode

(require 'server)
(unless (server-running-p) (server-start))



;; Web editing

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(define-key web-mode-map (kbd "C-f") 'web-mode-fold-or-unfold)
(define-key web-mode-map (kbd "C-'") 'web-mode-mark-and-expand)

(set-face-attribute 'web-mode-html-tag-face nil :foreground "DarkViolet")

;; Set up ido

(load-library "ido")
(ido-mode t)

;; Fix for the warnings

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'etags)
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tag-names)))
	  tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'ido-find-file-in-tag-files)

(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'magit)
(add-to-list 'load-path "~/.emacs.d/extras")

(require 'setup-magit)

(global-set-key (kbd "C-x m") 'magit-status)

(require 'string-edit)

(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactiv
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "Base directory: "
                                          nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (beginning-of-buffer))

(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

(eval-after-load "grep"
  '(progn
     ;; Don't recurse into some directories
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories "log")

     ;; Add custom keybindings
     (define-key grep-mode-map "q" 'rgrep-quit-window)
     (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)))

(projectile-global-mode)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

(load-theme 'solarized-dark t)

(electric-pair-mode t)

(tool-bar-mode 0)

(menu-bar-mode 0)

(scroll-bar-mode 0)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(column-number-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode)

(blink-cursor-mode t)

(global-hl-line-mode 1)

(setq scroll-conservatively 10000
      scroll-step 1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq fill-column 80)

(setq-default truncate-lines t)

(setq org-src-fontify-natively t)

(global-set-key (kbd "RET") 'newline-and-indent)

(when (equal system-type 'darwin)
  (set-face-attribute 'default nil :font "Monaco-16")
  (set-face-attribute 'default nil :height 140)
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))

(setq org-export-html-postamble nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "<escape>") 'hippie-expand)
;;(global-set-key (kbd "M-o") 'find-file-in-project) ;; Now using projectile instead of ffip
(global-set-key (kbd "M-o") 'projectile-find-file)
(global-set-key (kbd "M-r") 'rgrep)
(global-set-key (kbd "M-f") 'find-dired)
(global-set-key (kbd "C-f") 'ido-goto-symbol)
(global-set-key [f5] 'apply-macro-to-region-lines)
(global-set-key (kbd "M-?") 'tags-search)
(global-set-key (kbd "C-v") 'eval-buffer)
(global-set-key (kbd "C-x p") 'persp-switch)
(global-set-key (kbd "§") 'just-one-space)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; (define-key my-keys-minor-mode-map (kbd "C-'") 'er/expand-region)
;;; --> Define other "truly global" keybindings here...

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(windmove-default-keybindings)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(defun my-orgfiles ()
  (interactive)
  (dired "~/Dropbox/orgfiles/"))

(global-set-key (kbd "C-x g") 'my-orgfiles)

(defun new-orgletter ()
  (interactive)
  (let ((name (concat "~/Dropbox/orgfiles/2_tmp/" (read-string "New org file: " "tmp.org"))))
    (find-file "~/Dropbox/orgfiles/2_tmp/_template.org")
    (write-file name)))

(global-set-key (kbd "C-o") 'new-orgletter)

(defun browse-orgletters ()
  (interactive)
  (dired "~/Dropbox/orgfiles/2_tmp/"))

(global-set-key (kbd "C-p") 'browse-orgletters)

(defun edit-emacs-config ()
 (interactive)
 (find-file "~/.emacs.d/thomanil.org"))

(defun reload-emacs-config ()
  (interactive)
  (save-window-excursion
    (find-file "~/.emacs.d/init.el")
    (eval-buffer)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n"
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim\n"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut\n"
          "aliquip ex ea commodo consequat. Duis aute irure dolor in\n"
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla\n"
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in\n"
          "culpa qui officia deserunt mollit anim id est laborum."))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun ert-test-current-buffer ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t nil))

(global-set-key (kbd "C-x t") 'ert-test-current-buffer)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun edit-hostsfile ()
  (interactive)
  (sudo-find-file "/etc/hosts"))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(provide 'move-text)


(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

(defun clean-up-buffer ()
       "Perform housekeeping on the current buffer"
       (interactive)
       (save-excursion
         (whitespace-cleanup)
         (mark-whole-buffer)
         (indent-region (point) (mark))))

(global-set-key (kbd "<backtab>") 'clean-up-buffer)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/git/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/git/bin" exec-path))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/Users/thomanil/Dropbox/scripts:" (getenv "PATH")))
  (push "/Users/thomanil/Dropbox/scripts" exec-path))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-U" 'unfill-paragraph)
