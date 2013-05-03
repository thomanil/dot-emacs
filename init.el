;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in
;; Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))

;; load up Org-mode and Org-babel
(require 'org-install)
(require 'ob-tangle))

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
