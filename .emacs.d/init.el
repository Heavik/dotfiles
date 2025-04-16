(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default truncate-lines 1)

(setq ring-bell-function 'ignore)

(global-display-line-numbers-mode t) ;; Enable line numbers

;; Set fullscreen mode by default
(unless (eq (frame-parameter nil 'fullscreen) 'maximized)
  (toggle-frame-maximized))

;; Set default strings encoding to utf-8
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(electric-pair-mode 1)

;; Remember recently edited files
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Remember minibuffers prompt history
(setq history-length 30)
(savehist-mode 1)

;; Remember the last place in a file
(save-place-mode 1)

;; Automatically revert buffers for changed files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Backup and auto save files configuration
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; IDO-mode configuration
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always)

(setq-default confirm-nonexistent-file-or-buffer nil)
(ido-mode 1)

;; Set editor font
(set-face-attribute 'default nil :font "FiraMono Nerd Font Medium:style=Medium,Regular" :height 120)

;; Packages configuration
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package diminish
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
	
  (load-theme 'doom-material-dark t))

(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil
	company-minimum-prefix-length 2)
  :hook (after-init . global-company-mode))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down)))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c C-d" . eldoc)
	      ("C-c C-r" . eglot-rename)
	      ("M-." . xref-find-definitions)
	      ("C-c C-f" . eglot-format-buffer)))

(defun mycustom/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'mycustom/duplicate-line)

;; BEGIN Tree-sitter configuration
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript")))

(defun my/treesit-install-language-grammar (lang)
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(mapc #'my/treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\go.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(setq c-ts-mode-indent-offset 4)

;; END TRee-sitter configuration

;; Set up custom file location
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))

(unless (file-exists-p custom-file)
  (make-empty-file custom-file))

(load custom-file)
