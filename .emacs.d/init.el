(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default truncate-lines 1)

(setq-default tab-width 4)

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

(use-package emacs
  :custom
  (tab-always-indent 'complete))

(use-package diminish
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))
	
(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  (modus-themes-load-theme 'ef-dream))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :hook
  ((prog-mode text-mode) . (lambda () (setq-local corfu-auto t)))
  :custom
  (corfu-cycle t)
  (corfu-preview-current nil))
  
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down)))

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

;; BEGIN Custom move to beginning of the line
(defvar newline-and-indent t)

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun mycustom/move-begin-of-line (arg)
  (interactive "p")
  (let ((column (current-column))
        (indent (current-indentation)))
    (when (and (> column 0) (= column indent))
      (move-beginning-of-line arg))
    (when (not (= column indent))
      (back-to-indentation))
    ))

(global-set-key [remap move-beginning-of-line] 'mycustom/move-begin-of-line)
;; END Custom move to beginning of the line

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

;; BEGIN Tree-sitter configuration
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4" nil nil nil)
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2" nil nil nil)
	(rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3" nil nil nil)
	(python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6" nil nil nil)
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" nil nil nil)))

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

(setq local-init-file (locate-user-emacs-file "local-init.el"))

(when (file-exists-p local-init-file)
  (load local-init-file))
