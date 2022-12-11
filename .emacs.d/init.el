(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default truncate-lines 1)
(setq ring-bell-function 'ignore)

;;Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;;Backups config
(setq make-backup-files nil)
(setq auto-save-list-files nil)
(setq auto-save-default nil)

(unless (eq (frame-parameter nil 'fullscreen) 'maximized)
  (toggle-frame-maximized))

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(fido-mode)
(windmove-default-keybindings)

(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\Program Files (x86)\\GnuWin32\\bin\\"
			 path-separator
			 (getenv "PATH")))
  )

(when (eq system-type 'darwin)
  (setq default-directory "~/")
  (add-to-list 'exec-path "/usr/local/bin")
  (setenv "PATH"
	  (concat "/usr/local/bin:" (getenv "PATH"))))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)

(use-package diminish
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))

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
   
;; Key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-k") 'next-line)
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'previous-line)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-k") 'forward-paragraph)
(global-set-key (kbd "M-i") 'backward-paragraph)
(global-set-key (kbd "C-M-j") 'backward-sexp)
(global-set-key (kbd "C-M-l") 'forward-sexp)

(global-set-key (kbd "C-<return>") 'open-next-line)
(global-set-key (kbd "S-<return>") 'open-previous-line)
(global-set-key (kbd "M-u") 'back-to-indentation)
(global-set-key (kbd "M-o") 'move-end-of-line)
(global-set-key (kbd "C-n") 'kill-line)
(global-set-key (kbd "M-n") 'kill-word)
(global-set-key (kbd "C-M-n") 'kill-sexp)
(global-set-key (kbd "C-;") 'recenter-top-bottom)
;; END Key bindings

;; Toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
;; End toggle window split
