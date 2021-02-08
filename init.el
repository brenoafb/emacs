;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sanemacs version 0.2.5 ;;;
;;; https://sanemacs.com   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Useful Defaults
(setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
(setq linum-format "%4d ")                ; Prettify line number format
(add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
	  (if (fboundp 'display-line-numbers-mode)
	      #'display-line-numbers-mode
	    #'linum-mode))
;; (use-package undo-tree                    ; Enable undo-tree, sane undo/redo behavior
;;   :init (global-undo-tree-mode))

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Keybindings
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)  ; De-indent selection by one tab length

;;; Offload the custom-set-variables to a separate file
;;; This keeps your init.el neater and you have the option
;;; to gitignore your custom.el if you see fit.
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)

;;; Avoid littering the user's filesystem with backups
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '((".*" . "~/.emacs.d/saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Lockfiles unfortunately cause more pain than benefit
(setq create-lockfiles nil)

;;; Load wheatgrass as the default theme if one is not loaded already
(if (not custom-enabled-themes)
    (load-theme 'wheatgrass t))

;; font
(set-face-attribute 'default nil
		    :family "Fira Code"
		    :height 130
		    :weight 'normal
		    :width 'normal)

;; orgmode
;; load org-babel language if needed
(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed"
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
      (add-to-list 'org-babel-load-languages (cons (intern language) t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    ad-do-it))

(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)
; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

; useful for high-res displays
(setq org-latex-create-formula-image-program 'dvisvgm)

(setq org-agenda-files '("~/org"))

(setq org-agenda-custom-commands
  '(("n" . "Search in notes")
    ("nt" "Note tags search" tags ""
     ((org-agenda-files (file-expand-wildcards "~/org/*.org"))))
    ("ns" "Note full text search" search ""
     ((org-agenda-files (file-expand-wildcards "~/org/*.org"))))))


;; misc

(load-theme 'spacemacs-dark t)

; disable electric '_' inside latex equations
(defun my-after-load-cdlatex ()
  (define-key cdlatex-mode-map "_" nil)
  t)
(eval-after-load "cdlatex" '(my-after-load-cdlatex))

(use-package undo-fu
  :config
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  )

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; soothe theme
; (use-package soothe-theme
;   :ensure t
;   :init (load-theme 'soothe t))
(use-package soothe-theme
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1))

(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
	helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))


(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package hindent
  :ensure t
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package ob-racket
  :straight
  (ob-racket
   :type git
   :host github
   :repo "xchrishawk/ob-racket"))

					; org-fc
;;  (use-package hydra)
;;  (use-package org-fc
;;    :straight
;;    (org-fc
;;     :type git :repo "https://git.sr.ht/~l3kn/org-fc"
;;     :files (:defaults "awk" "demo.org"))
;;    :custom
;;    (org-fc-directories '("~/org/"))
;;    :config
;;    (require 'org-fc-hydra
;;    (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
;;    (kbd "RET") 'org-fc-review-flip
;;    (kbd "n") 'org-fc-review-flip
;;    (kbd "s") 'org-fc-review-suspend-card
;;    (kbd "q") 'org-fc-review-quit)
;;
;;    (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
;;      (kbd "a") 'org-fc-review-rate-again
;;      (kbd "h") 'org-fc-review-rate-hard
;;      (kbd "g") 'org-fc-review-rate-good
;;      (kbd "e") 'org-fc-review-rate-easy
;;      (kbd "s") 'org-fc-review-suspend-card
;;      (kbd "q") 'org-fc-review-quit)
;;    (setq org-fc-directories '("~/org/"))))

;; init.el ends here
