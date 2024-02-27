;; Minimal Emacs v4.0
;; ===========================================================================

;; ===========================================================================
;; Unclutter startup
;; ===========================================================================
(setq inhibit-startup-message t)
(setq cursor-type 'bar)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; https://emacs.stackexchange.com/questions/2350/how-to-toggle-fullscreen-in-emacs-gui-mode-full-screen-option-is-greyed-out-i
;; <f11>   toggle-frame-fullscreen
;; M-<f10> toggle-frame-maximized
(toggle-frame-fullscreen)

;; ===========================================================================
;; Set base font
;; https://stackoverflow.com/questions/6026713/how-do-i-change-emacs-default-font-size-and-font-type
;; ===========================================================================
(set-face-attribute 'default nil    :font "Iosevka"    :height 160)

;; ===========================================================================
;; Centralized back up
;; ===========================================================================
(if (file-directory-p "~/.emacs.d/backup")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (message "Directory does not exist: ~/.emacs.d/backup"))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; ===========================================================================
;; Using the Trash
;; https://ashok-khanna.medium.com/introduction-to-dired-mode-91cecd3a06ff
;; ===========================================================================
(if (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))

;; ===========================================================================
;; Line numbers
;; ===========================================================================
(setq display-line-numbers-width-start '5)
(setq display-line-numbers-type 'visual)
(setq display-line-numbers 'relative)

(global-display-line-numbers-mode)

(set-face-attribute 'line-number-current-line nil :foreground "#ECEFF4")

;; ===========================================================================
;; Misc Properties
;; ===========================================================================
;; Automatically follow sumbolic links
;; My config files are all symlinks. This faciliates opening those files
;; ...................................
(setq vc-follow-symlinks t)

;; recentf mode
;; List of rencently opened files
;; Use: consult-recent-file
;; ...................................
(recentf-mode 1)

;; Clean up when saving
;; ...................................
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ===========================================================================
;; parenthesis
;; ===========================================================================
(electric-pair-mode t)

;; ===========================================================================
;; mark
;; ===========================================================================
(delete-selection-mode 1)

;; ===========================================================================
;; save a recent files list
;; ===========================================================================
(recentf-mode t)

;; Highlight current line
;; Set colors using Nord https://www.nordtheme.com
;; ..............................................
(global-hl-line-mode 1)

;; Show fill line
;; This is set in conjunction with visual-fill-column mode
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)

;; set pdf resolution
;; ..............................................
(setq doc-view-resolution 220)

;; Enable transpacent encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
;; ..............................................
(require 'epa-file)
(epa-file-enable)
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
;; If this line is not there, you get error: "inappropriate ioctl for device"
(setq epa-pinentry-mode 'loopback)

;; ===========================================================================
;; Set mode-line
;; ===========================================================================
;; Date Time in mode-line
(setq display-time-format "[%Y-%m-%d %a %H:%M]")
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Clean out misc info
;;https://emacs.stackexchange.com/questions/13855/how-to-append-string-that-gets-updated-to-mode-line
(setq-default mode-line-misc-info "")

;; Left and right justified mode-line
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
  Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote (" "
             mode-line-modified
             ;; full path name: buffer-file-name
             ;; just the filename: " %b "
             buffer-file-name
             ;; major mode
             " ["
             mode-name
             mode-line-process
             "%n"
             "] "
             ))
     ;; Right.
     (quote (" "
             display-time-string
             ))))))

;; ===========================================================================
;; Package manager: straight.el
;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;; ===========================================================================
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; --------------------------------------------------------------------------
(setq package-enable-at-startup nil)

(setq find-file-visit-truename nil)

;; ===========================================================================
;; Install use-package
;; ===========================================================================
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; ===========================================================================
;; Override outdated default packages
;; ===========================================================================
;; Use updated org-mode package
;; - required by org-roam
;; This must be invoked BEFORE any org-babel-load-files occur
;; or emacs will report a version conflict
(straight-use-package 'org)

(use-package org
  :init
  (setq
   org-log-into-drawer t
   org-use-speed-commands t
   )
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  )

;; ===========================================================================
;; Install ef themes
;; ===========================================================================
;; https://protesilaos.com/emacs/ef-themes
;; ---------------------------------------------------------------------------
(straight-use-package 'ef-themes)
;;(use-package ef-themes
;;  :init
;;  (setq ef-themes-to-toggle '(ef-duo-dark ef-duo-light))
;;  :config
;;  (load-theme 'ef-duo-dark :no-confirm))

;; https://protesilaos.com/emacs/modus-themes
;; modus is built into emacs as of 28.1
;; ---------------------------------------------------------------------------
;;; For the built-in themes which cannot use `require'.
(require-theme 'modus-themes)

;; Add all your customizations prior to loading the themes.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

;; Load the theme of your choice.
(load-theme 'modus-vivendi)

;; Optionally define a key to switch between Modus themes.  Also check
;; the user option `modus-themes-to-toggle'.
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;; ===========================================================================
;; Install global packages
;; ===========================================================================
(straight-use-package 'vertico)
(use-package vertico
  :config
  (vertico-mode t))

;; ---------------------------------------------------------------------------
(straight-use-package 'orderless)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ---------------------------------------------------------------------------
(straight-use-package 'marginalia)
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; ---------------------------------------------------------------------------
(straight-use-package 'consult)
(use-package consult
  :bind
  (("M-s" . consult-line)
   :map ctl-x-map ("b" . consult-buffer))
  )

;; ---------------------------------------------------------------------------
(straight-use-package 'try)

;; ---------------------------------------------------------------------------
(straight-use-package 'visual-fill-column)
(use-package visual-fill-column
  :bind
  ("<f12>" . visual-fill-column-mode)
  :init
  (setq-default visual-fill-column-center-text t)
  ;; add padding for display-fill-column-indicator-column
  (setq-default visual-fill-column-extra-text-width (cons '3 3))
  )

;; dired-preview
;; https://protesilaos.com/emacs/dired-preview
;; ---------------------------------------------------------------------------
(straight-use-package 'dired-preview)

(use-package dired-preview
  :init
  ;; preview dealy of 0.0 causes emacs to flicker
  (setq dired-preview-delay 0.25)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
	(concat "\\."
		"\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
		"\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
		"\\|iso\\|epub\\|pdf\\)"))

  :config
  ;; Enable `dired-preview-mode' in a given Dired buffer or do it
  ;; globally:
  (dired-preview-global-mode 1)
  )

;; org-roam
;; ---------------------------------------------------------------------------
(straight-use-package 'org-roam)

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/Notebook/org-roam"))
  :config
  )

;; emacs ePub reader
;; https://tech.toryanderson.com/2022/11/23/viewing-epub-in-emacs/
;; https://depp.brause.cc/nov.el/
;; ---------------------------------------------------------------------------
(straight-use-package 'esxml)

;;
(straight-use-package 'nov)
(use-package nov
  :init
  (setq nov-text-width 80)
  (setq nov-text-width t)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  )

;; emacs org-noter
;; ---------------------------------------------------------------------------
(straight-use-package 'org-noter)

;; ===========================================================================
;; Babel configuration code
;; ===========================================================================
(org-babel-load-file (expand-file-name "~/Config/dotEmacsV4/babel/osxKeyBindings.org"))
(org-babel-load-file (expand-file-name "~/Config/dotEmacsV4/babel/orgMode.org"))
