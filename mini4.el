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
;; Install ef themes
;; https://protesilaos.com/emacs/ef-themes
;; ===========================================================================
(straight-use-package 'ef-themes)
(use-package ef-themes
  :init
  (setq ef-themes-to-toggle '(ef-duo-dark ef-duo-light))
  :config
  (load-theme 'ef-duo-dark :no-confirm))

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
  (:map ctl-x-map ("b" . consult-buffer))
  )

;; ---------------------------------------------------------------------------


;; ===========================================================================
;; Babel configuration code
;; ===========================================================================
(org-babel-load-file (expand-file-name "~/Config/dotEmacsV4/babel/osxKeyBindings.org"))
