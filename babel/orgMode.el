;; use the org-mode speed keys
(setq org-use-speed-commands t)

;; Reference
;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
;; -----------------------------
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

;; Reference
;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
;; http://xahlee.info/emacs/emacs/elisp_idioms_prompting_input.html
;; -----------------------------
(defun akh-org-content ()
  "Prompt user for the org-mode level to display."
  (interactive)
  (setq n (read-number "Type a number: " 1))
  (if (= n 1)
      (org-overview)
    (org-content n)
    ))

;; Org-Bulletjournal
;; Reference
;; https://emacs.stackexchange.com/questions/36944/key-binding-to-invoke-more-than-two-commands
;; -----------------------------
(defun bulletjournal-new-entry ()
  "Run 'insert a new bullet' and 'insert time' in sequence."
  (interactive)
  ;; Add org-return only if the line is not empty
  ;; (when current-line-empty-p nil (org-return))
  ;; NOT WORKING
  (move-end-of-line nil)
  (newline)
  (insert "* ---- ")
  (org-time-stamp '(16) nil)
  (insert " ")
  )

;; Org-Zettelkasten
  ;; https://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
  ;; -----------------------------
  (defvar ztkn-uuid-format "%Y%m%d%H%M%S"
    "Format of a zettlekasten date-time uuid")

(defun ztkn-uuid ()
    "Generate a zettlekasten UUID"
    (progn
      (format-time-string ztkn-uuid-format (current-time))))

  (defun ztkn-insert-uuid ()
    "Generate a zettlekasten UUID"
    (interactive)
    (insert (format-time-string ztkn-uuid-format (current-time)))
    )

  (defun zettlekasten-new-entry ()
    "Run 'insert a new bullet' and 'ztkn uuid' in sequence."
    (interactive)
    ;; Add org-return only if the line is not empty
    ;; (when current-line-empty-p nil (org-return))
    ;; NOT WORKING
    (move-end-of-line nil)
    (newline)
    (insert "* ")
    (ztkn-insert-uuid)
    (insert " - ")
    )

;; For org-mode file, determine what type of org file it is
;; -----------------------------
(setq myOrgSubType nil) ;; default value
(make-local-variable 'myOrgSubType) ;; give each buffer own setting

;; Function to set org-mode subtype
;; -----------------------------
;;  (defun akh-set-myOrgSubType-variable ()
;;    "Grabs the level 2 extension. Currently: jrnl, note, ztkn"
;;    (progn
;;      (if (derived-mode-p 'org-mode)
;;          (progn
;;            (setq myOrgSubType (file-name-extension (file-name-sans-extension (buffer-file-name))))
;;            ;; (message "myOrgSubType is %s" myOrgSubType)
;;            )
;;        (progn
;;          (setq myOrgSubType nil)
;;          ;; (message "myOrgSubtype is %s" myOrgSubType)
;;          ))))

(defun akh-set-myOrgSubType-variable ()
  "Grabs the level 2 extension. Currently: jrnl, note, ztkn"
  (progn
    ;; by default, it is set to nil
    (if (derived-mode-p 'org-mode)
        ;; if we are in org-mode
        (if (buffer-file-name)
            ;; if the buffer has a filename, not *scratch*
            (if (string= (file-name-extension (buffer-file-name)) "org")
                ;; if this is a .org file
                (setq myOrgSubType (file-name-extension (file-name-sans-extension (buffer-file-name))))
              )))))

  ;; manually set the org subtype
  ;; -----------------------------
  (defun akh-update-myOrgSubType-variable ()
    (interactive)
    (akh-set-myOrgSubType-variable)
    )

  ;; rebind the enter key based on org subtype only for .org files
  ;; C-RET does not bind in a terminal
  ;; -----------------------------
  (defun akh-set-myOrgSubType-keybindings ()
    ;; This will allow us to overload the return key for different org subtypes
    ;; https://stackoverflow.com/questions/13945782/emacs-auto-minor-mode-based-on-extension
    (progn
      (when (derived-mode-p 'org-mode)
        (progn
          (cond
           ((string= myOrgSubType "jrnl")
            (progn
              (local-set-key (kbd "C-RET")  #'bulletjournal-new-entry)
              (local-set-key (kbd "M-RET")  #'org-meta-return)
              (local-set-key (kbd "C-<return>")  #'bulletjournal-new-entry)
              (local-set-key (kbd "M-<return>")  #'org-meta-return)
              ))

           ((string= myOrgSubType "note")
            (progn
              (local-set-key (kbd "C-RET")  #'zettlekasten-new-entry)
              (local-set-key (kbd "M-RET")  #'org-meta-return)
              (local-set-key (kbd "C-<return>")  #'zettlekasten-new-entry)
              (local-set-key (kbd "M-<return>")  #'org-meta-return)
              ))

           (t
            (progn
              (local-set-key (kbd "C-RET")  #'org-insert-heading-respect-content)
              (local-set-key (kbd "M-RET")  #'org-meta-return)
              (local-set-key (kbd "C-<return>")  #'org-insert-heading-respect-content)
              (local-set-key (kbd "M-<return>")  #'org-meta-return)
              ))

           )))))

  ;; auto execute the function on buffer change
  ;; - This will execute when you engage the minibuffer
  ;; -----------------------------
  (add-hook 'window-configuration-change-hook
            (lambda()
              (akh-set-myOrgSubType-variable)
              (akh-set-myOrgSubType-keybindings)
              ))

;; Default key words
;; To override in a file, add the following
;; #+STARTUP: noptag
;; #+TAGS: <your custom tags>
(setq org-tag-alist '(
                      ("BUY" . ?b)
                      ("EXPENSE" . ?e)
                      ("FOLLOWUP" . ?f)
                      ("GYM" . ?g)
                      ("JUDO" . ?j)
                      ("MOOD" . ?m)
                      ("BPCC" . ?p)
                      ("X220" . ?x)))

;; Custom tags used for tech documents
;; #+TAGS: { easy(1) normal(2) hard(3) } { low(a) medium(b) high(c) bugfix(d) } { go(g) delay(d) cancel(x) }
;; We also don't want the org-tree to inherit these tags (show only top level entries)
(setq org-tags-exclude-from-inheritance
      '("easy" "normal" "hard"
        "low" "medium" "high" "bugfix"
        "go" "delay" "cancel"
        "EXPENSE"
        ))

(setq org-todo-keywords '(
                          (sequence
                           "TODO(t!)" "WAIT(w@/!)" "INPR(i!)" "|"
                           "BEGN(b)" "WRKG(g)" "DONE(d!)" "MIGR(m@/!)" "CXLD(c@/!)" "XPIR(x!)" "----(-)"
                           )))

;; Org-capture
;; -----------------------------
;; This hook is used to position the point at the bottom but not have all
;; the stuff at the top scroll off the screen
(add-hook 'org-capture-mode-hook
          (lambda ()
            (end-of-buffer)
            (recenter-top-bottom)))

(setq org-default-notes-file
      "~/Journal/2023/2023-org/2023.capture.org")

;; https://stackoverflow.com/questions/11902620/org-mode-how-do-i-create-a-new-file-with-org-capture
;; (defun akh-create-notes-file ()
;;   "Create an org file in ~/tmp/org-notes."
;;   (interactive)
;;   (let ((name (read-string "Filename: ")))
;;     (expand-file-name
;;      (format "%s.org" name) "/Users/alex/Library/Mobile Documents/com~apple~CloudDocs/Zettelkasten/inbox")))

(defun akh-create-notes-file ()
  "Create an org file in ~/tmp/org-notes."
  (progn
    (expand-file-name
     (format "%s.org" (ztkn-uuid)) "/Users/alex/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents/inbox"))
  )

;; Important:
;; (1) Captures must be a tree
;; (2) Org headers cannot be placed before the first heading
;; Zettelkasten captures must involve minimal work: open and type ideas
(setq org-capture-templates
      '(("t" "todo" entry (file "/Users/alex/Dropbox/Journal/2023/2023-org/2023.inbox.org")
         "* TODO %i%? \n:LOGBOOK: \n:CREATED: %U \n:END:")

        ("r" "Reference - a single reference" entry (file akh-create-notes-file)
         "* Metadata\t:reference:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* Reference\n")

        ("u" "Uncategorized random thoughts and ideas" entry (file akh-create-notes-file)
         "* Metadata\t:uncategorized:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* Notes\n")

        ("c" "Claim - an assertion that must be confirmed" entry (file akh-create-notes-file)
         "* Metadata\t:claim:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* References\n* Notes\n")

        ("f" "Fact - an assertion that has sufficent evidence" entry (file akh-create-notes-file)
         "* Metadata\t:fact:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* References\n* Notes\n")

        ("e" "Event - an emprically verifiable event documentated by media"
         entry (file+olp+datetree "/Users/alex/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents/timeline/timeline.org")
         "* %^{TITLE}\t:event:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n** References\n** Notes\n")


       ;; ("e" "Event - an emprically verifiable event documentated by media" entry (file akh-create-notes-file)
       ;;  "* Metadata\t:event:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* References\n* Notes\n")

        ("m" "Conceptual model, abstraction, or principle based on facts" entry (file akh-create-notes-file)
         "* Metadata\t:concept:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* References\n* Notes\n")

        ("l" "Link - reasons why two ideas are connected" entry (file akh-create-notes-file)
         "* Metadata\t:connection:\n:PROPERTIES:\n:date:\t%U\n:zuid:\t%(ztkn-uuid)\n:tags:\tnone\n:END:\n\n* Notes\n")
        ))

(global-set-key (kbd "C-c c") #'org-capture)

;; https://www.gnu.org/software/emacs/manual/html_node/org/Setting-options.html
  ;;
  ;; I AM NOT ABLE TO COMBINE THE COMMANDS
                                          ; (setq org-agenda-custom-commands
                                          ;       '(("z" todo "WAIT|INPR|BEGN|WRKG|DONE|MIGR|CXLD"
                                          ;         ((org-search-view 'TODO-ONLY))
                                          ;         )
                                          ;         ("Z" "my stuff"
                                          ;          ((org-search-view 'TODO-ONLY)
                                          ;           (todo "WAIT|INPR|BEGN|WRKG|DONE|MIGR|CXLD")
                                          ;           )
                                          ;          )
                                          ;       )
                                          ; )
  ;; WE KEEP IT SIMPLE FOR NOW
  (setq org-agenda-custom-commands
        '(
          ("z" todo "WAIT|INPR|BEGN|WRKG|DONE|MIGR|CXLD")
          )
        )

;; Formatting the agenda
;; -----------------------------
;; DEFAULT Setting
;; .............................
;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s")
;; 	(todo . " %i %-12:c")
;; 	(tags . " %i %-12:c")
;; 	(search . " %i %-12:c"))
;;       )

;; my settings
;; .............................
(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
        (todo .   " %i %-12:c")
        (tags .   " %i %-12:c")
        (search . " %i %-12:c"))
      )

  ;; Agenda files
  ;; hack - only allow one at a time
  ;; -----------------------------
  (load-library "find-lisp")
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              ;; (setq org-agenda-files (find-lisp-find-files "/Volumes/homes/alexkhon/Drive/Journal/2020" "\\.org$"))
              ;; (setq org-agenda-files (find-lisp-find-files "/Volumes/homes/alexkhon/Drive/Journal/2021" "\\.org$"))
              (setq org-agenda-files (find-lisp-find-files "~/Journal/2024" "\\.org$"))
              ))

;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (emacs-lisp . t)
   ;; Include other languages here...
   ))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

(with-eval-after-load "org"
  ;; convert this to the C-c map in org-mode
  (define-key org-mode-map (kbd "C-c x")       #'org-export-dispatch)
  (define-key org-mode-map (kbd "C-c i")       #'org-tree-to-indirect-buffer)

  (define-key org-mode-map (kbd "C-c j")       #'bulletjournal-new-entry)
  (define-key org-mode-map (kbd "C-c z")       #'zettlekasten-new-entry)

  (define-key org-mode-map (kbd "C-c e")       #'org-edit-src-code)
  )
