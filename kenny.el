;;;
;;; General settings
;;;

(server-start)

(setq x-select-enable-clipboard t)
;; Set C-x C-m as M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; Set C-c C-m as M-x
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Backward kill word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; turn on visual bell
(setq visible-bell t)
;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Show line numbers
(setq line-number-mode t)
;; Save Emacs on exit
(desktop-save-mode 1)
;; Show column number
(setq column-number-mode t)
;; Disable toolbar
(tool-bar-mode -1) 
;; Disable scroll bar
(toggle-scroll-bar -1)
;; Disable menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
;; Switch to /home/kenny on start
(cd "/home/kenny")

; return a backup file path of a give file path
; with full directory mirroring from a root dir
; non-existant dir will be created
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/emacs-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
  )
)
(setq make-backup-file-name-function 'my-backup-file-name)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)

; Add directory to load path
(add-to-list 'load-path "~/.emacs.d/auto-load")

;;; Use vi-mode. Remove this line for plain emacs
(setq viper-mode t)
(require 'viper)
(require 'vimpulse)

; Turn off binding of colon to eval
(put 'eval-expression 'disabled nil)

;; Color theme
(require 'color-theme)

;;;
;;; Org mode
;;;
(setq load-path (cons "~/code/repos/org-mode/lisp" load-path))
(setq load-path (cons "~/code/repos/org-mode/contrib/lisp" load-path))
(require 'org-install)
(require 'org-latex)
;; Set org-directory
(setq org-directory "~/org/")
;; Org-mode default for .org, .org_archive, .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> h") 'bh/hide-other)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/org-scratch.org")
  (gnus-make-directory "/tmp/publish"))

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> u") 'bh/untabify)

(defun bh/untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun bh/switch-to-todo ()
  (interactive)
  (find-file (concat org-directory "todo.org"))
  "Switch to todo list")

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "M-<f11>") 'org-resolve-clocks)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c n") 'org-add-note)
(global-set-key (kbd "C-c o") 'bh/switch-to-todo)
(global-set-key (kbd "M-<f9>") (lambda ()
                                 (interactive)
                                 (unless (buffer-modified-p)
                                   (kill-buffer (current-buffer)))
                                 (delete-frame)))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly smiley stamp spelling track unmorse)))
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Org-mode Diary
(defvar org-journal-file "~/org/diary.org"
  "Path to OrgMode journal file.")
(defvar org-journal-date-format "%Y-%m-%d"
  "Date format string for journal headings.")

(global-set-key (kbd "C-c d") 'org-diary-entry)
(defun org-diary-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (unless (org-goto-local-search-forward-headings today nil t)
      ((lambda () 
         (org-insert-heading)
         (insert today)
         (insert "\n\n  \n"))))
    (beginning-of-buffer)
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 2)
    (unless (= (current-column) 2)
      (insert "\n\n  "))))

;; Org-mode capture
(setq org-default-notes-file (concat org-directory "/refile.org"))
(define-key global-map "\C-cr" 'org-capture)

(setq org-default-notes-file "~/org/refile.org")
(setq journal-file "~/org/journal.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree journal-file)
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n  %i" :immediate-finish t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))

;; org-refile
; Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)


(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))


; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("PHONE" . ?p)
                            ("QUOTE" . ?q)
                            ("WAITING" . ?w)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("MARK" . ?M)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?C)
                            ("FLAGGED" . ??))))

(defun start-journal-entry ()
  "Start a new journal entry."
  (interactive)
  (find-file journal-file)
  (goto-char (point-min))
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t)
  (open-line 2)
  (insert " "))
(global-set-key (kbd "C-c j") 'start-journal-entry)

(setq gabinete-journal-file "~/org/gabinete.org")
(defun start-gabinete-journal-entry ()
  "Start a new journal entry."
  (interactive)
  (find-file gabinete-journal-file)
  (goto-char (point-min))
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t)
  (open-line 2)
  (insert " "))
(global-set-key (kbd "C-c M-g") 'start-gabinete-journal-entry)

;; Org-mode agenda
(setq org-agenda-include-diary t)

(setq org-agenda-files (list "~/org/todo.org" 
                             "~/org/habits.org"
                             "~/org/refile.org"
                             "~/org/birthday.org"
                             ))

;; Custom agenda command definitions

;;;
;;; Zen coding
;;;
(require 'zencoding-mode)

;; Beamer and org-mode
;; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; nxhtml
(setq mumamo-background-colors nil)
;(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;;; YaSnippet
(add-to-list 'load-path "~/.emacs.d/elpa-to-submit/yasnippet")
(require 'yasnippet) 
(yas/initialize)
(yas/load-directory "~/.emacs.d/elpa-to-submit/yasnippet/snippets")

;;; Pony-mode
(add-to-list 'load-path
             "~/code/repos/pony-mode")
(require 'pony-mode)

;;; erc

(require 'erc)
(require 'erc-match)

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "kennym"
      erc-user-full-name "Kenny Meyer"
      erc-email-userid "knny.myer")

(setq erc-log-channels t
      erc-log-channels-directory "~/.erc"
      erc-log-insert-log-on-open nil
      erc-max-buffer-size 10000
      erc-log-p nil
      erc-paranoid t
      erc-auto-reconnect t)

(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%m%d.%T ")

(setq erc-keywords '("kennym"))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#olpc-paraguay" "#archlinux" "#sugar")
        ("irc.gnome.org" "#gnome-shell")))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; score up
(setq erc-pals '("tch"))

;; score down
(setq erc-fools '())
(setq erc-ignore-list '())
(setq erc-ignore-reply-list '())

;;; ERC - GTK3 notification for Emacs 24

(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

;;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;; Eshell
(setq eshell-prompt-function
  (lambda nil
    (concat
     (eshell/pwd)
     " $ ")))

;;; coffee-mode
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;(autoload 'po-mode "po-mode+"
;  "Major mode for translators to edit PO files" t) ;; po mode

  ;; load up Org-mode and Org-babel
(require 'org-drill)

(setq org-ditaa-jar-path "~/code/repos/org-mode/contrib/scripts/ditaa.jar")
(setq org-plantuml-jar-path
      (expand-file-name "~/code/repos/org-mode/contrib/scripts/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(setq php-mode-force-pear 1)

;; Wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "knny.myer@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "mattofransen")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) 

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;(setq flyspell-issue-welcome-flag nil)
;; fix flyspell problem

