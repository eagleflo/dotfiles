;; Turn off menubar, toolbar & scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-splash-screen t)

;; Read changes immediately from disk
(global-auto-revert-mode)

;; Disable backups & autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Show column number in mode line
(setq column-number-mode t)

;; Show file size in mode
(size-indication-mode t)

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Spaces, not tabs. 2 spaces per tab.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 200 2))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq coffee-tab-width 2)

;; Require newlines at the end of file
(setq require-final-newline t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Highlight tabs
(setq-default highlight-tabs t)

;; We are civilized people here
(setq sentence-end-double-space nil)

;; No audible or visible bell
(setq ring-bell-function 'ignore)

;; No blinking cursor
(blink-cursor-mode 0)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-j") 'join-line)

;; Show full pathname
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Follow symbolic links to version controlled files... Helm find-file
;; seems to want this ever since I dropped .emacs under Git.
(setq-default vc-follow-symlinks t)

;; Packaging
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun packages-install (packages)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p)))
  (delete-other-windows))

(packages-install
 '(ac-cider
   ac-slime
   ag
   auto-complete
   auto-complete-clang-async
   cider
   clojure-mode
   coffee-mode
   dash-at-point
   ensime
   epc
   evil
   expand-region
   find-file-in-repository
   geiser
   ggtags
   haskell-mode
   helm
   jedi
   less-css-mode
   magit
   nginx-mode
   paredit
   rainbow-delimiters
   rust-mode
   slime
   solarized-theme
   tagedit
   tern
   tern-auto-complete
   web-mode
   wrap-region))

;; Color scheme
(load-theme 'solarized-light t)

;; Find File in Project
;(global-set-key (kbd "C-x f") 'find-file-in-repository)

;; Rainbows!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ParEdit
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)

;; tagedit
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;; Evil
;(evil-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

;; IDO
;(ido-mode t)

;; Use mdfind for locate on OS X
(setq locate-command "mdfind")

;; Require Common Lisp for Emacs for case
(require 'cl)

;; Helm
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(setq helm-c-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))
(helm-mode 1)

;; ClojureScript
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs.d/dict")
(ac-config-default)

;; Clojure, CIDER & autocomplete
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-mode))
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(add-to-list 'same-window-buffer-names "*cider*")
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Completion at point function
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook
          'set-auto-complete-as-completion-at-point-function)

;; Clang Complete
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "/usr/local/bin/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

;; SBCL
(setq inferior-lisp-program "sbcl")

;; Slime
(require 'slime)
(slime-setup '(slime-fancy))

;; Racket
(setq geiser-racket-binary "/Applications/Racket v6.1/bin/racket")

;; Python
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; ENSIME
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Tern
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; Unicode
;(unicode-fonts-setup)

;; Whitespace
(require 'whitespace)

;; Dash at point
(global-set-key "\C-cd" 'dash-at-point)

;; Size, position and font in windowed mode
(if (window-system)
  (progn (set-frame-height (selected-frame) 42)
         (set-frame-position (selected-frame) 0 0)
         ;(set-face-attribute 'default nil :family "Inconsolata")
         (set-face-attribute 'default nil :height 140)))

;; Left alt is meta, right alt remains as alt in windowed mode
(if (window-system)
  (setq mac-option-key-is-meta t
        mac-right-option-modifier nil))

;; Set super to meta on Linux
(setq x-super-keysym 'meta)

;; Make mouse work in terminal
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)

;; Turn on horizontal scrolling with mouse wheel
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(0.01))
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Calendar
(add-hook 'calendar-load-hook (lambda () (calendar-set-date-style 'european)))
(setq calendar-week-start-day 1)

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/TODO.org"
                             "~/reaktor/work.org"))

;; ERC
(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; Setting PATH for Emacs.app
(if (not (getenv "TERM_PROGRAM"))
  (let ((path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
       (setenv "PATH" path)
       (setq exec-path (split-string path ":"))))

;; Japanese
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
