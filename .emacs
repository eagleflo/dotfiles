;; Identity
(setq user-full-name "Aku Kotkavuo"
      user-mail-address "aku.kotkavuo@gmail.com")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1 - Fixing bad defaults ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off menubar, toolbar & scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; No splash screen, no audible or visible bell, no blinking cursor
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

;; Show file size & column number in mode line
(size-indication-mode)
(column-number-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Read changes immediately from disk
(global-auto-revert-mode)

;; Disable backups & autosave
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2 - Indentation and whitespace preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spaces, not tabs. Either two of four spaces per tab, based on each
;; language's established conventions. (Not eight for C, though.)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(setq-default c-default-style "k&r")

;; Require newlines at the end of file
(setq require-final-newline t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Highlight tabs
(setq-default highlight-tabs t)

;; We are civilized people here
(setq sentence-end-double-space nil)

;; Size, position and font in windowed mode
(if (window-system)
    (progn (set-frame-height (selected-frame) 45)
           (set-frame-width (selected-frame) 110)
           (set-frame-position (selected-frame) 0 0)
           ;(set-face-attribute 'default nil :family "Inconsolata")
           (set-face-attribute 'default nil :height 140)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3 - Packaging ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl) ;; For every and case

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages
 '(ac-slime
   ag
   auto-complete
   auto-complete-clang-async
   cider
   clojure-mode
   cmake-mode
   coffee-mode
   company
   dash-at-point
   epc
   evil
   expand-region
   find-file-in-repository
   flycheck
   flycheck-clojure
   flx-ido
   geiser
   ggtags
   gitconfig-mode
   gitignore-mode
   haskell-mode
   helm
   helm-ag
   helm-gtags
   helm-ls-git
   helm-projectile
   jedi
   less-css-mode
   magit
   nginx-mode
   paredit
   projectile
   purescript-mode
   racket-mode
   rainbow-delimiters
   rust-mode
   slime
   solarized-theme
   tagedit
   tern
   tern-auto-complete
   web-mode
   wrap-region))

(defun my-packages-installed-p ()
  (every #'package-installed-p my-packages))

(defun install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun install-packages ()
  (unless (my-packages-installed-p)
    (message "%s" "Refreshing package database")
    (package-refresh-contents)
    (message "%s" "Done.")
    (mapc #'install-package my-packages)))

(install-packages)

;; Color scheme
(if (window-system)
    (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t))

;; Solarized is broken with TERM=xterm-256color and has been for *ages*.
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Rainbows!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ParEdit
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

;; Use mdfind for locate on OS X
(if (eq system-type 'darwin)
    (setq locate-command "mdfind"))

;; Helm
(require 'helm-config)
(require 'helm-ls-git)
(setq helm-c-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))
(helm-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-c h")   'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action)

;; helm-gtags
(custom-set-variables
 '(helm-gtags-suggested-key-mapping t))

(require 'helm-gtags)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)))

;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Clojure
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

;; ClojureScript
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; tagedit
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (tagedit-add-experimental-features)
     (add-hook 'web-mode-hook (lambda () (tagedit-mode 1)))))

;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-indent-style 2)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

;; JavaScript
(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; JSX
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;; Python
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; SBCL
(setq inferior-lisp-program "sbcl")

;; Slime
(require 'slime)
(slime-setup '(slime-fancy))

;; Racket
(if (eq system-type 'darwin)
    (setq geiser-racket-binary "/Applications/Racket v6.1.1/bin/racket"))

;; Whitespace
(require 'whitespace)

(global-set-key (kbd "C-=") 'er/expand-region)

(if (eq system-type 'darwin)
    (global-set-key "\C-cd" 'dash-at-point))

;; Autocomplete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs.d/dict")
;; (ac-config-default)

;; Clang Complete
;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "/usr/local/bin/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process))

;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; (global-auto-complete-mode)

;; Tern
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 4 - Miscellaneous ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-j") 'join-line)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)

;; Objective-C & Objective-C++
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
(add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
(add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))

(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                              "/System/Library/Frameworks" "/Library/Frameworks"))

;; Show full pathname in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Follow symbolic links to version controlled files... Helm find-file
;; seems to want this ever since I dropped .emacs under Git.
(setq-default vc-follow-symlinks t)

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

;; Eshell
(add-hook 'eshell-mode-hook (lambda () (set show-trailing-whitespace nil)))

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
