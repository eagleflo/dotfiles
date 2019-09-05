;; Identity
(setq user-full-name "Aku Kotkavuo"
      user-mail-address "aku.kotkavuo@gmail.com")

;; Packages to investigate:
;; - emacs-ycmd
;; - skewer-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1 - Fixing bad defaults ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off menubar, toolbar & scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

;; UTF-8 by default
(set-locale-environment "utf-8")
(prefer-coding-system 'utf-8)

;; Customization to its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Lessen the insane escaping in re-builder
(setq reb-re-syntax 'string)

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
(setq-default js-switch-indent-offset 2)
(setq-default jsx-indent-level 2)
(setq-default css-indent-offset 2)

(setq-default c-default-style "k&r")

;; No indentation inside namespaces
(c-set-offset 'innamespace 0)

;; Require newlines at the end of file
(setq require-final-newline t)

;; Show trailing whitespace, but not in all modes
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook            (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'sql-interactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'cider-repl-mode-hook      (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'slime-repl-mode-hook      (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'inferior-python-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; Highlight tabs
(setq-default highlight-tabs t)

;; We are civilized people here
(setq sentence-end-double-space nil)

;; Size, position and font in windowed mode
(if (display-graphic-p)
    (progn (set-frame-height (selected-frame) 55)
           (set-frame-width (selected-frame) 105)
           (set-frame-position (selected-frame) 0 0)
           (set-face-attribute 'default nil :family "Fira Code")
           (set-face-attribute 'default nil :height 150)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3 - Packaging ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Silence bogus warnings
(setq ad-redefinition-action 'accept)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(if (version<= emacs-version "27")
    (package-initialize))

(defvar my-packages
 '(add-node-modules-path
   cargo
   cider
   clojure-mode
   cmake-mode
   company
   company-c-headers
   company-go
   company-irony
   company-jedi
   company-racer
   counsel
   counsel-projectile
   docker
   dockerfile-mode
   elpy
   ensime
   evil
   evil-magit
   evil-surround
   exec-path-from-shell
   expand-region
   find-file-in-repository
   flycheck
   flycheck-clojure
   flycheck-irony
   flycheck-rust
   geiser
   ggtags
   gitconfig-mode
   gitignore-mode
   glsl-mode
   go-mode
   haskell-mode
   irony
   ivy
   julia-mode
   less-css-mode
   magit
   merlin
   nginx-mode
   paredit
   prettier-js
   projectile
   psci
   purescript-mode
   racer
   racket-mode
   rainbow-delimiters
   rjsx-mode
   rust-mode
   slime
   solarized-theme
   swiper
   tagedit
   tern
   terraform-mode
   tide
   tuareg
   utop
   wrap-region))

(defun my-packages-installed-p ()
  (cl-every #'package-installed-p my-packages))

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
(if (display-graphic-p)
    (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t))

(evil-mode)
(global-evil-surround-mode)

(add-to-list 'evil-emacs-state-modes 'term-mode)
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(add-to-list 'evil-emacs-state-modes 'cider-popup-buffer-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
(add-to-list 'evil-emacs-state-modes 'sql-interactive-mode)
(add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
(setq evil-emacs-state-cursor 'bar)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(require 'evil-magit)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++1z")))
(add-hook 'org-mode-hook (lambda () (flycheck-mode -1)))

;; Rainbows!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ParEdit
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook      'enable-paredit-mode)

;; Use mdfind for locate on OS X
(if (eq system-type 'darwin)
    (setq locate-command "mdfind"))

;; Ivy
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Projectile
(projectile-mode)
(setq projectile-enable-caching t)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-x C-d") 'counsel-projectile-find-file)

;; Clojure
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

;; tagedit
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (tagedit-add-experimental-features)))

;; JavaScript
(add-hook 'js-mode-hook #'add-node-modules-path)
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . js-jsx-mode))

;; Rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(setq racer-cmd "~/.cargo/bin/racer")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; Python
(setq python-shell-interpreter "python3")
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; (defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
;; (defvaralias 'flycheck-python-pylint-executable 'python-shell-interpreter)
;; (defvaralias 'flycheck-python-pycompile-executable 'python-shell-interpreter)

;; SBCL
(setq inferior-lisp-program "sbcl")

;; Slime
(require 'slime)
(slime-setup '(slime-fancy))

;; Racket
(if (eq system-type 'darwin)
    (setq geiser-racket-binary "/Applications/Racket v7.3/bin/racket"))

;; OCaml
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; PureScript
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
(add-hook 'purescript-mode-hook 'inferior-psci-mode)

;; Whitespace
(require 'whitespace)

(global-set-key (kbd "C-=") 'er/expand-region)

;; Irony mode
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony-c-headers))
(eval-after-load 'company '(add-to-list 'company-backends 'company-jedi))

;; SQL
(require 'sql)
(sql-set-product "postgres")
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

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

(setq cc-search-directories '("."
                              "../include"
                              "/usr/include"
                              "/usr/local/include/*"
                              "/System/Library/Frameworks"
                              "/Library/Frameworks"))

;; Show full pathname in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Follow symbolic links to version controlled files... Helm find-file
;; seems to want this ever since I dropped .emacs under Git.
(setq-default vc-follow-symlinks t)

;; Left cmd is meta, left alt is super, right alt remains as alt in windowed mode
(when (eq window-system 'ns)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-right-option-modifier nil
        ns-function-modifier 'hyper))

;; Set super to meta on Linux
(when (eq window-system 'x)
  (setq x-super-keysym 'meta))

;; Make mouse work in terminal
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)

;; Scrolling
(setq scroll-conservatively 1000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;(setq mouse-wheel-progressive-speed nil)

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
                             "~/reaktor/Rex.org"))

;; ERC
(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; Setting PATH for Emacs.app
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

;; Japanese
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))

;; Enable font ligatures on mac OS
(when (eq window-system 'mac)
  (set-face-attribute 'default nil :family "Fira Code Retina")
  (set-face-attribute 'default nil :height 160)
  (mac-auto-operator-composition-mode))

(setq tramp-default-method "ssh")
