;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Aku Kotkavuo"
      user-mail-address "aku.kotkavuo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Own additions start here.

;; Indent levels
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; Disable pretty-code
(setq +pretty-code-enabled-modes nil)

;; ;; Persist the frame on quit!
;; (when-let* ((dims (doom-cache-get 'last-frame-size)))
;;   (cl-destructuring-bind ((left . top) width height fullscreen) dims
;;     (setq initial-frame-alist
;;           (append initial-frame-alist
;;                   `((left . ,left)
;;                     (top . ,top)
;;                     (width . ,width)
;;                     (height . ,height)
;;                     (fullscreen . ,fullscreen))))))

;; (defun save-frame-dimensions ()
;;   (doom-cache-set 'last-frame-size
;;                   (list (frame-position)
;;                         (frame-width)
;;                         (frame-height)
;;                         (frame-parameter nil 'fullscreen))))

;; (add-hook 'kill-emacs-hook #'save-frame-dimensions)

;; Start org-mode week properly
(setq calendar-week-start-day 1)
(setq org-agenda-start-on-weekday 1)

;; Timestamps in English
(setq system-time-locale "C")
