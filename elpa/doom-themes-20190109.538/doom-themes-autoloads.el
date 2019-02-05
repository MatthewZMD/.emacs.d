;;; doom-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doom-Iosvkem-theme" "doom-Iosvkem-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-Iosvkem-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem")))

;;;***

;;;### (autoloads nil "doom-challenger-deep-theme" "doom-challenger-deep-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-challenger-deep-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep")))

;;;***

;;;### (autoloads nil "doom-city-lights-theme" "doom-city-lights-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-city-lights-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights")))

;;;***

;;;### (autoloads nil "doom-dracula-theme" "doom-dracula-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-dracula-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dracula-theme" '("doom-dracula")))

;;;***

;;;### (autoloads nil "doom-molokai-theme" "doom-molokai-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-molokai-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-molokai-theme" '("doom-molokai")))

;;;***

;;;### (autoloads nil "doom-nord-light-theme" "doom-nord-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-nord-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-light-theme" '("doom-nord-light")))

;;;***

;;;### (autoloads nil "doom-nord-theme" "doom-nord-theme.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doom-nord-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-theme" '("doom-nord")))

;;;***

;;;### (autoloads nil "doom-nova-theme" "doom-nova-theme.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doom-nova-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nova-theme" '("doom-nova")))

;;;***

;;;### (autoloads nil "doom-one-light-theme" "doom-one-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-one-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-light-theme" '("doom-one-light")))

;;;***

;;;### (autoloads nil "doom-one-theme" "doom-one-theme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from doom-one-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-theme" '("doom-one")))

;;;***

;;;### (autoloads nil "doom-opera-light-theme" "doom-opera-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-opera-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-light-theme" '("doom-opera-light")))

;;;***

;;;### (autoloads nil "doom-opera-theme" "doom-opera-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-opera-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-theme" '("doom-opera")))

;;;***

;;;### (autoloads nil "doom-peacock-theme" "doom-peacock-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-peacock-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-peacock-theme" '("doom-peacock")))

;;;***

;;;### (autoloads nil "doom-solarized-light-theme" "doom-solarized-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-solarized-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light")))

;;;***

;;;### (autoloads nil "doom-sourcerer-theme" "doom-sourcerer-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-sourcerer-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer")))

;;;***

;;;### (autoloads nil "doom-spacegrey-theme" "doom-spacegrey-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-spacegrey-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey")))

;;;***

;;;### (autoloads nil "doom-themes" "doom-themes.el" (0 0 0 0))
;;; Generated autoloads from doom-themes.el

(autoload 'doom-name-to-rgb "doom-themes" "\
Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

\(fn COLOR)" nil nil)

(autoload 'doom-blend "doom-themes" "\
Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

\(fn COLOR1 COLOR2 ALPHA)" nil nil)

(autoload 'doom-darken "doom-themes" "\
Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-lighten "doom-themes" "\
Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-color "doom-themes" "\
Retrieve a specific color named NAME (a symbol) from the current theme.

\(fn NAME &optional TYPE)" nil nil)

(autoload 'doom-ref "doom-themes" "\
TODO

\(fn FACE PROP &optional CLASS)" nil nil)

(autoload 'doom-themes-set-faces "doom-themes" "\
Customize THEME (a symbol) with FACES.

\(fn THEME &rest FACES)" nil nil)

(function-put 'doom-themes-set-faces 'lisp-indent-function 'defun)

(autoload 'doom-themes-org-config "doom-themes" "\
Enable custom fontification and improves doom-themes integration with org-mode.

\(fn)" nil nil)

(autoload 'doom-themes-neotree-config "doom-themes" "\
Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.

\(fn)" nil nil)

(autoload 'doom-themes-treemacs-config "doom-themes" "\
Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.

\(fn)" nil nil)

(autoload 'doom-themes-visual-bell-config "doom-themes" "\
Enable flashing the mode-line on error.

\(fn)" nil nil)

(autoload 'doom-themes-visual-bell-fn "doom-themes" "\
Blink the mode-line red briefly. Set `ring-bell-function' to this to use it.

\(fn)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes" '("doom-themes-" "def-doom-theme")))

;;;***

;;;### (autoloads nil "doom-themes-common" "doom-themes-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-common" '("doom-")))

;;;***

;;;### (autoloads nil "doom-themes-neotree" "doom-themes-neotree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-neotree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-neotree" '("doom-")))

;;;***

;;;### (autoloads nil "doom-themes-org" "doom-themes-org.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doom-themes-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-org" '("doom-org-")))

;;;***

;;;### (autoloads nil "doom-themes-treemacs" "doom-themes-treemacs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-treemacs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-treemacs" '("doom-")))

;;;***

;;;### (autoloads nil "doom-tomorrow-day-theme" "doom-tomorrow-day-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-tomorrow-day-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day")))

;;;***

;;;### (autoloads nil "doom-tomorrow-night-theme" "doom-tomorrow-night-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-tomorrow-night-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night")))

;;;***

;;;### (autoloads nil "doom-vibrant-theme" "doom-vibrant-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-vibrant-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant")))

;;;***

;;;### (autoloads nil nil ("doom-themes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-themes-autoloads.el ends here
