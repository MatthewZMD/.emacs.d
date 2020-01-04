;;; swiper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swiper" "swiper.el" (0 0 0 0))
;;; Generated autoloads from swiper.el

(autoload 'swiper-avy "swiper" "\
Jump to one of the current swiper candidates.

\(fn)" t nil)

(autoload 'swiper-backward "swiper" "\
`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-thing-at-point "swiper" "\
`swiper' with `ivy-thing-at-point'.

\(fn)" t nil)

(autoload 'swiper-all-thing-at-point "swiper" "\
`swiper-all' with `ivy-thing-at-point'.

\(fn)" t nil)

(autoload 'swiper "swiper" "\
`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-all "swiper" "\
Run `swiper' for all open buffers.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-isearch "swiper" "\
A `swiper' that's not line-based.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-isearch-backward "swiper" "\
Like `swiper-isearch' but the first result is before the point.

\(fn &optional INITIAL-INPUT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swiper" '("swiper-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swiper-autoloads.el ends here
