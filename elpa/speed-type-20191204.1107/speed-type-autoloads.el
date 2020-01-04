;;; speed-type-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "speed-type" "speed-type.el" (0 0 0 0))
;;; Generated autoloads from speed-type.el

(autoload 'speed-type-top-x "speed-type" "\
Speed type the N most common words.

\(fn N)" t nil)

(autoload 'speed-type-top-100 "speed-type" "\
Speed type the top 100 most common words.

\(fn)" t nil)

(autoload 'speed-type-top-1000 "speed-type" "\
Speed type the top 1000 most common words.

\(fn)" t nil)

(autoload 'speed-type-region "speed-type" "\
Open copy of [START,END] in a new buffer to speed type the text.

\(fn START END)" t nil)

(autoload 'speed-type-buffer "speed-type" "\
Open copy of buffer contents in a new buffer to speed type the text.

If using a prefix while calling this function (C-u), then the FULL text
will be used. Else some text will be picked randomly.

\(fn FULL)" t nil)

(autoload 'speed-type-text "speed-type" "\
Setup a new text sample to practice touch or speed typing.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "speed-type" '("speed-type-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; speed-type-autoloads.el ends here
