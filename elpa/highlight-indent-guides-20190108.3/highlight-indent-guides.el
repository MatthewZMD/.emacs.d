;;; highlight-indent-guides.el --- Minor mode to highlight indentation
;;
;; Copyright (c) 2015 DarthFennec
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; Author: DarthFennec <darthfennec@derpymail.org>
;; Version: 0.8.5
;; Package-Version: 20190108.3
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/DarthFennec/highlight-indent-guides

;;; Commentary:
;; This minor mode highlights indentation levels via font-lock.  Indent widths
;; are dynamically discovered, which means this correctly highlights in any
;; mode, regardless of indent width, even in languages with non-uniform
;; indentation such as Haskell.  This mode works properly around hard tabs and
;; mixed indentation, and it behaves well in large buffers.
;;
;; To install, put this file in your load-path, and do
;; M-x highlight-indent-guides-mode to enable it.  To enable it automatically in
;; most programming modes, use the following:
;;
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;
;; To set the display method, use:
;;
;;   (setq highlight-indent-guides-method METHOD)
;;
;; Where METHOD is either 'fill, 'column, or 'character.
;;
;; To change the character used for drawing guide lines with the 'character
;; method, use:
;;
;;   (setq highlight-indent-guides-character ?ch)
;;
;; By default, this mode automatically inspects your theme and chooses
;; appropriate colors for highlighting.  To tweak the subtlety of these colors,
;; use the following (all values are percentages):
;;
;;   (setq highlight-indent-guides-auto-odd-face-perc 15)
;;   (setq highlight-indent-guides-auto-even-face-perc 15)
;;   (setq highlight-indent-guides-auto-character-face-perc 20)
;;
;; Or, to manually set the colors used for highlighting, use:
;;
;;   (setq highlight-indent-guides-auto-enabled nil)
;;
;;   (set-face-background 'highlight-indent-guides-odd-face "color")
;;   (set-face-background 'highlight-indent-guides-even-face "color")
;;   (set-face-foreground 'highlight-indent-guides-character-face "color")

;;; Code:

(require 'color)

(defgroup highlight-indent-guides nil
  "Indentation highlighting."
  :group 'faces)

(defface highlight-indent-guides-odd-face '((t nil))
  "Face to highlight odd indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-even-face '((t nil))
  "Face to highlight even indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-character-face '((t nil))
  "Face to highlight guide line characters."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-top-odd-face '((t nil))
  "Face to highlight odd indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-top-even-face '((t nil))
  "Face to highlight even indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-top-character-face '((t nil))
  "Face to highlight guide line characters."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-stack-odd-face '((t nil))
  "Face to highlight odd indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-stack-even-face '((t nil))
  "Face to highlight even indent levels."
  :group 'highlight-indent-guides)

(defface highlight-indent-guides-stack-character-face '((t nil))
  "Face to highlight guide line characters."
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-character ?\x2502
  "Character to use to display guide lines."
  :type 'character
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-method 'fill
  "Method to use when displaying indent guides.
This can be `fill', `column', or `character'."
  :type '(choice (const fill) (const column) (const character))
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-responsive nil
  "Whether responsive highlights should be used.
This allows different highlight colors to be used in response to the location of
the point.  If this is nil, no responsive highlighting is used.  If this is
`top', the indent level of the current line is colored distinctly.  If this is
`stack', three colorations are used: one for the current indent level (as with
`top'), one for all parent levels of the current indent level, and one for all
other levels."
  :type '(choice (const nil) (const top) (const stack))
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-enabled t
  "Whether to automatically calculate faces.
If this is enabled, highlight-indent-guides will use the current theme's
background color to automatically calculate reasonable indent guide colors."
  :type 'boolean
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-highlighter-function
  'highlight-indent-guides--highlighter-default
  "Determine the correct face to use for a given indentation level.
Customizable function which applies faces to indentation.  The function is
called once per indentation character, and takes three parameters: LEVEL is the
indentation level of the character, with 0 being the outermost level.
RESPONSIVE is either nil, `top', or `stack', depending on which responsive class
the character falls into.  DISPLAY is the current display method setting, which
can be `fill', `column', or `character'.  The return value is either the face to
apply to the guide character, or nil if the guide should not be displayed at
all.  The results of this function are cached per indentation character, so the
function should consistently return the same output given the same input."
  :type 'function
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-odd-face-perc 5
  "Color adjustment percentage for highlight-indent-guides-odd-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-even-face-perc 10
  "Color adjustment percentage for highlight-indent-guides-even-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-character-face-perc 10
  "Color adjustment percentage for highlight-indent-guides-character-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-top-odd-face-perc 25
  "Color adjustment percentage for highlight-indent-guides-odd-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-top-even-face-perc 30
  "Color adjustment percentage for highlight-indent-guides-even-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-top-character-face-perc 30
  "Color adjustment percentage for highlight-indent-guides-character-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-stack-odd-face-perc 15
  "Color adjustment percentage for highlight-indent-guides-odd-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-stack-even-face-perc 20
  "Color adjustment percentage for highlight-indent-guides-even-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-auto-stack-character-face-perc 20
  "Color adjustment percentage for highlight-indent-guides-character-face.
This is used to automatically calculate the indent guide faces from the
background color."
  :type 'number
  :group 'highlight-indent-guides)

(defcustom highlight-indent-guides-delay 0.1
  "The number of seconds to wait for an idle state before redrawing.
This is only useful if `highlight-indent-guides-responsive' is not nil."
  :type 'number
  :group 'highlight-indent-guides)

(defvar highlight-indent-guides--idle-timer nil
  "The idle timer object for responsive mode.")

(defvar highlight-indent-guides--line-cache '(nil nil nil)
  "The line cache for responsive mode.")
(make-variable-buffer-local 'highlight-indent-guides--line-cache)

(defun highlight-indent-guides--try-merge-ranges (&rest args)
  "Given multiple character position ranges (ARGS), merge where possible.
When ranges are calculated separately, there is a possibility of overlap, which
can cause unnecessary redraws.  This function merges overlapping ranges to
minimize redraws."
  (let ((ranges (sort (delq nil args) (lambda (x y) (> (car x) (car y)))))
        curr next results)
    (unless (null ranges)
      (setq curr (pop ranges))
      (while ranges
        (setq next (pop ranges))
        (if (<= (car curr) (+ 2 (cdr next)))
            (setq curr (cons (car next) (max (cdr curr) (cdr next))))
          (setq results (cons curr results))
          (setq curr next)))
      (setq results (cons curr results))
      results)))

(defun highlight-indent-guides--discover-ranges (sect1 sect2)
  "Given two sections SECT1 and SECT2, discover the ranges where they differ.
Gives a list of two ranges that should be redrawn when the point moves between
SECT1 and SECT2.  This is the shallowest indent level that is not shared."
  (if (not (eq highlight-indent-guides-responsive 'stack))
      (list (car sect1) (car sect2))
    (let ((rsect1 (reverse sect1))
          (rsect2 (reverse sect2)))
      (catch 'return
        (while t
          (if (and (cdr rsect1) (cdr rsect2) (eq (car rsect1) (car rsect2)))
              (setq rsect1 (cdr rsect1) rsect2 (cdr rsect2))
            (throw 'return (list (car rsect1) (car rsect2)))))))))

(defun highlight-indent-guides--update-line-cache ()
  "Update the line cache.
The line cache tracks the current line data to make it easy for the drawing
functions to quickly access the needed context data for responsive mode.  This
function is called whenever the current line data changes."
  (let ((higp 'highlight-indent-guides-prop))
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$")))
        (forward-line))
      (back-to-indentation)
      (unless (bolp) (nth 5 (get-text-property (1- (point)) higp))))))

(defun highlight-indent-guides--try-update-line-cache ()
  "Update the line cache, if necessary.
This function is called whenever the point moves in a way that might change the
line cache.  It only updates the cache when absolutely necessary."
  (when (and highlight-indent-guides-responsive
             highlight-indent-guides-mode)
    (let ((cached-pt (car highlight-indent-guides--line-cache))
          (cached-ln (nth 1 highlight-indent-guides--line-cache))
          (cached-dt (nth 2 highlight-indent-guides--line-cache))
          (pt (point))
          ln dt rng)
      (catch 'nochange
        (when (eq pt cached-pt) (throw 'nochange nil))
        (setcar highlight-indent-guides--line-cache pt)
        (setq ln (line-number-at-pos))
        (when (eq ln cached-ln) (throw 'nochange nil))
        (setcar (cdr highlight-indent-guides--line-cache) ln)
        (setq dt (highlight-indent-guides--update-line-cache))
        (when (equal dt cached-dt) (throw 'nochange nil))
        (setcar (cddr highlight-indent-guides--line-cache) dt)
        (setq rng (highlight-indent-guides--discover-ranges dt cached-dt))
        (dolist (range (apply 'highlight-indent-guides--try-merge-ranges rng))
          (highlight-indent-guides--overdraw (car range) (cdr range)))))))

(defun highlight-indent-guides--iscdr (sub sup)
  "Calculate whether SUB is a cdr of SUP."
  (if (null sub) t
    (while (and sup (not (eq sub sup))) (setq sup (cdr sup)))
    (eq sub sup)))

(defun highlight-indent-guides--calc-guides (prev-guides)
  "Calculate the indent guides for a line.
PREV-GUIDES are the previous line's indent guides, and INDENT is this line's
indent width."
  (let ((indent (current-indentation))
        (guides (car prev-guides))
        (sections (cdr prev-guides))
        oldsections)
    (while (and guides (< indent (car guides)))
      (set-marker (cdar sections) (line-end-position 0))
      (setq oldsections sections)
      (setq sections (cdr sections))
      (setq guides (cdr guides)))
    (when (and (< 0 indent) (or (null guides) (> indent (car guides))))
      (if oldsections (setq sections oldsections)
        (let* ((lbp (line-beginning-position))
               (begmark (copy-marker lbp)) (endmark (copy-marker lbp)))
          (setq sections (cons (cons begmark endmark) sections))))
      (setq guides (cons indent guides)))
    (cons guides sections)))

(defun highlight-indent-guides--get-guides ()
  "Extract the indent guides from a line, by reading the text properties."
  (save-excursion
    (catch 'invalid
      (let (prop face seg sect nface nseg nsect guides fst)
        (while (looking-at "[[:space:]]")
          (setq prop (get-text-property (point) 'highlight-indent-guides-prop))
          (setq nface (car prop) nseg (nth 1 prop) nsect (nth 5 prop))
          (setq fst (nth 2 prop))
          (unless (natnump nface) (throw 'invalid t))
          (unless (or seg nseg)
            (when (and fst (eq face nface)) (throw 'invalid t))
            (when (not (or fst (eq face nface))) (throw 'invalid t)))
          (unless (highlight-indent-guides--iscdr sect nsect)
            (throw 'invalid t))
          (let ((l (- (length nsect) (length sect) (length nseg))))
            (when fst (setq l (1- l)))
            (when nseg (setq l (1+ l)))
            (when (not (zerop l)) (throw 'invalid t)))
          (unless (and (eq face nface) (equal seg nseg))
            (let ((col (current-column)))
              (when (and face (not (eq face nface)))
                (setq guides (cons col guides)))
              (dolist (segment nseg)
                (setq guides (cons (+ segment col) guides))
                (setq nface (1+ nface))))
            (setq face nface seg nseg))
          (setq sect nsect)
          (forward-char))
        (dolist (section sect)
          (unless (and (eq (marker-buffer (car section)) (current-buffer))
                       (eq (marker-buffer (cdr section)) (current-buffer))
                       (<= (car section) (point) (cdr section)))
            (throw 'invalid t)))
        (let ((col (current-column)))
          (when (< 0 col) (setq guides (cons col guides))))
        (cons guides sect)))))

(defun highlight-indent-guides--get-prev-guides ()
  "Scan up the buffer to find a starting point to calculate guides from."
  (let ((guides t))
    (while (and (nlistp guides) (let ((p (point)))
                                  (or (/= -1 (forward-line -1))
                                      (not (goto-char p)))))
      (unless (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                  (looking-at "[[:space:]]*$"))
        (setq guides (highlight-indent-guides--get-guides))))
    (if (listp guides) guides nil)))

(defun highlight-indent-guides--guide-line (guides)
  "Draw the indent guides specified by GUIDES on the current line."
  (let ((guides (reverse (car guides)))
        (sections (cdr guides))
        (column (current-column))
        (currpt (point))
        (starter t)
        (face 0) currcol currface props oldprop newprop subsect)
    (while guides
      (setq props nil)
      (setq currcol column)
      (setq currface face)
      (setq currpt (point))
      (forward-char)
      (setq column (current-column))
      (while (and guides (< (car guides) column))
        (setq props (cons (- (car guides) currcol) props))
        (setq guides (cdr guides))
        (setq face (1+ face)))
      (setq props (reverse props))
      (when (and props (zerop (car props)))
        (setq props (cdr props))
        (setq currface (1+ currface))
        (setq starter t))
      (setq subsect (nthcdr (1- (length guides)) sections))
      (setq oldprop (get-text-property currpt 'highlight-indent-guides-prop))
      (setq newprop
            (list currface props starter (- column currcol) nil subsect))
      (when (and oldprop
                 (eq (car newprop) (car oldprop))
                 (equal (nth 1 newprop) (nth 1 oldprop))
                 (eq (nth 2 newprop) (nth 2 oldprop))
                 (eq (nth 3 newprop) (nth 3 oldprop)))
        (setcar (nthcdr 4 newprop) (nth 4 oldprop)))
      (when guides
        (add-text-properties
         currpt (1+ currpt) `(highlight-indent-guides-prop ,newprop)))
      (setq starter nil))))

(defun highlight-indent-guides--replace-section (old search replace)
  "Replace in a list OLD section prefixes SEARCH with REPLACE.
All lines in the same section should have the same (eq) section prefixes.  If
the prefix changes on some lines, all other lines in the section need to be
updated to match."
  (let* ((oldlen (length old))
         (replen (length replace))
         (minlen (min oldlen replen))
         (cparent (nthcdr (- oldlen minlen) (cons nil old)))
         (cold (nthcdr (- oldlen minlen) old))
         (csearch (nthcdr (- replen minlen) search))
         (crepl (nthcdr (- replen minlen) replace)))
    (while (and cold (not (eq cold csearch)))
      (setq cparent (cdr cparent))
      (setq cold (cdr cold))
      (setq csearch (cdr csearch))
      (setq crepl (cdr crepl)))
    (if (null cold) old
      (setcdr cparent crepl)
      (if (car cparent) old (cdr cparent)))))

(defun highlight-indent-guides--guide-region (start end)
  "Add or update indent guides in the buffer region from START to END."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (let ((prop 'highlight-indent-guides-prop)
            (guides (highlight-indent-guides--get-prev-guides))
            (eof (< 0 (forward-line)))
            (startl (point)) (endl end)
            chunk oldguides oldsect newsect lf le rng)
        ;; for the given region, extract old guides and calculate new guides
        (while (not (or eof (and (>= (point) endl)
                                 (not (eq oldguides t))
                                 (equal (car guides) (car oldguides))
                                 (eq (cdr guides) (cdr oldguides)))))
          (if (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                  (looking-at "[[:space:]]*$"))
              (setq chunk (cons (list (point)) chunk))
            (let ((tmpguides (cdr guides)) ends currend)
              (while tmpguides
                (when (car tmpguides)
                  (setq ends (cons (marker-position (cdar tmpguides)) ends)))
                (setq tmpguides (cdr tmpguides)))
              (setq guides (highlight-indent-guides--calc-guides guides))
              (setq endl (max endl (or (nth (length (cdr guides)) ends) 0))))
            (setq oldguides (highlight-indent-guides--get-guides))
            (setq chunk (cons (list (point) guides oldguides) chunk)))
          (setq eof (< 0 (forward-line)))
          ;; expand sections if necessary
          (when (or eof (and (>= (point) endl)
                             (not (eq oldguides t))
                             (equal (car guides) (car oldguides))))
            (let ((lep (line-end-position 0)))
              (dolist (guide (cdr guides))
                (when (and (cdr guide) (> lep (cdr guide)))
                  (set-marker (cdr guide) lep)))))
          ;; ensure chunk is flush with surrounding sections
          (when (and (>= (point) endl)
                     (not (eq oldguides t))
                     (equal (car guides) (car oldguides))
                     (not (eq (cdr guides) (cdr oldguides))))
            (setq guides (cons (car guides) (cdr guides)))
            (let ((ng (cdr guides)) (og (cdr oldguides)) (badguide t)
                  abovestart aboveend belowstart belowend above below)
              (while (and og ng (nlistp badguide))
                (when (eq (cdr og) (cdr ng)) (setq badguide (cons og ng)))
                (setq ng (cdr ng) og (cdr og)))
              (setq abovestart (caar (cdr badguide)) aboveend startl)
              (setq belowstart (point) belowend (cdar (car badguide)))
              (setq above (- aboveend abovestart) below (- belowend belowstart))
              (if (>= (- belowstart abovestart) below) (setq endl belowend)
                (if (>= 0 above)
                    (let ((ng (cdr guides)) (og (cdr oldguides)))
                      ;; transform existing lines in chunk to use new sections
                      (while (and og ng)
                        (set-marker (caar og) (caar ng))
                        (setq ng (cdr ng) og (cdr og)))
                      (dolist (line chunk)
                        (when (cdr line)
                          (setcdr (nth 1 line)
                                  (highlight-indent-guides--replace-section
                                   (cdr (nth 1 line))
                                   (cdr guides) (cdr oldguides))))))
                  (goto-char abovestart)
                  (setq guides (highlight-indent-guides--get-prev-guides))
                  (setq eof (< 0 (forward-line)))
                  (setq startl (point) oldguides nil chunk nil))))))
        ;; rewrite text properties for all lines in chunk
        (dolist (line chunk)
          (goto-char (car line))
          (if (cdr line)
              (setq lf (save-excursion (back-to-indentation) (point)))
            (setq lf (car line)))
          (setq le (line-end-position))
          (unless (and (null (get-text-property lf prop))
                       (eq le (next-single-property-change lf prop nil le)))
            (remove-text-properties lf le (list prop nil)))
          (when (or (eq t (nth 2 line))
                    (not (equal (car (nth 1 line)) (car (nth 2 line))))
                    (not (eq (cdr (nth 1 line)) (cdr (nth 2 line)))))
            (highlight-indent-guides--guide-line (nth 1 line))))
        ;; update the line cache if necessary
        (when (car highlight-indent-guides--line-cache)
          (goto-char (car highlight-indent-guides--line-cache))
          (setq oldsect (nth 2 highlight-indent-guides--line-cache))
          (setq newsect (highlight-indent-guides--update-line-cache))
          (setcar (cddr highlight-indent-guides--line-cache) newsect))
        ;; refontify updated regions
        (if (equal oldsect newsect)
            (font-lock-fontify-region startl endl)
          (setq rng (highlight-indent-guides--discover-ranges oldsect newsect))
          (dolist (range (highlight-indent-guides--try-merge-ranges
                          (cons startl endl) (car rng) (cadr rng)))
            (font-lock-fontify-region (car range) (cdr range))))))))

(defun highlight-indent-guides--unguide-region (start end)
  "Remove all indent guides in the buffer region from START to END."
  (with-silent-modifications
    (remove-text-properties start end '(highlight-indent-guides-prop nil))))

(defun highlight-indent-guides--fill-keyword-matcher (limit)
  "Search for indent guides between the point and LIMIT.
Find the next character that is part of any indentation.  This is meant to be
used as a `font-lock-keywords' matcher."
  (let* ((pos (point))
         (prop 'highlight-indent-guides-prop)
         (face (car (get-text-property pos prop))))
    (while (and (not (natnump face)) (< pos limit))
      (setq pos (next-single-property-change pos prop nil limit))
      (setq face (car (get-text-property pos prop))))
    (when (< pos limit)
      (set-match-data (list (copy-marker pos) (copy-marker (1+ pos))))
      (goto-char (1+ pos)))))

(defun highlight-indent-guides--column-keyword-matcher (limit)
  "Search for indent guides between the point and LIMIT.
Find the next character that contains the first column of an indentation level.
This is meant to be used as a `font-lock-keywords' matcher."
  (let* ((pos (point))
         (prop 'highlight-indent-guides-prop)
         (propval (get-text-property pos prop)))
    (while (and (not (and (natnump (car propval))
                          (or (nth 2 propval) (nth 1 propval)))) (< pos limit))
      (setq pos (1+ pos))
      (setq propval (get-text-property pos prop))
      (while (and (< pos limit) (not (natnump (car propval))))
        (setq pos (next-single-property-change pos prop nil limit))
        (setq propval (get-text-property pos prop))))
    (when (< pos limit)
      (set-match-data (list (copy-marker pos) (copy-marker (1+ pos))))
      (goto-char (1+ pos)))))

(defun highlight-indent-guides--highlighter-default (level responsive display)
  "Determine the correct face to use for a given indentation level.
Uses the LEVEL, RESPONSIVE context, and DISPLAY method to decide on a correct
face for any given indentation.  This is the default implementation of
`highlight-indent-guides-highlighter-function'."
  (cond ((null responsive)
         (cond ((eq display 'character)
                'highlight-indent-guides-character-face)
               ((zerop (mod level 2))
                'highlight-indent-guides-even-face)
               (t 'highlight-indent-guides-odd-face)))
        ((eq responsive 'top)
         (cond ((eq display 'character)
                'highlight-indent-guides-top-character-face)
               ((zerop (mod level 2))
                'highlight-indent-guides-top-even-face)
               (t 'highlight-indent-guides-top-odd-face)))
        ((eq responsive 'stack)
         (cond ((eq display 'character)
                'highlight-indent-guides-stack-character-face)
               ((zerop (mod level 2))
                'highlight-indent-guides-stack-even-face)
               (t 'highlight-indent-guides-stack-odd-face)))
        (t nil)))

(defmacro highlight-indent-guides--cache-highlight (type prop hlkey &rest body)
  "Memoize the highlighter results in the character's properties.
If a cached result with the right TYPE (`fill', `column', or `character') is
contained in PROP with a responsive context matching HLKEY, return that result
instead of calculating a new one.  Otherwise, calculate a new result by running
BODY, cache it in PROP, and return it."
  `(let ((cache (nth 4 ,prop)) plist result)
     (if (and (eq ,type (car cache))
              (setq result (lax-plist-get (cdr cache) ,hlkey)))
         result
       (setq result (progn ,@body))
       (setq plist (lax-plist-put (cdr cache) ,hlkey result))
       (setcar (nthcdr 4 ,prop) (cons ,type plist))
       result)))

(defun highlight-indent-guides--should-highlight (prop)
  "Determine how a guide should be highlighted in responsive mode.
The guide's data is given as PROP."
  (if (null highlight-indent-guides-responsive) nil
    (let ((currseg (nth 5 prop))
          (segct (max 1 (+ (length (nth 1 prop)) (if (nth 2 prop) 1 0))))
          (cacheseg (nth 2 highlight-indent-guides--line-cache))
          (isstack (eq highlight-indent-guides-responsive 'stack))
          result)
      (dotimes (segnum segct result)
        (cond ((equal cacheseg currseg)
               (setq result (cons 'top result)))
              ((and isstack (highlight-indent-guides--iscdr currseg cacheseg))
               (setq result (cons 'stack result)))
              (t (setq result (cons nil result))))
        (setq currseg (cdr currseg))))))

(defun highlight-indent-guides--fill-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the matched character.  Highlights all
indentation characters in alternating colors.  This is meant to be used as a
`font-lock-keywords' face definition."
  (let* ((prop (get-text-property (match-beginning 0) 'highlight-indent-guides-prop))
         (shouldhl (highlight-indent-guides--should-highlight prop)))
    (highlight-indent-guides--cache-highlight
     'fill prop shouldhl
     (let ((highlighter highlight-indent-guides-highlighter-function)
           (facep (car prop)) (segs (nth 1 prop)) (cwidth (nth 3 prop))
           (pseg 0) face showstr)
       (if (null segs) (funcall highlighter facep (car shouldhl) 'fill)
         (setq showstr (make-string cwidth ?\s))
         (dolist (seg segs)
           (setq face (funcall highlighter facep (pop shouldhl) 'fill))
           (when face (add-text-properties pseg seg `(face ,face) showstr))
           (setq pseg seg)
           (setq facep (1+ facep)))
         (setq face (funcall highlighter facep (pop shouldhl) 'fill))
         (when face (add-text-properties pseg cwidth `(face ,face) showstr))
         `(face nil display ,showstr))))))

(defun highlight-indent-guides--column-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the matched character.  Highlights the
first column of each indentation level in alternating colors.  This is meant to
be used as a `font-lock-keywords' face definition."
  (let* ((prop (get-text-property (match-beginning 0) 'highlight-indent-guides-prop))
         (shouldhl (highlight-indent-guides--should-highlight prop)))
    (highlight-indent-guides--cache-highlight
     'column prop shouldhl
     (let ((highlighter highlight-indent-guides-highlighter-function)
           (facep (car prop)) (segs (nth 1 prop))
           (starter (nth 2 prop)) (cwidth (nth 3 prop))
           face showstr)
       (if (and (null segs) (eq cwidth 1))
           (funcall highlighter facep (car shouldhl) 'column)
         (setq showstr (make-string cwidth ?\s))
         (when starter
           (setq face (funcall highlighter facep (pop shouldhl) 'column))
           (when face (add-text-properties 0 1 `(face ,face) showstr)))
         (dolist (seg segs)
           (setq face (funcall highlighter facep (pop shouldhl) 'column))
           (when face (add-text-properties seg (1+ seg) `(face ,face) showstr))
           (setq facep (1+ facep)))
         `(face nil display ,showstr))))))

(defun highlight-indent-guides--character-highlighter ()
  "Apply highlighting to the indentation.
Return highlighting information for the matched character.  Displays a character
in place of the first column of each indentation level.  This is meant to be
used as a `font-lock-keywords' face definition."
  (let* ((prop (get-text-property (match-beginning 0) 'highlight-indent-guides-prop))
         (shouldhl (highlight-indent-guides--should-highlight prop)))
    (highlight-indent-guides--cache-highlight
     'character prop shouldhl
     (let ((highlighter highlight-indent-guides-highlighter-function)
           (facep (car prop)) (segs (nth 1 prop))
           (starter (nth 2 prop)) (cwidth (nth 3 prop))
           face showstr)
       (if (and (null segs) (eq cwidth 1))
           (progn
             (setq face (funcall highlighter facep (car shouldhl) 'character))
             (when face
               (setq showstr
                     (char-to-string highlight-indent-guides-character)))
             `(face ,face display ,showstr))
         (setq showstr (make-string cwidth ?\s))
         (when starter
           (setq face (funcall highlighter facep (pop shouldhl) 'character))
           (when face
             (aset showstr 0 highlight-indent-guides-character)
             (add-text-properties 0 1 `(face ,face) showstr)))
         (dolist (seg segs)
           (setq face (funcall highlighter facep (pop shouldhl) 'character))
           (when face
             (aset showstr seg highlight-indent-guides-character)
             (add-text-properties seg (1+ seg) `(face ,face) showstr))
           (setq facep (1+ facep)))
         `(face nil display ,showstr))))))

(defun highlight-indent-guides--overdraw (start end)
  "Overdraw the guides in the region from START to END.
This function is like `font-lock-fontify-region' or `font-lock-ensure', except
it only draws indent guides.  This function is called to update the display
whenever the active indent level changes, as long as responsive guides are
enabled.  This function is used because it avoids doing extra work like clearing
existing fontification, redrawing syntax and other keywords, or calling jit-lock
recursively."
  (with-silent-modifications
    (save-excursion
      (save-restriction
        (let ((matcher
               (pcase highlight-indent-guides-method
                 (`fill 'highlight-indent-guides--fill-keyword-matcher)
                 (`column 'highlight-indent-guides--column-keyword-matcher)
                 (`character 'highlight-indent-guides--column-keyword-matcher)))
              (highlight
               (pcase highlight-indent-guides-method
                 (`fill 'highlight-indent-guides--fill-highlighter)
                 (`column 'highlight-indent-guides--column-highlighter)
                 (`character 'highlight-indent-guides--character-highlighter)))
              (inhibit-point-motion-hooks t))
          (unless font-lock-dont-widen (widen))
          (goto-char start)
          (while (and (< (point) end) (funcall matcher end))
            (unless (> (point) (match-beginning 0)) (forward-char 1))
            (font-lock-apply-highlight (list 0 (list highlight) t))))))))

;;;###autoload
(defun highlight-indent-guides-auto-set-faces ()
  "Automatically calculate indent guide faces.
If this feature is enabled, calculate reasonable values for the indent guide
colors based on the current theme's colorscheme, and set them appropriately.
This runs whenever a theme is loaded, but it can also be run interactively."
  (interactive)
  (when highlight-indent-guides-auto-enabled
    (let* ((bk (face-background 'default nil 'default))
           (fg (color-name-to-rgb (face-foreground 'default nil 'default)))
           (bg (color-name-to-rgb bk))
           (oddf 'highlight-indent-guides-odd-face)
           (evenf 'highlight-indent-guides-even-face)
           (charf 'highlight-indent-guides-character-face)
           (toddf 'highlight-indent-guides-top-odd-face)
           (tevenf 'highlight-indent-guides-top-even-face)
           (tcharf 'highlight-indent-guides-top-character-face)
           (soddf 'highlight-indent-guides-stack-odd-face)
           (sevenf 'highlight-indent-guides-stack-even-face)
           (scharf 'highlight-indent-guides-stack-character-face)
           (oddp highlight-indent-guides-auto-odd-face-perc)
           (evenp highlight-indent-guides-auto-even-face-perc)
           (charp highlight-indent-guides-auto-character-face-perc)
           (toddp highlight-indent-guides-auto-top-odd-face-perc)
           (tevenp highlight-indent-guides-auto-top-even-face-perc)
           (tcharp highlight-indent-guides-auto-top-character-face-perc)
           (soddp highlight-indent-guides-auto-stack-odd-face-perc)
           (sevenp highlight-indent-guides-auto-stack-even-face-perc)
           (scharp highlight-indent-guides-auto-stack-character-face-perc)
           mod fl bl)
      (if (not (and fg bg))
          (message "Error: %s: %s"
                   "highlight-indent-guides cannot auto set faces"
                   "`default' face is not set properly")
        (setq fl (nth 2 (apply 'color-rgb-to-hsl fg)))
        (setq bl (nth 2 (apply 'color-rgb-to-hsl bg)))
        (setq mod (cond ((< fl bl) -1) ((> fl bl) 1) ((< 0.5 bl) -1) (t 1)))
        (set-face-background oddf (color-lighten-name bk (* mod oddp)))
        (set-face-background evenf (color-lighten-name bk (* mod evenp)))
        (set-face-foreground charf (color-lighten-name bk (* mod charp)))
        (set-face-background toddf (color-lighten-name bk (* mod toddp)))
        (set-face-background tevenf (color-lighten-name bk (* mod tevenp)))
        (set-face-foreground tcharf (color-lighten-name bk (* mod tcharp)))
        (set-face-background soddf (color-lighten-name bk (* mod soddp)))
        (set-face-background sevenf (color-lighten-name bk (* mod sevenp)))
        (set-face-foreground scharf (color-lighten-name bk (* mod scharp)))))))

(defadvice load-theme (after highlight-indent-guides-auto-set-faces disable)
  "Automatically calculate indent guide faces.
If this feature is enabled, calculate reasonable values for the indent guide
colors based on the current theme's colorscheme, and set them appropriately.
This runs whenever a theme is loaded."
  (highlight-indent-guides-auto-set-faces))

(defun highlight-indent-guides--auto-set-faces-with-frame (frame)
  "Run `highlight-indent-guides-auto-set-faces' in frame FRAME.
This function is designed to run from the `after-make-frame-functions' hook."
  (with-selected-frame frame
    (highlight-indent-guides-auto-set-faces)))

(make-variable-buffer-local 'font-lock-extra-managed-props)
(make-variable-buffer-local 'text-property-default-nonsticky)

;;;###autoload
(define-minor-mode highlight-indent-guides-mode
  "Display indent guides in a buffer."
  nil " h-i-g" nil
  (let ((fill-method-keywords
         '((highlight-indent-guides--fill-keyword-matcher
            0 (highlight-indent-guides--fill-highlighter) t)))
        (column-method-keywords
         '((highlight-indent-guides--column-keyword-matcher
            0 (highlight-indent-guides--column-highlighter) t)))
        (character-method-keywords
         '((highlight-indent-guides--column-keyword-matcher
            0 (highlight-indent-guides--character-highlighter) t))))
    (when highlight-indent-guides--idle-timer
      (cancel-timer highlight-indent-guides--idle-timer)
      (setq highlight-indent-guides--idle-timer nil))
    (if highlight-indent-guides-mode
        (progn
          ;; set highlight-indent-guides--line-cache so it becomes buffer-local
          ;; After this, we can destructively modify it just fine, as every
          ;; buffer has a unique object.
          (setq highlight-indent-guides--line-cache (list nil nil nil))
          (unless (daemonp) (highlight-indent-guides-auto-set-faces))
          (add-to-list 'after-make-frame-functions
                       'highlight-indent-guides--auto-set-faces-with-frame)
          (ad-enable-advice 'load-theme 'after
                            'highlight-indent-guides-auto-set-faces)
          (ad-activate 'load-theme)
          (add-to-list 'font-lock-extra-managed-props 'display)
          (add-to-list 'text-property-default-nonsticky
                       (cons 'highlight-indent-guides-prop t))
          (setq highlight-indent-guides--idle-timer
                (run-with-idle-timer
                 highlight-indent-guides-delay t
                 'highlight-indent-guides--try-update-line-cache))
          (font-lock-add-keywords
           nil
           (pcase highlight-indent-guides-method
             (`fill fill-method-keywords)
             (`column column-method-keywords)
             (`character character-method-keywords))
           t)
          (jit-lock-register 'highlight-indent-guides--guide-region))
      (setq after-make-frame-functions
            (delete 'highlight-indent-guides--auto-set-faces-with-frame
                    after-make-frame-functions))
      (ad-disable-advice 'load-theme 'after
                         'highlight-indent-guides-auto-set-faces)
      (ad-activate 'load-theme)
      (font-lock-remove-keywords nil fill-method-keywords)
      (font-lock-remove-keywords nil column-method-keywords)
      (font-lock-remove-keywords nil character-method-keywords)
      (jit-lock-unregister 'highlight-indent-guides--guide-region)
      (highlight-indent-guides--unguide-region (point-min) (point-max))
      (if (fboundp 'font-lock-flush) (font-lock-flush)
        (font-lock-fontify-buffer)))))

(provide 'highlight-indent-guides)

;;; highlight-indent-guides.el ends here
