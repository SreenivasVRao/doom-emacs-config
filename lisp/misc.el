;;; ~/.doom.d/lisp/misc.el -*- lexical-binding: t; -*-

;; Ivy Posframe has trouble with the size of the frame.
;;; Alternatively, disable ivy-rich-mode
;; replace the frame width multiplier with something slightly higher.
(defun my-ivy-posframe-get-size ()
  "The default functon used by `ivy-posframe-size-function'."
  (list
   :height ivy-posframe-height
   :width ivy-posframe-width
   :min-height (or ivy-posframe-min-height
                   (let ((height (+ ivy-height 1)))
                     (min height (or ivy-posframe-height height))))
   :min-width (or ivy-posframe-min-width
                  (let ((width (round (* (frame-width) 0.5))))
                    (min width (or ivy-posframe-width width))))))



;; Using C-r within swiper similar to how C-s works.
;; Modification of swiper-C-s function.
(defun swiper-C-r (&optional arg)
    "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
    (interactive "p")
    (if (string= ivy-text "")
        (ivy-previous-history-element 1)
      (ivy-previous-line arg)))


;; Google thing at point or highlighted region.
(defun search-google(start end)
  (interactive "r")
  (if (use-region-p)
      (browse-url
       (concat "https://www.google.com/search?q=" (buffer-substring start end)))
    (browse-url
     (concat "https://www.google.com/search?q=" (thing-at-point 'symbol)))))

;; Search internally for thing at point or highlighted region.
(defun search-amz-internal(start end)
  (interactive "r")
  (if (use-region-p)
      (browse-url
       (concat "https://is.amazon.com/search/?q=" (buffer-substring start end)))
    (browse-url
     (concat "https://is.amazon.com/search?q=" (thing-at-point 'symbol)))))


;; fix https://github.com/DarthFennec/highlight-indent-guides/issues/82
(defadvice insert-for-yank (before my-clear-indent-guides activate)
  (remove-text-properties
   0 (length (ad-get-arg 0))
   '(display highlight-indent-guides-prop) (ad-get-arg 0)))


;; vterm support M-y
;; (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
;;   (if (equal major-mode 'vterm-mode)
;;       (let ((inhibit-read-only t)
;;             (yank-undo-function (lambda (_start _end) (vterm-undo))))
;;         (cl-letf (((symbol-function 'insert-for-yank)
;;                (lambda (str) (vterm-send-string str t))))
;;             (apply orig-fun args)))
;;     (apply orig-fun args)))

;; (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
