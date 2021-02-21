;;; ~/.doom.d/lisp/misc.el -*- lexical-binding: t; -*-

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
       (concat "https://sage.amazon.com/search/?q=" (buffer-substring start end)))
      (browse-url
       (concat "https://is.amazon.com/search/?q=" (buffer-substring start end)))
    (browse-url
     (concat "https://is.amazon.com/search?q=" (thing-at-point 'symbol)))))


;; fix https://github.com/DarthFennec/highlight-indent-guides/issues/82
(defadvice insert-for-yank (before my-clear-indent-guides activate)
  (remove-text-properties
   0 (length (ad-get-arg 0))
   '(display highlight-indent-guides-prop) (ad-get-arg 0)))


;; map package dir from local to remote
(defun sreeni-get-remote-dir (buffername)
  (let* ((myfilename (replace-regexp-in-string "/Volumes" "~" buffername))
         (myfilename (subseq (split-string myfilename "/") 0 5))
         (myfilename (mapconcat 'identity myfilename "/")))
    (message myfilename)))

;; exec commands on remote shell
(defun sreeni-vterm-exec-remote (buildtarget)
  (interactive)
  (let* ((remotedir (sreeni-get-remote-dir (buffer-file-name))))
    (+vterm/toggle (buffer-file-name))
    (vterm-send-string "ssh venkobas@dev-dsk-venkobas-1e-5be32ed4.us-east-1.amazon.com\n")
    (vterm-send-string (concat "cd " remotedir "\n"))
    (vterm-send-string (concat buildtarget "\n"))))


;;Compilation mode can use some hints for the typical brazil-build
(defun my/java-class-to-src (clsName)
  "Junit, at least, reports only the classname of the failing test.  This will optimistically change that to the source file to find it in."
  (format "%s.java" (replace-regexp-in-string "\\." "/" clsName)))
(defun my/compilation-junit-file-finder ()
  (format "tst/%s" (my/java-class-to-src (match-string-no-properties 2))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(amazon-brazil-junit-1
               "\\[junit\\] Testcase: \\(.*\\)(\\(.*\\)):.*\\(FAILED\\|ERROR\\)"
               my/compilation-junit-file-finder nil nil 2 nil (0 compilation-error-face))) ; file line column type hyperlink highlight


(defun my/java-class-to-src (clsName)
  "Junit, at least, reports only the classname of the failing test.  This will optimistically change that to the source file to find it in."
  (format "%s.java" (replace-regexp-in-string "\\." "/" clsName)))

(defun my/java-test-method-to-src (input)
  ;; convert com.amazon.package.filename.method to com/amazon/package/filename
  (mapconcat 'identity (butlast (split-string input "\\.")) "/"))

(defun my/compilation-junit-test-file-finder ()
  (let ((data (match-data)) ;; query match data
        (filepath (format "tst/%s.java" (my/java-test-method-to-src (match-string-no-properties 1)))))
    (set-match-data data) ;; restore match data
    (message filepath))) ;; returns the filepath

(defun my/compilation-junit-get-line()
  (let* ((data (match-data)) ;; query match data
         (targetline (match-string-no-properties 2)))
    (string-to-number targetline)))

(add-to-list 'compilation-error-regexp-alist-alist
             '(amazon-brazil-junit-2
               "\\[junit\\].*at \\(.*\\)\(.*:\\([0-9]+\\)\)"
               my/compilation-junit-test-file-finder
               my/compilation-junit-get-line nil nil nil (0 compilation-error-face)))

(add-to-list 'compilation-error-regexp-alist  'amazon-brazil-junit-1)
(add-to-list 'compilation-error-regexp-alist  'amazon-brazil-junit-2)

(setq counsel-remote-compile-history '())
(defun counsel-remote-compile (&optional dir)
  (interactive)
  (ivy-read "Run on Dev-Desk: "
            (remove-duplicates counsel-remote-compile-history)
            :action (lambda (x) (let* ((remotehost "/ssh:venkobas@dev-dsk-venkobas-1e-5be32ed4.us-east-1.amazon.com:")
                                       (remotepath (sreeni-get-remote-dir (buffer-file-name)))
                                       (default-directory (concat remotehost remotepath)))
                                  (counsel-compile--action x)))
            :keymap counsel-compile-map
            :history 'counsel-remote-compile-history
            :caller 'counsel-remote-compile))
