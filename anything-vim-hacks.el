(eval-when-compile (require 'cl))

;;------------------------------
;; Define constant variable
;;------------------------------
(defconst vim-hacks:buffer-name "*vim-hacks*"
  "Buffer name used to `vim-hacks:http-get'")

(defconst vim-hacks:url "http://vim-users.jp/vim-hacks-project/"
  "URL of Vim Hacks Project")

(defconst vim-hacks:list-reverse? nil
  "If not nil, display list in reverse order.")

(defvar vim-hacks:cache '()
  "Cache which stores vim hacks list obtained by `vim-hacks:get-hacks'")

;;------------------------------
;; Define exception error
;;------------------------------
(put 'vim-hacks:exception-not-retrieved 'error-message
     "Vim Hacks - Not retrieved")
(put 'vim-hacks:exception-not-retrieved 'error-conditions
     '(vim-hacks:exception-not-retrieved error))

;;------------------------------
;; View
;;------------------------------
(defun vim-hacks:view-a (node)
  (concat "[" (caddr node) "]" "(" (xml-get-attribute node 'href) ")"))

(defun vim-hacks:view-h2 (node)
  (concat "\n## " (caddr node) "\n"))

(defun vim-hacks:view-img (node)
  (concat "![" (xml-get-attribute node 'alt) "](" (xml-get-attribute node 'src) ")"))

(defun vim-hacks:view-pre-code (node)
  (with-temp-buffer
    (erase-buffer)
    (insert (caddr node))
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match "    " nil nil))
    (goto-char (point-max))
    (buffer-string)))

(defun vim-hacks:view-code (node)
  (concat "`" (caddr node) "`"))

(defun vim-hacks:view-tree-scan (root)
  (mapconcat
   (lambda (x)
     (cond ((stringp x) x)
           ((listp x)
            (let ((node-name (xml-node-name x)))
              (cond ((eq node-name 'p)
                     (concat "\n" (vim-hacks:view-tree-scan (cddr x)) "\n\n"))
                    ((eq node-name 'a)
                     (vim-hacks:view-a x))
                    ((eq node-name 'img)
                     (vim-hacks:view-img x))
                    ((eq node-name 'h2)
                     (vim-hacks:view-h2 x))
                    ((eq node-name 'pre)
                     (let ((code (xml-get-children x 'code)))
                       (if code
                           (vim-hacks:view-pre-code (car code)))))
                    ((eq node-name 'code)
                     (vim-hacks:view-code x))
                    )))))
   root ""))

;;------------------------------
;; System Function
;;------------------------------
(defun vim-hacks:http-get (url &optional buffer)
  (if (null buffer) (setq buffer vim-hacks:buffer-name))
  (ignore-errors (kill-buffer buffer))
  (unless (eq 0 (call-process "curl" nil `(,buffer nil) nil
                              "-f"
                              "-X" "GET"
                              url))
    (signal 'vim-hacks:exception-not-retrieved
            "The requested URL returned error")))

(defun vim-hacks:parse-http-get-response (&optional buffer)
  (cond ((stringp buffer) (setq buffer (get-buffer buffer)))
        ((null buffer) (setq buffer vim-hacks:buffer-name)))
  (save-current-buffer
    (set-buffer buffer)
    (goto-char (point-min))
    (let (begin end)
      (re-search-forward "<h2>Hacks[^<]+</h2>[.\t\n]+\\(<ul>\\)" nil t)
      (setq begin (match-beginning 1))
      (setq end (search-forward "</ul>"))
      (mapcar
       (lambda (x)
         (let ((atag (assq 'a x))
               (date (nth 2 x)))
           (cons (concat date (nth 2 atag))
                 (cdr (assq 'href (cadr atag))))))
       (xml-get-children (car (xml-parse-region begin end)) 'li)))))

(defun vim-hacks:parse-view-http-get-response (&optional buffer)
  (cond ((stringp buffer) (setq buffer (get-buffer buffer)))
        ((null buffer) (setq buffer vim-hacks:buffer-name)))
  (save-current-buffer
    (set-buffer buffer)
    (goto-char (point-min))
    (let (begin end)
      (search-forward "<div class=\"textBody\">" nil t)
      (setq begin (point))
      (insert "<div>")
      (re-search-forward "<address class=\"hack-author\">.+</address>" nil t)
      (insert "</div>")
      (setq end (point))
      (remove-if (lambda (x) (stringp x)) (cddar (xml-parse-region begin end))))))

(defun vim-hacks:get-hacks-from-http ()
  (vim-hacks:http-get vim-hacks:url)
  (vim-hacks:parse-http-get-response))

(defun vim-hacks:get-view-from-http (url)
  (vim-hacks:http-get url)
  (vim-hacks:parse-view-http-get-response))

(defun vim-hacks:get-hacks ()
  (if (= (safe-length vim-hacks:cache) 0)
      (setq vim-hacks:cache (vim-hacks:get-hacks-from-http)))
  (if vim-hacks:list-reverse? (reverse vim-hacks:cache)
    vim-hacks:cache))

(defun vim-hacks:view (url)
  (insert (vim-hacks:view-tree-scan (vim-hacks:get-view-from-http url))))

;;------------------------------
;; User Function
;;------------------------------
(defun vim-hacks:anything ()
  (interactive)
  (anything
   `((name . "Vim hacks")
     (candidates . vim-hacks:get-hacks)
     (action
      ("Open Browser" . (lambda (x) (browse-url x)))
      ("View Hack"    . (lambda (x) (vim-hacks:view x)))
      )
     )))


(defun vim-hacks:anything-refresh ()
  (interactive)
  (setq vim-hacks:cache '())
  (vim-hacks:anything))
