(require 'anything-vim-hacks)

;; setup
(load-file "./fixture.el")

;; batch 処理の時に、message 関数が端末に出力されるのがいやだったので wrap した
(when noninteractive
  (defvar org-redmine-expectation-message nil)

  (defadvice message (around message-to-variable activate)
    (setq org-redmine-expectation-message (apply 'format format-string args)))

  (defadvice current-message (around get-message-from-variable activate)
    (setq ad-return-value org-redmine-expectation-message))
)

(defun vim-hacks-test:fixture-get (html)
  (save-current-buffer
    (set-buffer (get-buffer-create vim-hacks:buffer-name))
    (erase-buffer)
    (insert html)))

(expectations
  (desc "vim-hacks:view-pre-code")
  (expect "\

    12345
    hogehoge
    
    hago!
"
    (let ((code (xml-get-children vim-hacks-fixture:precode 'code)))
      (vim-hacks:view-pre-code (car code) 0)))

  (desc "vim-hacks:view-pre-code with arg")
  (expect "\

        12345
        hogehoge
        
        hago!
"
    (let ((code (xml-get-children vim-hacks-fixture:precode 'code)))
      (vim-hacks:view-pre-code (car code) 1)))

  (desc "vim-hacks:view-img")
  (expect "![alt-text](hoge.png)"
    (vim-hacks:view-img vim-hacks-fixture:img))

  (desc "vim-hacks:view-string")
  (expect "hogefuga"
    (vim-hacks:view-string "   hogefuga 	"))

  (desc "vim-hacks:view-a")
  (expect "[example.com](http://example.com)"
    (vim-hacks:view-a vim-hacks-fixture:a))

  (desc "vim-hacks:view-h2")
  (expect "
## h2
"
    (vim-hacks:view-h2 vim-hacks-fixture:h2))

  (desc "vim-hacks:view-h3")
  (expect "
### h3
"
    (vim-hacks:view-h3 vim-hacks-fixture:h3))

  (desc "vim-hacks:view-strong")
  (expect "*strong*"
    (vim-hacks:view-strong vim-hacks-fixture:strong))

  (desc "vim-hacks:view-italic")
  (expect "_italic_"
    (vim-hacks:view-italic vim-hacks-fixture:italic))

  (desc "vim-hacks:parse-hack p")
  (expect "\

aiueo
"
    (mock (vim-hacks:http-get *)
          => (vim-hacks-test:fixture-get vim-hacks-fixture:html-p))
    (vim-hacks:view ""))

  (desc "vim-hacks:parse-hack p+precode")
  (expect "\

gongo

    def gongo
      'du!'
    end
"
    (mock (vim-hacks:http-get *) => (vim-hacks-test:fixture-get vim-hacks-fixture:html-p+precode))
    (vim-hacks:view ""))

  (desc "vim-hacks:parse-hack p+precode+p")
  (expect "\

gongo

    def gongo
      'du!'
    end

dududu!
"
    (mock (vim-hacks:http-get *)
          => (vim-hacks-test:fixture-get vim-hacks-fixture:html-p+precode+p))
    (vim-hacks:view ""))

  (desc "vim-hacks:parse-hack h2+precode+h3+p")
  (expect "\

## h2

    def gongo
      'du!'
    end

### h3

ppppp
"
    (mock (vim-hacks:http-get *)
          => (vim-hacks-test:fixture-get vim-hacks-fixture:html-h2+precode+h3+p))
    (vim-hacks:view ""))


  )
