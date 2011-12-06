(require 'xml)

(defun vim-hacks-fixture:surround-html (src)
  (concat "<div class=\"textBody\">"
          src
          "<address class=\"hack-author\">gongo</address>"))

(setq vim-hacks-fixture:html-p
      (vim-hacks-fixture:surround-html "<p>aiueo</p>"))

(setq vim-hacks-fixture:html-p+precode
      (vim-hacks-fixture:surround-html "\
<p>gongo</p>

<pre><code>
def gongo
  'du!'
end
</code></pre>

"))

(setq vim-hacks-fixture:html-p+precode+p
      (vim-hacks-fixture:surround-html "\
<p>gongo</p>

<pre><code>
def gongo
  'du!'
end
</code></pre>



<p>dududu!</p>
"))

(setq vim-hacks-fixture:html-h2+precode+h3+p
      (vim-hacks-fixture:surround-html "\
<h2>h2</h2>

<pre><code>
def gongo
  'du!'
end
</code></pre>

<h3>h3</h3>

<p>ppppp</p>
"))

(setq vim-hacks-fixture:p
      (with-temp-buffer
        (insert "<p>aiueo</p>")
        (xml-parse-region (point-min) (point-max))))

(setq vim-hacks-fixture:precode
      (with-temp-buffer
        (insert "\
<pre><code>
12345
hogehoge

hago!
</code></pre>")
        (car (xml-parse-region (point-min) (point-max)))))

(setq vim-hacks-fixture:img
      (with-temp-buffer
        (insert "<img alt=\"alt-text\" src=\"hoge.png\" />")
        (car (xml-parse-region (point-min) (point-max)))))

(setq vim-hacks-fixture:a
      (with-temp-buffer
        (insert "<a href=\"http://example.com\">example.com</a>")
        (car (xml-parse-region (point-min) (point-max)))))

(setq vim-hacks-fixture:h2
      (with-temp-buffer
        (insert "<h2>h2</h2>")
        (car (xml-parse-region (point-min) (point-max)))))

(setq vim-hacks-fixture:h3
      (with-temp-buffer
        (insert "<h3>h3</h3>")
        (car (xml-parse-region (point-min) (point-max)))))

(setq vim-hacks-fixture:strong
      (with-temp-buffer
        (insert "<b>strong</b>")
        (car (xml-parse-region (point-min) (point-max)))))


(setq vim-hacks-fixture:italic
      (with-temp-buffer
        (insert "<i>italic</i>")
        (car (xml-parse-region (point-min) (point-max)))))
