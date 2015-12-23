(defsystem trivial-ircbot
  :name "trivial-ircbot"
  :author "Yangfl"
  :description "A simple irc bot framework."
  :license "public domain"
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:cl-irc :cl-ppcre :split-sequence)
  :components ((:file "trivial-ircbot")))
