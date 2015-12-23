(in-package :trivial-ircbot)

(setf my-bot (new-bot :nickname "testbot" :server "chat.someserver.org" :channels '("##testchannel")))

(add-command my-bot "ping" #'(lambda () "pong"))

(read-message-loop my-bot)

; Now you can use:
; testbot: ping
; testbot, ping
; `ping
