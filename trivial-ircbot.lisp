;;; import ;;;
(defpackage :trivial-ircbot
  (:import-from :cl-ppcre :scan)
  (:import-from :split-sequence :split-sequence)
  (:use :common-lisp :irc)
  (:export :new-bot :add-command :del-command :add-help-command))

(in-package :trivial-ircbot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for s being the external-symbols of :irc do (export s)))

;;; commands ;;;

(defclass bot (connection)
  ((nickname
    :initarg :nickname)
   (commands
    :initarg :commands
    :initform (make-hash-table :test 'equal))
   (asynchronous-p
    :initarg :asynchronous-p
    :initform nil)
   (fallback-command
    :initarg :fallback-command
    :initform nil)
   (interval
    :initarg :interval
    :initform 0.7)
   (max-line
    :initarg :max-line
    :initform 5)
   (max-column
    :initarg :max-column
    :initform 300)
   (attention-prefix
    :initarg :attention-prefix)))

(defgeneric add-command (bot command func))
(defgeneric get-command (bot command))
(defgeneric del-command (bot command))

(defmethod add-command ((bot bot) (command string) func)
  (setf (gethash command (slot-value bot 'commands)) func))

(defmethod get-command ((bot bot) (command string))
  (or (gethash command (slot-value bot 'commands)) (slot-value bot 'fallback-command)))

(defmethod del-command ((bot bot) (command string))
  (remhash command (slot-value bot 'commands)))

(defun add-help-command (bot &key protected-command-list (help-message-format "commands: ~{~a~^ ~}"))
  (let ((help-message (loop
          for command being the hash-keys of (slot-value bot 'commands)
          unless (member command protected-command-list :test #'equal)
          collect command into command-list
          finally (return (format nil help-message-format command-list)))))
    (add-command bot "help" (lambda (message args) help-message))))

;;; hooks ;;;

(defun parse-message (message)
  "channel == nil means privmsg"
  (apply #'values (or
    (let ((bot (connection message)))
      (destructuring-bind (target text) (arguments message)
        (let* ((priv-p (string= target (slot-value bot 'nickname)))
               (start-pos (if priv-p
                 0
                 (nth-value 1 (scan (slot-value bot 'attention-prefix) text)))))
          (when start-pos
            (multiple-value-bind
                (command-list space-index)
                (split-sequence #\Space text :start start-pos :remove-empty-subseqs t :count 1)
              (let* ((command (car command-list))
                     (command-func-or-symbol (get-command bot command)))
                  (when command-func-or-symbol
                    (let ((command-func (if (symbolp command-func-or-symbol)
                            (symbol-function command-func-or-symbol)
                            command-func-or-symbol))
                          (nick (source message))
                          (args (subseq text space-index)))
                      `(,priv-p ,(if priv-p nick target) ,nick ,command-func ,args)))))))))
    '(nil nil nil nil))))

(defun message-hook (message)
  (let ((bot (connection message)))
    (multiple-value-bind (priv-p channel nick command-func args) (parse-message message)
      (when command-func
        ; (format (client-stream bot) "~s~%" (list priv-p channel nick command-func args))
        (client-log bot message "HANDLING-EVENT:")
        (if (slot-value bot 'asynchronous-p)
          (funcall *thread-func*
            #'(lambda () (send-message bot message priv-p channel nick command-func args)))
          (send-message bot message priv-p channel nick command-func args))
        t))))

(defun send-message (bot message priv-p channel nick command-func args)
  (let ((reply (handler-case (funcall command-func message args) (error (ex) ex))))
    (if reply
      (let* ((reply-list (split-sequence
               #\Newline (princ-to-string reply) :remove-empty-subseqs t))
             (reply-list (if (> (length reply-list) (slot-value bot 'max-line))
               (concatenate 'list (subseq reply-list 0 (slot-value bot 'max-line)) '("..."))
               reply-list)))
        (loop
          for i on reply-list
          do (progn
            (privmsg bot channel (if priv-p (car i) (format nil "~a: ~a" nick (car i))))
            (if (cdr i)
              (sleep (slot-value bot 'interval)))))))))

(defun ignore-hook (message)
  t)

;;; init ;;;

(defconstant +thread-package-name+ "BORDEAUX-THREADS")
(defconstant +thread-func-name+ "MAKE-THREAD")

(defvar *thread-func* nil)

(defun make-attention-prefix (nick prefix)
  (format nil "^(?:~a|~a[ ,:]\\s+)" prefix nick))

(defun new-bot (&rest connect-options &key nickname channels (prefix #\`) asynchronous-p &allow-other-keys)
  (if (and asynchronous-p (not *thread-func*))
    (let ((thread-package (find-package +thread-package-name+)))
     (if thread-package
       (let ((thread-func (find-symbol +thread-func-name+ thread-package)))
         (if thread-func
           (setf *thread-func* thread-func)
           (error "Symbol ~A in package ~A isn't fbound"
             +thread-function-name+ +thread-func-name+)))
       (error "Can't resolve symbol ~A in package ~A"
         +thread-function-name+ +thread-func-name+))))
  (let ((bot (change-class
               (apply #'connect (cons :allow-other-keys (cons t connect-options))) 'bot
               :attention-prefix (make-attention-prefix nickname prefix) :nickname nickname
               :asynchronous-p asynchronous-p)))
    (mapcar #'(lambda (channel) (join bot channel)) channels)
    (add-hook bot 'irc-privmsg-message 'message-hook)
    (dolist (motd-event '(irc-rpl_motdstart-message
                          irc-rpl_motd-message
                          irc-rpl_endofmotd-message))
      (add-hook bot motd-event 'ignore-hook))
    bot))
