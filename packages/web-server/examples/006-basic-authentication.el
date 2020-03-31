;;; basic-authentication.el --- basic authentication  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2020  Free Software Foundation, Inc.

(let ((users '(("foo" . "bar")
               ("baz" . "qux"))))
  (ws-start
   (ws-with-authentication
    (lambda (request)
      (with-slots (process headers) request
        (let ((user (caddr (assoc :AUTHORIZATION headers))))
          (ws-response-header process 200 '("Content-type" . "text/plain"))
          (process-send-string process (format "welcome %s" user)))))
    users)
   9006))
