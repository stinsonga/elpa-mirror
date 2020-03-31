;;; hello-world-html.el --- html hello world server using Emacs Web Server  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2020  Free Software Foundation, Inc.

(ws-start
 (lambda (request)
   (with-slots (process headers) request
     (ws-response-header process 200 '("Content-type" . "text/html"))
     (process-send-string process "<html>
  <head>
    <title>Hello World</title>
  </head>
  <body>
    <b>hello world</b>
  </body>
</html>")))
 9002)
