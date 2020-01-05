(in-package :asdf-user)
(defsystem #:truetype-clx
  :serial t
  :description "Ripped out rendering from clx-truetype"
  ;original author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  ;The Butcher "Johannes Martinez Calzada"
  :license "MIT"
  :version "0.1"
  :depends-on (#:zpb-ttf
               #:cl-vectors
               #:cl-paths-ttf
               #:cl-aa)
  :components ((:file "truetype-clx")))
