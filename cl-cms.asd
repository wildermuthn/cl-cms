;;;; cl-cms.asd

(asdf:defsystem #:cl-cms
  :serial t
  :description "A simple rest server to post and get json data"
  :author "Nate Wildermuth <nate@501creative.com>"
  :license "MIT"
  :depends-on (:hunchentoot
               :cl-json
               :ironclad
               :babel
               :cl-store
               :local-time
               :cl-utilities)
  :components ((:file "package")
               (:file "cl-cms")))

