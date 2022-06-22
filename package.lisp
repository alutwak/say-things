;;;; package.lisp

(defpackage #:say-things
  (:use #:cl)
  (:import-from :uiop)
  (:export
   :say
   :say-clause
   :load-dicts
   :say-random-sentence))
