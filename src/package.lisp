;;;; package.lisp

(defpackage #:nbt
  (:use #:cl)
  (:export
   :nbt-read
   :nbt-write
   :attempt
   :lflatten 
   :partition-list
   :ln1
   :ln2
   :nbt-get-value
   :gunzip
   :get-tag
   :get-byte
   :nbt-string-to-bytes
   :nbt-byte-list-to-bytes
   :nbt-int-array-to-bytes
   :nbt-long-array-to-bytes
   :nl2
   :str-replace
   :nbt-change
   :nbt-eval-at
   :nbt-str-replace
   :nbt-get-pos
   :get-pos
           ))
