;;;; nbt.asd

(asdf:defsystem #:nbt
  :description "Minecraft NBT file format reader and writer"
  :author "MrJCraft"
  :license  "Copyright (C) 2023 MRJ"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:chipz
               #:babel)
  :pathname "src"
  :components ((:file "package")
               (:file "l")
               (:file "nbt")))
