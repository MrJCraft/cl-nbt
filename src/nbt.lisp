;;;; nbt.lisp

(in-package #:nbt)

;; read file
;; sperate into

;; turn these into functions?
;; 0         x TAG_End     
;; 1         x TAG_Byte
;; 2         x TAG_Short
;; 3         x TAG_Int 
;; 4         x TAG_Long 
;; 5         x TAG_Float
;; 6         x TAG_Double
;; 7         x TAG_Byte_Array
;; 8         x TAG_String
;; 9         x TAG_List
;; 10         x TAG_Compound 
;; 11         TAG_Int_Array
;; 12         TAG_Long_Array

;; type compound 0a
;; name length and name 00 0b hello world
;; type string 08
;; length of name 00 04 name
;; length of string and string 9 Bananrama

(defun nbt-read (content)
  "Reads Minecrafts NBT file format, can return error for incorrect tag error pos includes ZERO, when successful returns a list of tag name value, except in situations where it is a list of values that are delimited by :TAG-END"
  (setq nbt content)
  (setq result '())
  (setq pos 0)
  (loop
    ;; Car of nbt should always be a new tag or nil, if not it is malformed or implemented wrong
    ;;(break "~a~%~a" nbt (reverse result))
   (cond
     ((equal (car nbt) nil)
      (progn
       (print "Finished")
       (return nil)))
     ((equal (car nbt) 0)
      (progn (push :tag-end result)
             (pop nbt)))
     ((equal (car nbt) 1)
      (progn (push :tag-byte result)
             (pop nbt)
             (push (nbt-get-value 2 'string) result)
             (push (car nbt) result)
             (pop nbt)
             ))
    ((equal (car nbt) 2)
     (progn (push :tag-short result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (ln2 (subseq nbt 0 2)) result)
            (setq nbt (subseq nbt 2))))
    ((equal (car nbt) 3)
     (progn (push :tag-int result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (ln2 (subseq nbt 0 4)) result)
            (setq nbt (subseq nbt 4))))
    ((equal (car nbt) 4)
     (progn (push :tag-long result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (ln2 (subseq nbt 0 8)) result)
            (setq nbt (subseq nbt 8))
            ))
    ((equal (car nbt) 5)
     (progn (push :tag-float result)
            (pop nbt)
            (error "~a is not implemented yet" (car result))
            ))
    ((equal (car nbt) 6)
     (progn (push :tag-double result)
            (pop nbt)))
    ((equal (car nbt) 7)
     (progn (push :tag-byte-array result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (nbt-get-value 4) result)))
    ((equal (car nbt) 8)
     (progn (push :tag-string result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (nbt-get-value 2 'string) result)
            ))
    ((equal (car nbt) 9)
     (progn (push :tag-list result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (get-tag (car nbt)) result)
            (pop nbt)
            (push (ln2 (subseq nbt 0 4)) result)
            (setq nbt (subseq nbt 4))
            ))
    ((equal (car nbt) 10)
     (progn (push :tag-compound result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)))
    ((equal (car nbt) 11)
     (progn (push :tag-int-array result)
            (pop nbt)
            (push (nbt-get-value 2 'string) result)
            (push (nbt-get-value 4 'int-list) result)
            ))
    ((equal (car nbt) 12)
     (progn (push :tag-long-array result)
            (pop nbt)
            (error "~a is not implemented yet" (car result))))
    (t (error "~a is an incorrect tag~% stopped at pos ~a ~% result = ~s"
           (car nbt)
           pos
           (reverse result))))
    (incf pos)
    ) 
  (reverse result))
  

(defun nbt-write (nbt filename)
  "writes the Minecrafts NBT file format, can return error for incorrect tag error, when successful prints the output to a file and compresses it."
  (setq result '())
  (loop 
   (cond
     ((equal (car nbt) nil) (return nil))
     ((equal (car nbt) :TAG-END)
      (progn
        (push 0 result)
        (pop nbt)
        ))     
     ((equal (car nbt) :TAG-BYTE)
      (progn
        (push 1 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (car nbt) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-SHORT)
      (progn
        (push 2 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nl2 (car nbt) 2) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-INT)
      (progn
        (push 3 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nl2 (car nbt) 4) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-LONG)
      (progn
        (push 4 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nl2 (car nbt) 8) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-FLOAT)
      (progn
        (push 5 result)
        (pop nbt)
        (error "~a is not implemented yet" (car result))
        ))
     ((equal (car nbt) :TAG-DOUBLE)
      (progn
        (push 6 result)
        (pop nbt)
        (error "~a is not implemented yet" (car result))
        ))
     ((equal (car nbt) :TAG-BYTE-ARRAY)
      (progn
        (push 7 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nbt-byte-list-to-bytes (car nbt)) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-STRING)
      (progn
        (push 8 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-LIST)
      (progn
        (push 9 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (get-byte (car nbt)) result)
        (pop nbt)
        (push (nl2 (car nbt) 4) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-COMPOUND)
      (progn
        (push 10 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-INT-ARRAY)
      (progn
        (push 11 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nbt-int-array-to-bytes (car nbt)) result)
        (pop nbt)
        ))
     ((equal (car nbt) :TAG-LONG-ARRAY)
      (progn
        (push 12 result)
        (pop nbt)
        (push (nbt-string-to-bytes (car nbt)) result)
        (pop nbt)
        (push (nbt-long-array-to-bytes (car nbt)) result)
        (pop nbt)
        ))
     (t
      (return nil))
     ))
  (setq result (lflatten (reverse result)))
  (with-open-file (s filename
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (loop :for i
          :across (coerce result '(simple-array (unsigned-byte 8) (*)))
          :do (write-byte i s))
    )
  result
  )


(defun attempt ()
  "A utitility Function to Test the library"
  (setq filename "./src/tuffsand.schem")
  (setq content (coerce (gunzip filename) 'list))
  (setq conv (nbt-read content))
  (setq res (nbt-write conv "test.txt"))
)



