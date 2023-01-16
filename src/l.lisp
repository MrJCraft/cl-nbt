;;;; l.lisp
(in-package #:nbt)

(defun lflatten (lst &aux (result '()))
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (if (listp el)
                 (rflatten el)
                 (push el result)))))
      (nreverse (rflatten lst))))

(defun partition-list (list parts &key (last-part-longer nil))
  ;; Partition LIST into PARTS parts.  They will all be the same
  ;; length except the last one which will be shorter or, if
  ;; LAST-PART-LONGER is true, longer.  Doesn't deal with the case
  ;; where there are less than PARTS elements in LIST at all (it does
  ;; something, but it may not be sensible).
  (cond
    ((<= parts 0) nil)
    (t (loop with size = (if last-part-longer
                             (floor (length list) parts)
                             (ceiling (length list) parts))
             and tail = list
             for part upfrom 1
             while tail
             collect (loop for pt on tail
                           for i upfrom 0
                           while (or (and last-part-longer (= part parts))
                                     (< i size))
                           collect (first pt)
                           finally (setf tail pt))))
    )
  )


(defun ln1 (l &key (power 255) (p 1))
  "converts u8 list to single number"
  (setq ll (reverse l))
  (if (equal ll nil)
      0
   (+ (* p (car ll))
    (ln (cdr ll) :p (* p 256)))))

(defun ln2 (l &key (power 255))
  "converts u8 list to single number"
  (setq ll (reverse l))
  (setq p 1)
  (setq res nil)
  (loop :for i :in ll
        :do (progn (push (* 1 p i) res)) (setq p (* p 256)))
  (reduce #'+ res))

(defun nbt-get-value (l &optional (type 'list))
  "Gets the value from a nbt tag and returns it as a list with a length and the u8 values"
  (cond
   ( (equal type 'list)
    (progn (setq v (ln2 (subseq nbt 0 l)))
     (setq nbt (subseq nbt l))
     (setq vs (subseq nbt 0 v))
     (setq nbt (subseq nbt v))
     (list v vs)))
   ( (equal type 'int-list)
    (progn (setq v (ln2 (subseq nbt 0 l)))
     (setq nbt (subseq nbt l))
     (setq vs (loop :for (a b c d) :on (subseq nbt 0 (* v 4)) :by #'(lambda (x) (nthcdr 4 x))
                    :collect (ln2 (list a b c d))))
     (setq nbt (subseq nbt (* v 4)))
     (list v vs)))
   ((equal type 'string)
    (progn (setq v (ln2 (subseq nbt 0 l)))
     (setq nbt (subseq nbt l))
     (setq vs (babel:octets-to-string (coerce (subseq nbt 0 v) '(vector (unsigned-byte 8)))))
     (setq nbt (subseq nbt v))
     (list v vs)))))

(defun gunzip (gzip-filename)
  "unzip and decompress file using Gzip"
  (with-open-file (gzstream gzip-filename :direction :input
                            :element-type '(unsigned-byte 8))
      (chipz:decompress nil 'chipz:gzip gzstream)))

(defun get-tag (v)
    (cond
      ((equal v nil)
       nil)
      ((equal v 0)
       :tag-end)
      ((equal v 1)
       :tag-byte)
     ((equal v 2)
      :tag-short)
     ((equal v 3)
      :tag-int)
     ((equal v 4)
      :tag-long)
     ((equal v 5)
      :tag-float)
     ((equal v 6)
      :tag-double)
     ((equal v 7)
      :tag-byte-array)
     ((equal v 8)
      :tag-string)
     ((equal v 9)
      :tag-list)
     ((equal v 10)
      :tag-compound)
     ((equal v 11)
      :tag-int-array)
     ((equal v 12)
      :tag-long-array)
     (t (error "~a is an incorrect tag~% stopped at pos ~a ~%" v pos ))))

(defun get-byte (v)
    (cond
      ((equal v nil)
       nil)
      ((equal v :TAG-END)
        0)
      ((equal v :TAG-BYTE)
        1)
     ((equal v :TAG-SHORT)
       2)
     ((equal v :TAG-INT)
       3)
     ((equal v :TAG-LONG)
       4)
     ((equal v :TAG-FLOAT)
       5)
     ((equal v :TAG-DOUBLE)
       6)
     ((equal v :TAG-BYTE-ARRAY)
       7)
     ((equal v :TAG-STRING)
       8)
     ((equal v :TAG-LIST)
       9)
     ((equal v :TAG-COMPOUND)
       10)
     ((equal v :TAG-INT-ARRAY)
       11)
     ((equal v :TAG-LONG-ARRAY)
       12)
     (t
      (error "~a is an incorrect tag~%" v))))

(defun nbt-string-to-bytes (x)
  (append (nl2 (car x) 2)
        (loop :for c :across (car (cdr x))
              :collect (char-int c))
        )
  )

(defun nbt-byte-list-to-bytes (x)
  (append (nl2 (car x) 4)
          (cadr x)
          )
  )

(defun nbt-int-array-to-bytes (x)
  (append (nl2 (car x) 4)
        (loop :for c :in (car (cdr x))
              :collect (nl2 c 4))
        )
  )

(defun nbt-long-array-to-bytes (x)
  (append (nl2 (car x) 8)
        (loop :for c :in (car (cdr x))
              :collect (nl2 c 8))
        )
  )

(defun nl2 (n ln &key (power 255))
  "converts a single number to a u8 list."
  (setq num n)
  (setq len (if (>= ln 1) (- ln 1) 0))
  (setq res (make-list len :initial-element 0))
  (loop :for i :from len :downto 0 
        :collect (progn
                   (setq res (floor (/ num (expt 256 i))))
                   (setq num (- num (* res (expt 256 i))))
                   res
                   )
        )
  )

(defun str-replace (str c w)
  (concatenate 'string
               (subseq str 0 (search c str))
               w
               (subseq str (+ (length c) (search c str)))))

(defun nbt-change (nbt n c)
  (maplist #'(lambda (x)
               (cond
                 ((and (stringp n) (equal (car x) (list (length n) n))) (setf x (list (length c) c)))
                 ((and (numberp n) (equal (car x) n)) (setf x c))
                 (t (car x))
                   ))
           nbt))

(defun nbt-eval-at-name (nbt str fn)
  (maplist #'(lambda (x)
               (if (equal (car x) (list (length str) str))
                   (setq x (funcall fn x))
                   (car x)
                   ))
           nbt))

(defun get-pos (nbt n)
  (loop
    :for x :in nbt
    :for i :from 0
    :if (equal x n)
      :do (return i)
    )
  )

(defun nbt-get-pos (nbt n)
  (get-pos nbt (list (length n) n))
  )

(defun nbt-str-replace (nbt str c w)
 (nbt-eval-at nbt
              str
              #'(lambda (x) (list (length (cdr x)) (str-replace (cadr x) c w)))))

