(in-package #:tests)

(def-suite :nbt-untils
  :description "regresssion testing for cl-nbt utility functions")

(in-suite :nbt-untils)

(test lflatten
      "tests the flatten function: ((1 2 3) (1 2 3) (1 2 3)) => (1 2 3 1 2 3 1 2 3)"
      (is (equal (lflatten '((1 2 3) (1 2 3) (1 2 3))) '(1 2 3 1 2 3 1 2 3)))
      )

(test partition-list
  (is (equal (partition-list '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9))))
  (is (equal (partition-list '(1 2 3 4 5 6 7 8 9) -1) nil))
  )

(test ln2
  (is (= (ln2 '(1 1 1 1)) 16843009))
  (is (= (ln2 '(0 1 1 1)) 65793))
  (is (= (ln2 '(1 0 1 1)) 16777473))
  (is (= (ln2 '(1 1 0 1)) 16842753))
  (is (= (ln2 '(1 1 1 0)) 16843008))
  (is (= (ln2 '(0 0 0 0)) 0))
  (for-all ((a (gen-integer :min 0 :max 255))
            (b (gen-integer :min 0 :max 255))
            (c (gen-integer :min 0 :max 255))
            (d (gen-integer :min 0 :max 255))
            )
    (is (numberp (ln2 (list a b c d))))
    (is (< 0 (ln2 (list a b c d)))))
  ) 

(test gzip
  (is (vectorp (gunzip "tests/tuffsand.schem")))
  (is (= (length (gunzip "tests/tuffsand.schem")) 66435))
  )

(test get-tag
  (is (equal (get-tag 0) :tag-end))
  (is (equal (get-tag 1) :tag-byte))
  (is (equal (get-tag 2) :tag-short))
  (is (equal (get-tag 3) :tag-int))
  (is (equal (get-tag 4) :tag-long))
  (is (equal (get-tag 5) :tag-float))
  (is (equal (get-tag 6) :tag-double))
  (is (equal (get-tag 7) :tag-byte-array))
  (is (equal (get-tag 8) :tag-string))
  (is (equal (get-tag 9) :tag-list))
  (is (equal (get-tag 10) :tag-compound))
  (is (equal (get-tag 11) :tag-int-array))
  (is (equal (get-tag 12) :tag-long-array)))

(test get-byte
  (is (equal (get-byte :tag-end) 0))
  (is (equal (get-byte :tag-byte) 1))
  (is (equal (get-byte :tag-short) 2))
  (is (equal (get-byte :tag-int) 3))
  (is (equal (get-byte :tag-long) 4))
  (is (equal (get-byte :TAG-FLOAT) 5))
  (is (equal (get-byte :TAG-DOUBLE) 6))
  (is (equal (get-byte :tag-byte-array) 7))
  (is (equal (get-byte :tag-string) 8))
  (is (equal (get-byte :tag-list) 9))
  (is (equal (get-byte :tag-compound) 10))
  (is (equal (get-byte :tag-int-array) 11))
  (is (equal (get-byte :tag-long-array) 12))
  )

(test nbt-string-to-bytes
  (is (equal
       (nbt-string-to-bytes '(10 "hellow world 1 2 3 4 () []:: ;;.,><./?!@#$$%^&_+-="))
       '(0 10 104 101 108 108 111 119 32 119 111 114 108 100 32 49 32 50 32 51 32 52 32 40 41 32 91 93 58 58 32 59 59 46 44 62 60 46 47 63 33 64 35 36 36 37 94 38 95 43 45 61)))
  )

(test nbt-byte-list-to-bytes
  (is (equal (nbt-byte-list-to-bytes '(4 (1 2 10 0))) '(0 0 0 4 1 2 10 0))) 
  ) 

(test nbt-int-array-to-bytes
  (is (equal (nbt-int-array-to-bytes '(4 (1 2 3 4))) '(0 0 0 4 (0 0 0 1) (0 0 0 2) (0 0 0 3) (0 0 0 4))))
  )

(test nbt-long-array-to-bytes
  (is (equal (nbt-long-array-to-bytes '(4 (1 2 10 0))) '(0 0 0 0 0 0 0 4 (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 2) (0 0 0 0 0 0 0 10) (0 0 0 0 0 0 0 0))))
  )

(test nl2
  (is (equal (nl2 5 0) '(5)))
  (is (equal (nl2 5 1) '(5)))
  (is (equal (nl2 5 2) '(0 5)))
  (is (equal (nl2 5 3) '(0 0 5)))
  (is (equal (nl2 5 4) '(0 0 0 5)))
  (is (equal (nl2 5 5) '(0 0 0 0 5)))
  (is (equal (nl2 5 6) '(0 0 0 0 0 5)))
  (is (equal (nl2 256 4) '(0 0 1 0)))
  (is (equal (nl2 512 4) '(0 0 2 0)))
  (is (equal (nl2 1024 4) '(0 0 4 0)))
  (is (equal (nl2 (* 256 256) 4) '(0 1 0 0)))
  (is (equal (nl2 (* 256 256 3) 4) '(0 3 0 0)))
  (is (equal (nl2 (* 256 256 256) 4) '(1 0 0 0)))
  (is (equal (nl2 (* 256 256 256 3) 4) '(3 0 0 0)))
  )

(test str-replace
      (is (equal (str-replace "testing one two three" "one" "1") "testing 1 two three"))
      (is (equal (str-replace "testing two three" "two" "2") "testing 2 three"))
      (is (equal (str-replace "testing three" "three" "3") "testing 3"))
      (is (equal (str-replace "testing three" "test" "wait") "waiting three"))
      )

(test nbt-change
  (is (equal (nbt-change '(1 2 3 4 (10 "testing as")) "testing as" "changed") '(1 2 3 4 (7 "changed"))))
  (is (equal (nbt-change '(1 2 3 4 (10 "testing as")) 2 10) '(1 10 3 4 (10 "testing as"))))
  )

(test get-pos
  (is (equal (get-pos '(1 2 3 4 (3 "asd") 10) '(3 "asd")) 4)) 
  )

(test nbt-get-pos
  (is (equal (nbt-get-pos '(1 2 3 4 (3 "asd") 10) "asd") 4))
  )
