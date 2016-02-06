#lang Scheme
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
;(load "include.ss")
(include "include.ss")
;; contains simple dictionary definition
;(load "test-dictionary.ss")
(include "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; returns a list-represented bit vector from a given dictionary
(define parse_dict
  (lambda (dict hashfns) 
   (cond 
       ((null? dict) dict)
       ((list? dict) (append (for_each_hashfn hashfns (car dict)) (parse_dict (cdr dict) hashfns)))
       (else dict))    
   ))

;;Returns a list of hash-vals build from each hash function in the list applied to the given word
(define for_each_hashfn
  (lambda (hashfns word)
    (cond
      ((null? hashfns) hashfns)
      ((pair? hashfns) (cons ((car hashfns) word) (for_each_hashfn (cdr hashfns) word)))
     (else '()))
    )
  )

;;Returns #t if the , #f otherwise
(define check_word
  (lambda (w hashfn lst)
    (cond
      ((null? lst) #f)
      ((not (= (hashfn 	w) (car lst))) (check_word w hashfn (cdr lst)))
      (else #t)
     )
    ))

;;checks if a word is in the bitvector for each hash function in hashfl
(define spell_check
  (lambda (w hashfl lst)
    (cond
      ((null? hashfl) '())
      (else (cons (check_word w (car hashfl) lst) (spell_check w (cdr hashfl) lst)))
      )
    )
  )

;; -----------------------------------------------------
;; KEY FUNCTION
(define key
  (lambda (w)
     (let ((k 5387))
     (cond
       ((null? w) k)
       ((pair? w) (+ (* 31 (key (cdr w))) (ctv (car w))))   ;almost same 
       (else (ctv w)))
)))


;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562


;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (w)
       (modulo (key w) size));;
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
       (floor(* size (- (* (key w) A) (floor(* (key w) A))  )))
)))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))


;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 538
;;  (hash-1 '(w a y))         ==> 635
;;  (hash-1 '(r a i n b o w)) ==> 308
;;
;;  (hash-2 '(h e l l o))     ==> 379
;;  (hash-2 '(w a y))         ==> 642
;;  (hash-2 '(r a i n b o w)) ==> 172
;;
;;  (hash-3 '(h e l l o))     ==> 415.0
;;  (hash-3 '(w a y))         ==> 390.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR


(define gen-checker
  (lambda (hashfunctionlist dict)
   (let ((bit_vector (parse_dict dict hashfunctionlist))) ;;local binding of hashed dictionary bitvector (only done once)
    (lambda (w) 
      (reduce 
       (lambda (x y) (cond ((and x y) #t) (else #f))) 
       (spell_check w hashfunctionlist bit_vector) 
       #t)
))))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

