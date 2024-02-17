#lang racket

(define (morse-decode str)
  (define morse-codes
    '(("._" . "A") ("_..." . "B") ("_._." . "C") ("_.." . "D") ("." . "E") (".._." . "F")
      ("__." . "G") ("...." . "H") (".." . "I") (".___" . "J") ("_._" . "K") ("._.." . "L")
      ("__" . "M") ("_." . "N") ("___" . "O") (".__." . "P") ("__._" . "Q") ("._." . "R")
      ("..." . "S") ("_" . "T") (".._" . "U") ("..._" . "V") (".__" . "W") ("_.._" . "X")
      ("_.__" . "Y") ("__.." . "Z")
      ("_____" . "0") (".____" . "1") ("..___" . "2") ("...__" . "3")
      ("...._" . "4") ("....." . "5") ("_...." . "6") ("__..." . "7")
      ("___.." . "8") ("____." . "9") ("._._._" . ".")))
  
  (define (char->morse char)
    (cdr (assoc char morse-codes)))
  
  (define (words->morse words)
    (map (lambda (word)
           (string-append (char->morse word)))
         (string-split words))) 
  
  (apply string-append (words->morse str)))
