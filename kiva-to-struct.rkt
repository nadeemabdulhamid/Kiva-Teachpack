#lang racket

(require "kiva-teachpack.rkt")

(define-struct loan (borrower amount year) #:transparent)

(kiva-data->structs      ;; function from the teachpack
 sample-kiva-data        ;; this is the row of data
 make-loan               ;; your structure's constructor 
 (name loan_amt date)    ;; the fields you want to extra from each kiva data row
                         ;;  in the order that their values should be supplied to
                         ;;  your structure's constructor
 )
