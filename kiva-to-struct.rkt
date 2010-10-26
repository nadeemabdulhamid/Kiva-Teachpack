;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname kiva-to-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require "kiva-teachpack.rkt")

(define-struct loan (borrower amount year))

(kiva-data->structs      ;; function from the teachpack
 sample-kiva-data        ;; this is the row of data
 make-loan               ;; your structure's constructor 
 (name loan_amt date)    ;; the fields you want to extra from each kiva data row
                         ;;  in the order that their values should be supplied to
                         ;;  your structure's constructor
 )
