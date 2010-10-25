;; Teachpack for accessing Kiva data through its XML API
;; (A complete rewrite of a teachpack written by Shriram Krishnamurthi & Kathi Fisler)
;; Nadeem Abdul Hamid
;; v1: November 2009

;;;; Documentation for users ;;;;;;;;;;;;;;;;;;;;;;;;

;; The teachpack exports 2 functions and one constant:
;;
;; (get-kiva-page number) produces a list of numbers, symbols, and
;;    strings corresponding to the data on the given page number of 
;;    the kiva website.  Website entries may be omitted if their XML
;;    was formatted differently from our expectations.
;;
;; (get-kiva-page) produces a list of numbers, symbols, and strings
;;    corresponding to the data on the first page of the kiva website.
;;    Website entries may be omitted if their XML was formatted
;;    differently from our expectations.
;;
;;  sample-kiva-data is a constant of data in the format coming from the
;;    website that students can use to test their functions before
;;    connecting to the actual website.
