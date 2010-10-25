;; Teachpack for accessing Kiva data through its XML API
;; (A complete rewrite of a teachpack written by Shriram Krishnamurthi & Kathi Fisler)
;; Nadeem Abdul Hamid
;; v1.1: October 2010: bug fix, doc updates
;; v1: November 2009

;;;; Documentation for users ;;;;;;;;;;;;;;;;;;;;;;;;

;; The teachpack exports 3 functions and one constant:
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
;;  (total-kiva-pages) produces the total number of pages of loan 
;;    information available on the Kiva web site. Please, do not 
;;    attempt to retrieve more than 5 pages of information at a time
;;    in your program -- it can put undue load on the campus network 
;;    and on the Kiva website. (See point #6 on the Kiva API Code 
;;    of Conduct page: http://build.kiva.org/docs/code_of_conduct ).
;;
;;  sample-kiva-data is a constant of data in the format coming from the
;;    website that students can use to test their functions before
;;    connecting to the actual website.
;;
;; Information about an individual loan is represented as a tuple:
;;  (list Number Number String Symbol Number Number Number String Symbol String String Number)
;; interpreted as:
;;  (id name size status loan_amt funded_amt paid_amt activity sector use country date)


;; here goes...
(module kiva-teachpack racket
  
  (require scheme/list)
  (require net/url)
  (require xml)
  (require test-engine/scheme-tests)
  
  (define SEARCH-URL "http://api.kivaws.org/v1/loans/search.xml")
  
  ;(define in (get-pure-port (string->url 
  ;                           "http://api.kivaws.org/v1/loans/search.xml?page=1000")))
  ;(define doc (xml->xexpr (document-element (read-xml in))))
 
  ;; fetch-kiva-page : [Number] -> Xexpr
  (define (fetch-kiva-page [page-no 0])
    (let ([url-ext (if (zero? page-no) "" (string-append "?page="
                                                         (number->string page-no)))])
      (xml->xexpr 
       (document-element 
        (read-xml (get-pure-port (string->url (string-append SEARCH-URL url-ext))))))))
  
  #|
(define-struct: paging (; [current-page : Number]
                        [pages : Number]
                        [page-size : Number]
                        [total-entries : Number]) #:transparent)
|#
  (define-struct paging (pages page-size total-entries) #:transparent)
  
  ;; extract-paging-info : Xexpr -> Paging
  ;; extract paging information from the response Xexpr
  
  (check-expect (extract-paging-info '(response () "stuff\n" 
                                                (paging () (page () "1")
                                                        (total () "142382")
                                                        (page_size () "20")
                                                        (pages () "7120"))
                                                (loans
                                                 ()
                                                 "yada"
                                                 (loan () "yada"))))
                (make-paging 7120 20 142382))
  
  (define (extract-paging-info e)
    (let* ([lst (cddr (findf (lambda (x) (and (cons? x) (equal? 'paging (first x)))) e))]
           [get-num-val (lambda (tag) (string->number (third (assoc tag lst))))])
      (apply make-paging
             (map get-num-val '(pages page_size total)))))
  

  ;; extract-loans-list : Xexpr -> [listof Xexpr]
  ;; extract the list of loan Xexprs from the response Xexpr
  (check-expect (extract-loans-list '(response () "stuff\n" 
                                               (paging () (page () "1")
                                                       (total () "142382")
                                                       (page_size () "20")
                                                       (pages () "7120"))
                                               (loans
                                                ()
                                                "yada"
                                                (loan () "yada")
                                                (loan () "yidi"))))
                '((loan () "yada") (loan () "yidi")))
  
  (define (extract-loans-list e)
    (let ([find-res (findf (lambda (x) (and (cons? x) (equal? 'loans (first x)))) e)])
      (if (or (false? find-res) (< (length find-res) 4))
          '()
          (cdddr find-res))))
  
  ;; extract-loan-info : Xexpr ->
  ;;     (list Number Number String Symbol Number Number Number String Symbol String String Number)
  ;; i.e. '(id name size status loan_amt funded_amt paid_amt activity sector use country date)
  (check-expect (extract-loan-info 
                 '(loan
                   ()
                   (id () "124095")
                   (name () "Kucherboi Mahmaliev Group")
                   (status () "in_repayment")
                   (loan_amount () "1200")
                   (funded_amount () "1100")
                   (paid_amount () "100")
                   (borrower_count () "3")
                   (image () (id () "356769") (template_id () "1"))
                   (activity () "Animal Sales")
                   (sector () "Agriculture")
                   (use () "Purchase of bighorn cattle")
                   (location
                    ()
                    (country_code () "TJ")
                    (country () "Tajikistan")
                    (town () "Khuroson")
                    (geo () (level () "country") (pairs () "39 71") (type () "point")))
                   (partner_id () "63")
                   (posted_date () "2009-08-05T17:20:06Z")
                   (description
                    ()
                    (languages ((type "list")) (language () "ru") (language () "en")))))
                '(124095 "Kucherboi Mahmaliev Group" 3 in-repayment 1200 1100 100
                         "Animal Sales" agriculture "Purchase of bighorn cattle"
                         "Tajikistan" 2009))
  
  (define (extract-loan-info e)
    (with-handlers ([exn:fail? (lambda (exn) empty)])
      (let* ([lst (cddr e)]
             [mt-if-false (lambda (r) (if (false? r) (list "" "" "") r))]
             [zero-if-false (lambda (r) (if (false? r) 0 r))]
             [get-val (lambda (tag) (third (mt-if-false (assoc tag lst))))]
             [get-num-val (lambda (tag) (zero-if-false (string->number (get-val tag))))]
             [get-sym-val (lambda (tag) 
                            (string->symbol 
                             (string-downcase (regexp-replace* "[ _]"  (get-val tag) "-"))))]
             [country (third (mt-if-false (assoc 'country (cddr (assoc 'location lst)))))]
             [year (string->number (substring (get-val 'posted_date) 0 4))])
        (list (get-num-val 'id)
              (get-val 'name)
              (get-num-val 'borrower_count)
              (get-sym-val 'status)
              (get-num-val 'loan_amount)
              (get-num-val 'funded_amount)
              (get-num-val 'paid_amount)
              (get-val 'activity)
              (get-sym-val 'sector)
              (apply string-append (cddr (mt-if-false (assoc 'use lst))))
              country
              year
              ))))
  
  ;; extract-loans : Xexpr -> [listof Loan]
  (define (extract-loans e)
    (filter (lambda (l) (not (empty? l))) 
            (map extract-loan-info (extract-loans-list e))))
  
  ;; get-paging-info :  -> Paging
  (define (get-paging-info)
    (extract-paging-info (fetch-kiva-page)))
  
  ;; total-kiva-page :  -> Number
  (define (total-kiva-pages)
    (paging-pages (get-paging-info)))
  
  ;; get-kiva-page : [Number] -> [listof Loan]
  (define (get-kiva-page (page 0))
    (extract-loans (fetch-kiva-page page)))
  
  
  (provide get-kiva-page total-kiva-pages sample-kiva-data)
  
  
  
  (define sample-kiva-data
    '((149163
       "Jose Goltran Navarro Chavez"
       1
       in-repayment
       175
       175
       0
       "Farming"
       agriculture
       "Purchase fertilizers for his crop of beans"
       "Costa Rica" 2007)
      (149138
       "The Fruit Group"
       3
       funded
       950
       950
       0
       "Retail"
       retail
       "Purchase merchandise & supplies for their respective businesses"
       "Nicaragua" 2008)
      (148756
       "Angel"
       1
       funded
       525
       525
       0
       "Rickshaw"
       transportation
       "To purchase tires and to maintain the minibus, as well as to buy gas for its operation"
       "Bolivia" 2009)
      (149235
       "Jumanne Said"
       1
       funded
       475
       475
       0
       "Retail"
       retail
       "To buy more business goods to meet customer demand"
       "Tanzania" 2009)
      (124099
       "Mirali Iskandarov"
       1
       in-repayment
       1000
       1000
       166.67
       "Bakery"
       food
       "Buy ingredients and update ovens"
       "Tajikistan" 2008)
      (124373
       "Murodali Saidaliev"
       1
       in-repayment
       275
       275
       55
       "Animal Sales"
       agriculture
       "Purchase small horned animals (goats, sheep, etc.)"
       "Tajikistan" 2008)
      (124288
       "Rahmatullo Bobobekov"
       1
       in-repayment
       475
       475
       59.38
       "Agriculture"
       agriculture
       "Buy seeds and fertilizer"
       "Tajikistan" 2009)
      (127138
       "Joyce Richard"
       1
       in-repayment
       625
       625
       125
       "Beauty Salon"
       services
       "To buy more business materials"
       "Tanzania" 2008)
      (105961
       "Amanecer Group"
       15
       paid
       2625
       2625
       2625
       "Retail"
       retail
       "To buy clothes, vegetables, household goods, hardware supplies etc."
       "Paraguay" 2009)
      (105970
       "Jazmín Group"
       15
       paid
       2050
       2050
       2050
       "Retail"
       retail
       "To buy ingredients for meals, material for clothing production, ect."
       "Paraguay" 2007)
      (106615
       "Tamaraneh Ii Group"
       2
       in-repayment
       775
       775
       387.5
       "Food Market"
       food
       "To invest in her business"
       "Sierra Leone"
       2009)
      (54749
       "Kifoyat Rahimjanova"
       1
       paid
       525
       525
       525
       "Farming"
       agriculture
       "Purchasing of the seeds and mineral fertilizer."
       "Tajikistan"
       2008)
      (55626
       "Fatou Seydi"
       1
       paid
       1200
       1200
       1200
       "Retail"
       retail
       "Purchase and resale of tinted fabric boubous (traditional African clothing)"
       "Senegal"
       2008)
      (55785
       "José Amos Domingos"
       1
       in-repayment
       650
       650
       568.68
       "Construction"
       construction
       "To buy materials to build family house"
       "Mozambique"
       2008)
      (55786
       "José Pinto Chaúque"
       1
       in-repayment
       750
       750
       609.44
       "Construction"
       construction
       "To buy materials to build family house"
       "Mozambique"
       2008)
      (144197
       "Iluciones Group"
       13
       fundraising
       2250
       75
       0
       "Services"
       services
       "Capital de Operaciones"
       "Bolivia"
       2009)
      (143600
       "Avad"
       1
       fundraising
       625
       300
       0
       "Transportation"
       transportation
       "COMPRA DE BIENES PARA EL HOGAR"
       "Bolivia"
       2009)
      (149041
       "David Onyango Omondi"
       1
       fundraising
       1075
       575
       0
       "General Store"
       retail
       "To purchase stock for his general retail shop, including dairy products"
       "Kenya"
       2009)
      ))
  
  ;(test)
  
  )