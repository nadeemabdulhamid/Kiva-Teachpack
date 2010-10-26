 Teachpack for accessing Kiva data through its XML API
 (A complete rewrite of a teachpack written by Shriram Krishnamurthi & Kathi Fisler)
 Nadeem Abdul Hamid
 v1.2: October 2010: bug fix, doc updates, kiva-data->structs
       (thank you, Stephen Bloch & Shriram Krishnamurthi)
 v1: November 2009

 Documentation for users ;;;;;;;;;;;;;;;;;;;;;;;;

 The teachpack primarily exports 3 functions and one constant:

 (get-kiva-page number) produces a list of numbers, symbols, and
    strings corresponding to the data on the given page number of 
    the kiva website.  Website entries may be omitted if their XML
    was formatted differently from our expectations.

 (get-kiva-page) produces a list of numbers, symbols, and strings
    corresponding to the data on the first page of the kiva website.
    Website entries may be omitted if their XML was formatted
    differently from our expectations.

  (total-kiva-pages) produces the total number of pages of loan 
    information available on the Kiva web site. Please, do not 
    attempt to retrieve more than 5 pages of information at a time
    in your program -- it can put undue load on the campus network 
    and on the Kiva website. (See point #6 on the Kiva API Code 
    of Conduct page: http://build.kiva.org/docs/code_of_conduct ).

  sample-kiva-data is a constant of data in the format coming from the
    website that students can use to test their functions before
    connecting to the actual website.

 Information about an individual loan is represented as a tuple:
  (list Number Number String Symbol Number Number Number String Symbol String String Number)
 interpreted as:
  (id name size status loan_amt funded_amt paid_amt activity sector use country date)


 Because these long rows may be difficult to use, an additional function
   is provided that allows mapping a list of rows of Kiva data to a 
   list of client-defined structures. For instance, suppose you really just
   want to work with a list of structures that keep track of a few fields,
   defined as:
     (define-struct loan (borrower amount year))
   Then, use the (kiva-data->structs ...) function as illustrated in this
   example:
 
     (kiva-data->structs      
      sample-kiva-data        ;; this is the list of rows of data, could be (get-kiva-page)
      make-loan               ;; your structure's constructor 
      (name loan_amt date)    ;; the fields you want to extract from each kiva data row
                              ;;  in the order that their values should be supplied
                              ;;  to your structure's constructor
     )

 If you want to trim the data in the rows, but not introduce a structure, use
   the (kiva-data/select ...) function which works in a similar manner to
   kiva-data->structs, but doesn't take a structure's constructor parameter and
   produces a list of rows with only values for the specified fields, for example:

     (kiva-data/select
      sample-kiva-data
      (name loan_amt date))


