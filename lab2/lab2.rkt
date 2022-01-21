#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(: marginal-tax : Integer -> Integer)
(define (marginal-tax income)
  (cond
    [(<= income 18000) 0]
    [(<= income 32000) (exact-ceiling (* 0.15 (- income 18000)))]
    [(<= income 56000) (exact-ceiling (+ 2100 (* 0.25 (- income 32000))))]
    [else (exact-ceiling (+ 8100 (* 0.35 (- income 56000))))]))
;; marginal-tax: compute the amount of tax owed on the given income.
;; parameter "income": an integer number of thunks
;; output: the amount of marginal tax owed

(check-expect (marginal-tax 15000) 0)
(check-expect (marginal-tax 30000) 1800)
(check-expect (marginal-tax 50000) 6600)
(check-expect (marginal-tax 60000) 9500)

(: transit-deduction : Integer Integer -> Integer)
(define (transit-deduction income trips)
  (if (<= (* 10 trips) (* 0.25 income))
      (* 10 trips)
      (exact-ceiling (* 0.25 income))))
;; transit-deduction: compute deduction of 10 thunks per
;;  trip, but not more than 25% of income
;; parameter "income": an integer number of thunks
;; parameter "trips": the number of trips on transit
;; output: deduction as described above

(check-expect (transit-deduction 80000 100) 1000)
(check-expect (transit-deduction 10000 300) 2500)


(: hummingbird-deduction : Integer Integer -> Integer)
(define (hummingbird-deduction gallons-per-year num-people)
  (min (* (exact-ceiling (/ gallons-per-year num-people)) 8) 4000))
;; hummingbird-deduction: compute deduction of gallons-per-year-per-person
;;  in thunks times eight, but not more than 4000
;; parameter "gallons-per-year": the nectar provided in gallons per year
;; parameter "num-people": the number of people in the family unit
;; output: deduction as described above

(check-expect (hummingbird-deduction 150 3) 400)
(check-expect (hummingbird-deduction 3000 3) 4000)

(: itemized : Integer Integer Integer Integer -> Integer)
(define (itemized income num-people trips gallons-per-year)
  (+ (transit-deduction income trips)
     (hummingbird-deduction gallons-per-year num-people)))
;; itemized: compute itemized deduction on income
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;; parameter "trips": the number of trips on transit
;; parameter "gallons-per-year": the nectar provided in gallons per year
;; output: total itemized deduction for this family unit

(check-expect (itemized 80000 3 100 150) 1400)
(check-expect (itemized 20000 3 200 300) 2800)


(: standard : Integer -> Integer)
(define (standard num-people)
  (* 2000 num-people))
;; standard: compute the standard deduction for a family unit,
;;  which is just 2000 thunks per person
;; parameter "num-people"
;; output: the deduction for that number of people

(check-expect (standard 3) 6000)
(check-expect (standard 6) 12000)

(: should-itemize? : Integer Integer Integer Integer -> Boolean)
(define (should-itemize? income num-people trips gallons-per-year)
  (> (itemized income num-people trips gallons-per-year)
       (standard num-people)))
;; should-itemize?: should family unit itemize or take standard? 
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;; parameter "trips": the number of trips on transit
;; parameter "gallons-per-year": the nectar provided in gallons per year
;; output: #t if itemized deduction is larger than standard, #f otherwise

(check-expect (should-itemize? 80000 3 100 150) #f)
(check-expect (should-itemize? 80000 3 2000 1000) #t)


(: tax-return : Integer Integer Integer Integer Integer -> Integer)
(define (tax-return income num-people trips gallons-per-year withheld)
  (- (marginal-tax
      (- income (max
                 (itemized income num-people trips gallons-per-year)
                 (standard num-people))))
     withheld))
                          
;; tax-return: Determine the negative (refund) or positive (payment) due
;;  family unit, given their income as well as the amount withheld.
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;; parameter "trips": the number of trips on transit
;; parameter "gallons-per-year": the nectar provided in gallons per year
;; parameter "withheld": amount of thunks already withheld
;; output: positive or negative debt to the TRS (negative debt is a refund)

(check-expect (tax-return 80000 3 100 150 15000) -600)
(check-expect (tax-return 80000 3 2000 900 15000) -6340)
(check-expect (tax-return 80000 2 100 150 10000) 5100)

(test)