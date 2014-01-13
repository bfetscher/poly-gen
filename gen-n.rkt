#lang racket

(require "poly-types.rkt"
         "unparse.rkt"
         racket/cmdline)

(command-line
 #:args ([n 0])
 (begin
   (map (compose displayln unp-exp)
        (begin0
          (generate-n-terms (string->number n))
          (newline)))
   (newline)))

