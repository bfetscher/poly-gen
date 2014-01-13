#lang racket

(require "poly-types.rkt"
         racket/cmdline)

(command-line
 #:args ([n 0])
 (begin
   (generate-n-terms (string->number n))
   (void)))

