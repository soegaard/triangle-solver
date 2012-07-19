#lang racket

;;;
;;; After running Whalesong on triangle-solver.rkt
;;; the resulting HTML file contains no code to 
;;; run MathJax. This small script injects the
;;; piece of JavaScript in the header.
;;; 

(define build-dir           "build")
(define html-filename       "triangle-solver.html")
(define tmp-filename        (string-append html-filename ".tmp"))
(define html-path           (build-path build-dir html-filename))
(define tmp-path            (build-path build-dir tmp-filename))
;(define mathjax-script-path (build-path "mathjax-script.js"))
(define mathjax-script-path (build-path "mathjax-cdn.js"))
(define raphael-script-path (build-path "raphael-min.js"))

(define (is-title-line? line)
  (regexp-match #rx".*<title>.*" line))

(define (copy-until-title-line)
  (for/or ([line (in-lines)])
    (displayln line)
    (is-title-line? line)))

(define (insert-mathjax-script)
  (with-input-from-file mathjax-script-path
    (λ ()
      (for ([line (in-lines)])
        (displayln line)))))

(define (insert-raphael-script)
  (with-input-from-file raphael-script-path
    (λ ()
      (for ([line (in-lines)])
        (displayln line)))))

(define (copy-until-eof)
  (for ([line (in-lines)])
    (displayln line)))

(with-input-from-file html-path
  (λ ()
    (with-output-to-file tmp-path
      (λ ()
        (copy-until-title-line)
        (insert-mathjax-script)
        (insert-raphael-script)
        (copy-until-eof))
      #:exists 'replace)))
