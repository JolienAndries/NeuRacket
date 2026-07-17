#lang racket/base

(require (for-syntax racket/base (only-in racket/list flatten)) (only-in racket/vector vector-map) 
         racket/class (only-in racket/function identity))
(provide defMLObject identity)

;; initialise pyffi
(require pyffi)
(initialize)
(post-initialize)

;; conversion
(define (python->racket value)
  (cond ((pystring? value) (pystring->string value))
        ((pytuple? value) (vector-map python->racket (pytuple->vector value)))
        ((pylist? value) (map python->racket (pylist->list value)))
        ((pydict? value) (hash-map/copy (pydict->hash value) (lambda (k v) (values (python->racket k) (python->racket v)))))
        (else value)))

(define (racket->python value)
  (cond ((string? value) (string->pystring value))
        ((vector? value)  (vector->pytuple (vector-map racket->python value)))
        ((list? value)  (list->pylist (map racket->python value)))
        ((hash? value) (hash->pydict (hash-map/copy value (lambda (k v) (values (racket->python k) (racket->python v))))))
        (else value)))

(define (output->values output)
  (cond ((vector? output) (vector->values output))
        ((list? output) (vector->values (list->vector output)))
        (else output)))

;;; ML object
(define MLclass%  (class object%
                    (abstract train)
                    (abstract infer)
                    (super-new)))


(define-for-syntax (get-param lst-stx)
  (flatten (map (lambda (stx)
                  (let ((param (syntax->datum stx)))
                    (if (list? param)
                        (map (lambda (el) (datum->syntax stx el)) (cdr param)) ;; stx is the context
                        stx)))
                (syntax->list lst-stx))))

(define-syntax defMLObject
  (lambda (stx)
    (syntax-case stx (file infer train input label output)
      [(_ obj-name
          [file file-name]
          [infer infer-name]
          [train train-name]
          [input in ...]
          [label lbl ...]
          rest-body ...)
  
       (with-syntax ([(input-param ...) (get-param #'(in ...))]
                     [(label-param ...) (get-param #'(lbl ...))])
         #`(define obj-name
             (new (class MLclass%

                    (run* (string-append "with open('" file-name "') as file: exec(file.read())"))
                    (define python-train (run train-name))
                    (define python-infer (run infer-name))

                    (define/override (train label-param ... input-param ...)
                      (apply python-train (map racket->python (list lbl ... in ...))))
                    

                    (define/override (infer input-param ...)
                      (output->values (python->racket (apply python-infer (map racket->python (list in ...))))))

                    rest-body ...
                    (super-new)))))]
      [(_ obj-name
          [file file-name]
          [infer infer-name]
          [train train-name]
          [input in ...]
          [label lbl ...]
          [output out ...]
          rest-body ...)
  
       (with-syntax ([(input-param ...) (get-param #'(in ...))]
                     [(label-param ...) (get-param #'(lbl ...))]
                     [(output-param ...) (get-param #'(out ...))])
         #`(define obj-name
             (new (class MLclass%

                    (run* (string-append "with open('" file-name "') as file: exec(file.read())"))
                    (define python-train (run train-name))
                    (define python-infer (run infer-name))

                    (define/private (post-process output-param ...)
                      (values out ...))

                    (define/override (train label-param ... input-param ...)
                      (apply python-train (map racket->python (list lbl ... in ...))))
                    

                    (define/override (infer input-param ...)
                      (call-with-values (lambda ()
                                          (output->values (python->racket (apply python-infer (map racket->python (list in ...))))))
                                        post-process))

                    rest-body ...
                    (super-new)))))])))