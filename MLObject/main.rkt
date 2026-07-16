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




(define-syntax defMLObject
  (lambda (stx)
    (syntax-case stx (file infer train input label post-processing)
      [(_ obj-name
          [file file-name]
          [infer infer-name]
          [train train-name]
          [input arg ...]
          [label output ...]
          rest-body ...)
  
       (with-syntax ([(input-param ...) (flatten (map (lambda (stx)
                                                        (let ((param (syntax->datum stx)))
                                                          (if (list? param)
                                                              (map (lambda (el) (datum->syntax stx el)) (cdr param)) ;; stx is the context
                                                              stx)))
                                                      (syntax->list #'(arg ...))))]
                     [(output-param ...) (flatten (map (lambda (stx)
                                                         (let ((param (syntax->datum stx)))
                                                           (if (list? param)
                                                               (map (lambda (el) (datum->syntax stx el)) (cdr param)) ;; stx is the context
                                                               stx)))
                                                       (syntax->list #'(output ...))))])
         #`(define obj-name
             (new (class MLclass%

                    (run* (string-append "with open('" file-name "') as file: exec(file.read())"))
                    (define python-train (run train-name))
                    (define python-infer (run infer-name))

                    (define/override (train output-param ... input-param ...)
                      (apply python-train (map racket->python (list output ... arg ...))))

                    (define/override (infer input-param ...)
                      (output->values (python->racket (apply python-infer (map racket->python (list arg ...))))))

                    rest-body ...
                    (super-new)))))]
      [(_ obj-name
          [file file-name]
          [infer infer-name]
          [train train-name]
          [input arg ...]
          [label output ...]
          ;;   [post-processing post-func ...]
          rest-body ...)

       (with-syntax ([(input-param ...) (flatten (map (lambda (param)
                                                        (if (list? param) (cdr param) param)) (syntax->list #'(arg ...))))]
                     [(output-param ...) (flatten (map (lambda (param)
                                                         (if (list? param) (cdr param) param)) (syntax->list #'(output ...))))])
         #`(define obj-name
             (new (class MLclass%

                    (run* (string-append "with open('" file-name "') as file: exec(file.read())"))
                    (define python-train (run train-name))
                    (define python-infer (run infer-name))

                    (define/override (train input-param ... output-param ...)
                      (apply python-train (map racket->python '(arg ... output ...))))

                    (define/override (infer input-param ...)
                      (python->racket (apply python-infer (map racket->python '(arg ...)))))

                    rest-body ...
                    (super-new)))))])))