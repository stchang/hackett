#lang racket
(require (for-syntax
          rackunit
          syntax/srcloc
          syntax/parse
          racket/pretty
          (only-in hackett/private/typecheck
                   type<:!
                   type<:/elaborate!))
         (only-in hackett/private/base
                  τ⇒!
                  current-type-context)
         (only-in hackett/private/type-language
                  type-namespace-introduce
                  expand-type)
         rackunit)

(provide (all-defined-out))

(begin-for-syntax
  (define (tyerrmsg #:got a #:expected b #:in [in ""])
    (format "type mismatch.*~a.*~a.*~a" a b in)))

(define-syntax check-type
  (syntax-parser
    [(_ e (~datum :) t_expected)
     #:do[(define-values (e- t_e) (τ⇒! #'e))
          #;(displayln (current-type-context))
          #;(pretty-print (list e- t_e))
          (define t_expected+
            (expand-type (type-namespace-introduce #'t_expected)))]
     #:when (type<:/elaborate! t_e t_expected+ #:src #'e)
     #'(void)]
    [(_ e (~datum :) t_expected (~datum ->) v_expected) ; runtime val
     #:do[(define-values (e- t_e) (τ⇒! #'e))
          #;(displayln (current-type-context))
          #;(pretty-print (list e- t_e))
          (define t_expected+
            (expand-type (type-namespace-introduce #'t_expected)))]
     #:when (type<:/elaborate! t_e t_expected+ #:src #'e)
     #`(check-equal? (force #,e-) v_expected)]))

(define-syntax (typecheck-fail stx)
  (syntax-parse stx #:datum-literals (:)
    [(_ e (~optional (~seq #:with-msg msg-pat) #:defaults ([msg-pat #'""])))
     #:with msg:str (eval-syntax (datum->syntax #'h (syntax->datum #'msg-pat)))
     #:when (with-check-info*
             (list (make-check-expected (syntax-e #'msg))
                   (make-check-expression (syntax->datum stx))
                   (make-check-location (build-source-location-list stx))
                   (make-check-name 'typecheck-fail)
                   (make-check-params (list (syntax->datum #'e) (syntax-e #'msg))))
             (λ ()
               (check-exn
                (λ (ex)
                  (and (or (exn:fail? ex) (exn:test:check? ex))
                       ; check err msg matches
                       (regexp-match? (syntax-e #'msg) (exn-message ex))))
                (λ ()
                  (τ⇒! #'e)))))
     #'(void)]))

(define-syntax (typecheck-fail/toplvl stx)
  (syntax-parse stx #:datum-literals (:)
    [(_ e (~optional (~seq #:with-msg msg-pat) #:defaults ([msg-pat #'""])))
     #:with msg:str (eval-syntax (datum->syntax #'h (syntax->datum #'msg-pat)))
     #:when (with-check-info*
             (list (make-check-expected (syntax-e #'msg))
                   (make-check-expression (syntax->datum stx))
                   (make-check-location (build-source-location-list stx))
                   (make-check-name 'typecheck-fail)
                   (make-check-params (list (syntax->datum #'e) (syntax-e #'msg))))
             (λ ()
               (check-exn
                (λ (ex)
                  (and (or (exn:fail? ex) (exn:test:check? ex))
                       ; check err msg matches
                       (regexp-match? (syntax-e #'msg) (exn-message ex))))
                (λ ()
                  (local-expand #'e 'top-level null)))))
     #'(void)]))

