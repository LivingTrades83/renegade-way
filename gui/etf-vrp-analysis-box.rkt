#lang racket/base

(require racket/class
         racket/gui
         racket/list
         "../db-queries.rkt"
         "../etf-vrp-analysis.rkt"
         "../structs.rkt"
         "chart.rkt"
         "option-strategy-frame.rkt")

(provide etf-vrp-analysis-box
         update-etf-vrp-analysis-box)

(define analysis-box-ref #f)

(define (update-etf-vrp-analysis-box etf-vrp-analysis-list)
  (send analysis-box-ref set
          (map (λ (m) (etf-vrp-analysis-etf m)) etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-iv-hv m)) (real->decimal-string (etf-vrp-analysis-iv-hv m)) ""))
               etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-ivp-1yr m)) (real->decimal-string (etf-vrp-analysis-ivp-1yr m)) ""))
               etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-30d-60d-fwd-vol m)) (real->decimal-string (etf-vrp-analysis-30d-60d-fwd-vol m)) ""))
               etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-30d-60d-flat-fwd-vol m)) (real->decimal-string (etf-vrp-analysis-30d-60d-flat-fwd-vol m)) ""))
               etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-flat-fwd-to-fwd-ratio m)) (real->decimal-string (etf-vrp-analysis-flat-fwd-to-fwd-ratio m)) ""))
               etf-vrp-analysis-list)
          (map (λ (m) (if (real? (etf-vrp-analysis-option-spread m)) (real->decimal-string (etf-vrp-analysis-option-spread m)) ""))
               etf-vrp-analysis-list)
          )
  (map (λ (m i) (send analysis-box-ref set-data i m))
         etf-vrp-analysis-list (range (length etf-vrp-analysis-list))))

(define analysis-box-columns (list "ETF" "IV/HV" "IVP" "FwdVol" "FFwdVol" "FFwd/Fwd" "OptSprd"))

(define (etf-vrp-analysis-box parent-panel start-date end-date)
  (define analysis-box
    (new list-box%
         [parent parent-panel]
         [label #f]
         [callback (λ (b e)
                     (let ([stock (etf-vrp-analysis-etf (send b get-data (first (send b get-selections))))])
                       (refresh-chart ""
                                      ""
                                      ""
                                      stock
                                      start-date
                                      end-date)
                       (refresh-option-strategy stock
                                                end-date
                                                (dohlc-close (first (get-date-ohlc stock end-date end-date)))
                                                "VR")))]
         [style (list 'single 'column-headers 'vertical-label)]
         [columns analysis-box-columns]
         [choices (list "")]))
  (let ([box-width (send analysis-box get-width)]
        [num-cols (length analysis-box-columns)])
    (for-each (λ (i) (send analysis-box set-column-width i 80 80 80))
              (range num-cols)))
  (set! analysis-box-ref analysis-box)
  (update-etf-vrp-analysis-box etf-vrp-analysis-list))
