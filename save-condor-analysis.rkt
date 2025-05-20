#lang racket/base

; This (module) is a hack to get this code to load before the (requires) call below.
; We want to first set up the command line args before initializing stuff in db-queries.rkt.
(module cmd racket/base
  (require gregor
           racket/cmdline
           "params.rkt")

  (command-line
   #:program "racket save-condor-analysis.rkt"
   #:once-each
   [("-d" "--end-date") end-date
                        "End date for saving. Defaults to today"
                        (save-end-date (iso8601->date end-date))]
   [("-m" "--markets") markets
                       "Markets to save. Defaults to SPY,MDY,SLY,SPSM"
                       (save-markets markets)]
   [("-n" "--db-name") name
                       "Database name. Defaults to 'local'"
                       (db-name name)]
   [("-p" "--db-pass") password
                       "Database password"
                       (db-pass password)]
   [("-u" "--db-user") user
                       "Database user name. Defaults to 'user'"
                       (db-user user)]))

(require 'cmd
         gregor
         racket/list
         "db-queries.rkt"
         "params.rkt"
         "condor-analysis.rkt"
         "structs.rkt")

(cond [(and (or (= 0 (->wday (save-end-date)))
                (= 6 (->wday (save-end-date)))))
       (displayln (string-append "Requested date " (date->iso8601 (save-end-date)) " falls on a weekend. Terminating."))
       (exit)])

(run-condor-analysis (save-markets) "" (date->iso8601 (-months (save-end-date) 5)) (date->iso8601 (save-end-date)) #:fit-vols #t)

(for-each (λ (msis)
            (with-handlers
              ([exn:fail? (λ (e) (displayln (string-append "Failed to process " (condor-analysis-stock msis) " for date "
                                                           (date->iso8601 (save-end-date))))
                             (displayln e))])
              (insert-condor-analysis (date->iso8601 (save-end-date))
                                     msis
                                     (first (hash-ref condor-analysis-hash (condor-analysis-market msis)))
                                     (second (hash-ref condor-analysis-hash (condor-analysis-market msis)))
                                     (first (hash-ref condor-analysis-hash (condor-analysis-sector msis)))
                                     (second (hash-ref condor-analysis-hash (condor-analysis-sector msis)))
                                     (first (hash-ref condor-analysis-hash (condor-analysis-industry msis)))
                                     (second (hash-ref condor-analysis-hash (condor-analysis-industry msis)))
                                     (first (hash-ref condor-analysis-hash (condor-analysis-stock msis)))
                                     (second (hash-ref condor-analysis-hash (condor-analysis-stock msis))))))
          condor-analysis-list)
