;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  reports.scm
;;  load the standard report definitions
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash reports))

(use-modules (srfi srfi-13))
(use-modules (srfi srfi-8))
(use-modules (gnucash app-utils))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash report))
(use-modules (gnucash utilities))
(use-modules (gnucash reports standard register))
(use-modules (gnucash reports standard new-aging))
(use-modules (gnucash reports standard new-owner-report))

(export gnc:budget-report-create)
(export gnc:register-report-create)
(export gnc:invoice-report-create)
(export gnc:payables-report-create)
(export gnc:receivables-report-create)
(export gnc:owner-report-create-with-enddate)


;; gnc-locale-name depends on ${LC_ALL}. Unknow function to use LC_MONETARY (probably the most coherent setting to be able to use UI in une Language and Tax Report in another locale).

(let
  ((loc-spec (if (and ( >= (string-length (gnc-locale-name)) 5 ) ( < (string-length (gnc-locale-name)) 12 )) (substring (gnc-locale-name) 0 5) "en_US" )))
  (report-module-loader
   (list
    '(gnucash reports standard) ; prefix for standard reports included in gnucash
    '(gnucash reports example)  ; rexample for example reports included in gnucash
    `(gnucash reports locale-specific ,(string->symbol loc-spec)))))

(define (gnc:register-report-create account split query journal? ledger-type?
                                    double? title debit-string credit-string)
  (let* ((acct-type (xaccAccountGetType account))
         (create-fcn (gnc:lookup-register-report acct-type split)))
    (gnc:debug "create-fcn: " create-fcn)
    (if create-fcn
        (create-fcn account split query journal? double? title
                    debit-string credit-string)
        (gnc:register-report-create-internal #f query journal? ledger-type? double?
                                             title debit-string credit-string))))

;; Creates a new report instance for the given invoice. The given
;; report-template-id must refer to an existing report template, which
;; is then used to instantiate the new report instance.
(define (gnc:invoice-report-create invoice report-template-id)
    (if (gnc:find-report-template report-template-id)
        ;; We found the report template id, so instantiate a report
        ;; and set the invoice option accordingly.
        (let* ((options (gnc:make-report-options report-template-id)))
          (gnc-set-option
           (gnc:optiondb options)
           gnc:pagename-general gnc:optname-invoice-number
           invoice)
          (gnc:make-report report-template-id options))
        ;; Invalid report-template-id, so let's return zero as an invalid report id.
        0
        ))

(define budget-ID "810ed4b25ef0486ea43bbd3dddb32b11")
(define (gnc:budget-report-create budget)
  (if (gnc:find-report-template budget-ID)
      (let* ((options (gnc:make-report-options budget-ID)))
        (gnc-set-option options gnc:pagename-general "Budget" budget)
        (gnc:make-report budget-ID options))
      -1))

(define gnc:payables-report-create payables-report-create-internal)
(define gnc:receivables-report-create receivables-report-create-internal)
(define gnc:owner-report-create-with-enddate owner-report-create-with-enddate)
