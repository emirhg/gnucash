;; -*-scheme-*-
;;
;;  Richard -Gilligan- Uschold
;; These are TXF codes and a brief description of each. See taxtxf.scm
;; and txf-export-help.scm
;;
;; See also https://www.turbotax.com/txf/ [DEAD LINK]
;;
;; Updated Jan 2019 to include codes for version 42, although new codes not
;;   implemented yet because data not available
;; Updated Feb 2013, Jan 2014 & Jan 2019 . J. Alex Aycinena
;; Added updated tax line info
;; Updated Oct 2009. J. Alex Aycinena
;; Added 'txf-tax-entity-types' and related getter functions
;; Added 'tax-entity-type' argument to tax code getter functions
;; Updated txf data for individual tax returns to version 041 (from 039) and
;;    added tax-line year-effectivity data and code last-year data
;; Added asset and liability/equity tax code categories
;; Added version 041 txf data for Partnership, Corporation, S Corporation,
;;    tax entity types
;; Added 'None' type for no income tax options
;;
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

(define-module (gnucash locale es_MX tax txf))

(use-modules (gnucash engine))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (srfi srfi-2))
(use-modules (gnucash locale es_MX tax txf-help))

(export gnc:tax-type-txf-get-code-info)
(export gnc:txf-get-category-key)
(export gnc:txf-get-code-info)
(export gnc:txf-get-codes)
(export gnc:txf-get-description)
(export gnc:txf-get-form)
(export gnc:txf-get-format)
(export gnc:txf-get-help)
(export gnc:txf-get-last-year)
(export gnc:txf-get-line-data)
(export gnc:txf-get-multiple)
(export gnc:txf-get-payer-name-source)
(export gnc:txf-get-tax-entity-type)
(export gnc:txf-get-tax-entity-type-codes)
(export gnc:txf-get-tax-entity-type-description)
(export txf-asset-categories)
(export txf-expense-categories)
(export txf-help-categories)
(export txf-income-categories)
(export txf-liab-eq-categories)
(export txf-tax-entity-types)

(define txf-tax-entity-types
  (list
   (cons 'PFIS #("Declaración normal de la acividad fiscal" "Persona física con actividad empresarial"))
   (cons 'Other #("Nada" "Ninguna"))))

(define (gnc:tax-type-txf-get-code-info tax-entity-types type-code index)
  (and-let* ((tax-entity-type (assv-ref tax-entity-types type-code)))
    (vector-ref tax-entity-type index)))

(define (gnc:txf-get-tax-entity-type type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 0))

(define (gnc:txf-get-tax-entity-type-description type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 1))

(define (gnc:txf-get-tax-entity-type-codes)
  (map car txf-tax-entity-types))

(define (gnc:txf-get-payer-name-source categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 0 tax-entity-type))
(define (gnc:txf-get-form categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 1 tax-entity-type))
(define (gnc:txf-get-description categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 2 tax-entity-type))
(define (gnc:txf-get-format categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 3 tax-entity-type))
(define (gnc:txf-get-multiple categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 4 tax-entity-type))
(define (gnc:txf-get-category-key categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 5 tax-entity-type))

(define (gnc:txf-get-line-data categories code tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code))
             ((>= (vector-length category) 7)))
    (gnc:txf-get-code-info categories code 6 tax-entity-type)))

(define (gnc:txf-get-last-year categories code tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code))
             ((>= (vector-length category) 8)))
    (gnc:txf-get-code-info categories code 7 tax-entity-type)))

(define (gnc:txf-get-help categories code)
  (or (assv-ref txf-help-strings code)
      (G_ "No hay ayuda disponible")))

(define (gnc:txf-get-codes categories tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym)))
    (map car tax-entity-codes)))

(define (gnc:txf-get-code-info categories code index tax-entity-type)
  (and-let* ((sym (if (string-null? tax-entity-type)
                      'PFIS
                      (string->symbol tax-entity-type)))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code)))
    (vector-ref category index)))

(define txf-help-categories
  (list
   (cons 'H000 #(current "help" "Name of Current account is exported." 0 #f ""))
   (cons 'H002 #(parent "help" "Name of Parent account is exported." 0 #f ""))
   (cons 'H003 #(not-impl "help" "Not implemented yet, Do NOT Use!" 0 #f ""))))

(define txf-income-categories
 (list
  (cons 'PFIS
   (list
    (cons 'N000 #(none "" "Tax Report Only - No TXF Export" 0 #f ""))

    (cons 'N256 #(not-impl "PFIS" "Persona física" 1 #f ""))
    (cons 'N258 #(none "PFIS" "Ingresos propios de la actividad con extranjeros" 1 #f "Ingresos extranjeros" ))
    (cons 'N269 #(none "PFIS" "Ingresos propios de la actividad con nacionales" 1 #f "Ingresos nacionales" ))
   )
  )
 )
)

(define txf-expense-categories
 (list
  (cons 'PFIS
   (list
    (cons 'N000 #(none "" "Tax Report Only - No TXF Export" 0 #f ""))

    (cons 'N256 #(not-impl "PFIS" "Form 1040" 1 #f ""))
    (cons 'N680 #(none "PFIS" "Uso o goce temporal de bienes" 1 #f "" ))
    (cons 'N682 #(none "PFIS" "Intereses pagados sin ajuste algo e intereses moratorios" 1 #f "" ))
    (cons 'N683 #(none "PFIS" "Pérdidas por créditos incobrables" 1 #f "" ))
    (cons 'N681 #(none "PFIS" "Otros" 1 #f "" ))
   )
  )
 )
)

(define txf-asset-categories
 (list
  (cons 'PFIS
   (list
    (cons 'N000 #(none "" "Tax Report Only - No TXF Export" 0 #f ""))
   )
  )
 )
)

(define txf-liab-eq-categories
 (list
  (cons 'PFIS
   (list
    (cons 'N000 #(none "" "Tax Report Only - No TXF Export" 0 #f ""))
   )
  )
 )
)
