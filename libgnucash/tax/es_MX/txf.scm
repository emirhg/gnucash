;; -*-scheme-*-
;;  Author Emir Herrera González
;;  Heavily based on Richard -Gilligan- Uschold's work txf.scm
;;
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
;; Added 'Ninguno' type for no income tax options
;;
;; Bifurcado Ene 2023 para tropicalizar el código a la región mexicana
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
   (cons 'PFCAE #("Persona física con actividad empresarial" "Declaración de ingresos/egresos para personas físicas"))
   (cons 'Other #("Ninguno" "Sin opciones provistas para el reporte de impuestos a los ingresos"))
 )
)

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
      (G_ "No help available.")))

(define (gnc:txf-get-codes categories tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym)))
    (map car tax-entity-codes)))

(define (gnc:txf-get-code-info categories code index tax-entity-type)
  (and-let* ((sym (if (string-null? tax-entity-type)
                      'PFCAE
                      (string->symbol tax-entity-type)))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code)))
    (vector-ref category index)))

(define txf-help-categories
  (list
   (cons 'H000 #(current "ayuda" "El nombre de la cuenta actual es exportado" 0 #f ""))
   (cons 'H002 #(parent "ayuda" "El nombre de la cuenta padre es exportado" 0 #f ""))
   (cons 'H003 #(not-impl "ayuda" "No implementado, No usar!" 0 #f ""))))

(define txf-income-categories
 (list
  (cons 'PFCAE
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N200 #(current "Salario" "Ingreso acumulado" 3 #f "No sé que es esto" ((2022 "Ingreso")) ))
    (cons 'N201 #(current "Salario" "Aguinaldo" 0 #f "No sé que es esto" ((2022 "Ingreso excento")) ))
    (cons 'N202 #(current "Salario" "Prima vacacional" 0 #f "No sé que es esto" ((2022 "Ingreso excento")) ))
    (cons 'N203 #(current "Salario" "Otros ingresos excentos" 0 #f "No sé que es esto" ((2022 "Ingreso excento")) ))

    (cons 'N210 #(current "Salario" "Indemnización" 0 #f "No sé que es esto" ((2022 "Ingreso")) ))

    (cons 'N400 #(current "Honorarios" "Ingresos totales propios de la actividad nacionales" 1 #f "No sé que es esto" ((2010 "Actividades gravadas a la tasa del 16%")) ))
    (cons 'N401 #(current "Honorarios" "Ingresos totales propios de la actividad extranjeros" 1 #f "" ((2010 "Actividades gravadas a la tasa del 0%")) ))
    #|(cons 'N202 #(current "Ingresos" "Ingresos exclusivos por autotransporte de carga federal" 1 #f "" ((2010 "Actividades gravadas a la tasa del 0%")) ))|#
    #|(cons 'N203 #(current "Ingresos" "Anticipo de clientes" 1 #f "No sé que es esto" ((2010 "Actividades gravadas a la tasa del 16%")) ))|#
    #|(cons 'N204 #(current "Ingresos" "Ganancia en la enajenación de acciones o por reembolsos de cápital" 1 #f "" ((2010 "Actividades gravadas a la tasa del 0%")) ))|#
    #|(cons 'N205 #(current "Ingresos" "Ganancia en la enajenación de terrenos o activos fijos" 1 #f "" ((2010 "Actividades gravadas a la tasa del 0%")) ))|#
    ;(cons 'N206 #(current "Ingresos" "Íntereses cobrados sin ajuste alguno y ganancia cambiaria relacionados con actividades propias" 1 #f "" ((2010 "Actividades gravadas a la tasa del 16%")) ))
    #|(cons 'N207 #(current "Ingresos" "Servicios de transporte terrestres de pasajeros y transporte de bienes" 1 #f "" ((2010 "Actividades gravadas a la tasa del 0%")) ))|#
    #|(cons 'N208 #(current "Ingresos" "Servicios de hospedaje" 1 #f "" ((2010 "Actividades gravadas a la tasa del 16%")) ))|#
    (cons 'N409 #(current "Honorarios" "Enajenación de bienes y prestación de servicios" 1 #f "" ((2010 "Actividades gravadas a la tasa del 16%")) ))
    (cons 'N410 #(current "Honorarios" "Servicios profesionales (Honorarios)" 1 #f "" ((2010 "Actividades gravadas a la tasa del 16%")) ))

    #|(cons 'N400 #(current "Ingresos" "Actividades excentas" 3 #f "" ((2010 "Actividades excentas"))))|#


    (cons 'N900 #(current "Intereses" "Intereses nominales dentro del sistema financiero" 7 #f "" ((2022 "Intereses nóminales"))))
    (cons 'N901 #(current "Intereses" "Intereses nominales fuera del sistema financiero" 7 #f "" ((2022 "Intereses nóminales"))))

    #|(cons 'N900 #(none "NO-LIVA" "Bonos, descuentos y reembolsos" 1 #f "" ((2010 "Otros Ingresos")) ))|#
    #|(cons 'N901 #(none "NO-LIVA" "Regalos y donativos" 1 #f "" ((2010 "Otros Ingresos")) ))|#
    #|(cons 'N910 #(none "NO-LIVA" "Otras actividades excentas" 1 #f "" ((2010 "Otros Ingresos"))))|#
   )
  )
  (cons 'F1065
   (list
    (cons 'N000 #(parent "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
   )
  )
 )
)

(define txf-expense-categories
 (list
  (cons 'PFCAE
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
    ;(cons 'N680 #(parent "GASTOS" "Compras y gastos del periodo" 1 #f "" ((2020 "Compras y gastos del periodo") )))


    ; * Gastos
    (cons 'G230 #(parent "GASTOS" "Devoluciones, rebajas, descuentos y bonificaciones sobre ventas nacionales" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G231 #(parent "GASTOS" "Devoluciones, rebajas, descuentos y bonificaciones sobre ventas extranjeros" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G240 #(parent "GASTOS" "Adquicisiones netas de mercancías (compras) nacionales" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G241 #(parent "GASTOS" "Adquisiciones netas de mercancías (compras) extranjeros" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G250 #(parent "GASTOS" "Pérdidas por créditos incobrables" 1 #f "" ((2020 "Compras y gastos del periodo") )))

    ; ** Aportaciones al SAR e Infonavit y júbilaciones por vejez
    ; ** Cuotas al IMSS
    ; ** Sueldos salarios y conceptos asimilados y prestaciones a trabajadores
    ; ** Sueldos, salarios y prestaciones a trabajadores con discapacidad
    ; ** Sueldos, salarios y pretaciones a trabajadores adultos mayores
    ; ** Maniobras, empaques y fletes en el campo para la enajenacions de productos
    ; alimenticios
    ; ** Viáticos y gastos de viaje
    (cons 'G300 #(parent "GASTOS" "Interés pagados sin ajuste alguno e intereses moratorios" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G400 #(parent "GASTOS" "Consumo en restaurantes" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G401 #(parent "GASTOS" "Gastos" 1 #f "" ((2020 "Compras y gastos del periodo") )))

    (cons 'G500 #(parent "GASTOS" "Cuotas al IMSS" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    ; ** Honorarios
    ; ** Regalias y asistencia técnica
    ; ** Uso o goce temporal de bienes
    ; ** Seguros y finanzas
    ; ** Fletes y acarreo
    ; ** Combustibles y lubricantes
    ; ** Contribuciones pagadas, excepto IVA, ISR e IEPS
    ; ** Impuesto local sobre los ingresos por actividades empresariales
    ; ** Deducción de los pagos efectuados por el uso o goce temporal de
    ; automóviles
    ; ** Pagos efectuados por el uso o goce temporal de automóviles cuya propulsión
    ; sea através de baterías eléctricas recargables y automóviles eléctricos con
    ; combustión interna o accionados por hidrógeno
    ; ** Monto deducible al 47% (pagos que son ingresos excentos para el
    ; trabajador)
    ; ** Monto deducible al 53% (pagos que son ingresos excentos para el
    ; trabajador)
    ; ** Consumo en restaurantes
    (cons 'G350 #(parent "GASTOS" "Gasolina y mantenimiento de transporte" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    (cons 'G351 #(parent "GASTOS" "Deducción de mano de obra de trabajadores eventuales del campo, alimentación de ganado y gastos menores sin requisitos físcales" 1 #f "" ((2022 "Deducciones autorizadas") )))
    ; ** Deducción adicional por enajenación de libros, periódicos y revistas
    ; ** Deducción de donativos realizados a organismos descentralizados del
    ; Gobierno Federal
    (cons 'G680 #(parent "GASTOS" "Gastos" 1 #f "" ((2020 "Compras y gastos del periodo") )))
    ; ** Productos semiterminados o terminados
    ; ** Materias primas
    ; ** Transferencia de tecnología
    ; ** Gastos de viaje destinados al goce o uso temporal de automóviles
    ; ** Gastos de viaje destinados al hospedaje
    ; ** Pérdidas por caso fortuito o fuerza mayor
    ; ** Pérdidas derivadas de la enajenación
    ;
    ; * Estímulos
    ; ** Gastos realizados como consecuencia de desastres naturales
    ; ** Gastos realizados por adquisición de diesel marino especial
    ; ** Gastos realizados por adquisición de otro tipo de diésel
    ; ** Gasto realizados por uso de infraestructura carretera de cuota
    ; ** Gastos realizados por investigación de desarrollo tecnológico
    ; ** gastos realizados en la producción y distribución de cinematografía
    ; nacional
    ; ** gastos realizados en proyectos de inversión en las artes
    ; ** Reparaciones y adaptaciones en inmuebles considerados históricos en el
    ; centro histórico de la ciudad del contribuyente
    ; ** Estímulo por contratación de adultos mayores  y/o con discapacidad
    ;
    ; * Deducción físcal de inversiones
    ; ** Construcciones
    ; ** Máquinaria y equipo
    ; ** Mobiliario y equipo de oficina
    ; ** Automóviles, autobuses, camiones de carga, tractocamiones, montacargas y
    ; remolques
    ; ** Equipos de transporte otros
    ; ** 
    ;(cons 'G770 #(parent "Deducción físcal de inversiones" "Otras inversiones en activos fijos" 1 #f "" ((2022 "Deducciones autorizadas") )))
    ; ** Gastos, cargos diferidos y erogaciones en periodos preoperativos
    ; ** Adaptación a instalaciones para personas con capacidades diferentes
    ; ** Automóviles con propulsión de baterías eléctricas recargables y
    ; automóviles eléctricos con combustión interna o accionados por hidrógeno
    ; ** Bicicletas convencionales, bicicletas y motocicletas con propulsión de
    ; baterias eléctricas recargables** Equipos fijos de alimentación para
    ; vehículos eléctricos
    ; ** Automóviles
    ;(cons 'G780 #(parent "GASTOS" "Computadoras personales de escritorio y portátiles" 1 #f "" ((2022 "Deducciones autorizadas") )))
    ; **
    ; ** Inversiones de ejercicios anteriores
    ;
    ; * Adquisiciones en el ejercicio
    ; ** Construcciones
    ; ** Máquinaria y equipo
    ; ** Mobiliario y equipo de oficina
    ; ** Automóviles, autobuses, camiones de carga, tractocamiones, montacargas y
    ; remolques
    ; ** Equipos de transporte otros
    (cons 'G770 #(current "Adquisiciones en el ejercicio" "Otras inversiones en activos fijos" 1 #f "" ((2022 "Deducciones autorizadas") )))
    ; ** Otras inversiones en activos fijos
    ; ** Gastos, cargos diferidos y erogaciones en periodos preoperativos
    ; ** Adaptación a instalaciones para personas con capacidades diferentes
    ; ** Automóviles con propulsión de baterías eléctricas recargables y
    ; automóviles eléctricos con combustión interna o accionados por hidrógeno
    ; ** Bicicletas convencionales, bicicletas y motocicletas con propulsión de
    ; baterias eléctricas recargables
    ; ** Equipos fijos de alimentación para vehículos eléctricos
    ; ** Automóviles
    ; ** Computadoras personales de escritorio y portátiles
    (cons 'G780 #(current "Adquisiciones en el ejercicio" "Computadoras personales de escritorio y portátiles" 1 #f "" ((2022 "Deducciones autorizadas") )))
    ; ** Inversiones de ejercicios anteriores


    (cons 'N900 #(current "Intereses" "Pérdida" 7 #f "" ((2022 "Pérdida"))))
    ;(cons 'N256 #(not-impl "PFCAE" "Form 1040" 1 #f ""))
    ;(cons 'N681 #(none "PFCAE" "Participación de los trabajadores en las utilidades" 1 #f "" ((2019 "Schedule 1, 10") (2018 "Schedule 1, 23") (2007 "23") (2006 "NA - Expired") (2002 "23"))))
   )
  )
  (cons 'F1065
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1256 #(not-impl "F1065" "Form F1065" 1 #f ""))
    (cons 'N680 #(none "F1065" "Compras y gastos del periodo" 1 #f "" ))
    (cons 'N683 #(none "F1065" "Deducción de inversiones de ejercicios anteriores" 1 #f "" ))
    (cons 'N780 #(none "F1065" "IVA Acreditable del periodo" 1 #f "" ))
   )
  )
 )
)

(define txf-asset-categories
 (list
  (cons 'PFCAE
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
    ;(cons 'N440 #(none "F8606" "IRA basis at beg of year" 1 #t "" ((1993 "2") (1988 "3"))))
    ;(cons 'N438 #(none "F8606" "IRAs value at end of year" 1 #t "" ((1993 "6") (1989 "1") (1988 "11") (1987 "8"))))
    (cons 'N400 #(current "ISR" "ISR Retenido" 0 #f ""))
    (cons 'N401 #(current "IVA" "IVA Acreditable del periodo" 0 #f "" ((2020 "IVA a favor"))))
    (cons 'N402 #(current "IVA" "IVA Retenido" 0 #f ""))
   )
  )
  (cons 'F1065
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1864 #(none "F1065" "Cash" 1 #f "" ((1990 "L1"))))
    (cons 'N1865 #(none "F1065" "Accts. Rec. and trade notes" 1 #f "" ((1990 "L2a"))))
    (cons 'N1866 #(none "F1065" "Allowance for bad debts" 1 #f "" ((1990 "L2b"))))
    (cons 'N1868 #(none "F1065" "U.S. government obligations" 1 #f "" ((1990 "L4"))))
    (cons 'N1869 #(none "F1065" "Tax-exempt securities" 1 #f "" ((1990 "L5"))))
    (cons 'N1870 #(none "F1065" "Other current assets" 1 #f "" ((1990 "L6"))))
    (cons 'N1871 #(none "F1065" "Mortgage/real estate loans" 1 #f "" ((2011 "L7b")(1990 "L7"))))
    (cons 'N1872 #(none "F1065" "Other investments" 1 #f "" ((1990 "L8"))))
    (cons 'N1873 #(none "F1065" "Buildings/oth. depr. assets" 1 #f "" ((1990 "L9a"))))
    (cons 'N1874 #(none "F1065" "Accumulated depreciation" 1 #f "" ((1990 "L9b"))))
    (cons 'N1875 #(none "F1065" "Depletable assets" 1 #f "" ((1990 "L10a"))))
    (cons 'N1876 #(none "F1065" "Accumulated depletion" 1 #f "" ((1990 "L10b"))))
    (cons 'N1877 #(none "F1065" "Land" 1 #f "" ((1990 "L11"))))
    (cons 'N1878 #(none "F1065" "Intangible assets" 1 #f "" ((1990 "L12a"))))
    (cons 'N1879 #(none "F1065" "Accumulated amortization" 1 #f "" ((1990 "L12b"))))
    (cons 'N1880 #(none "F1065" "Other assets" 1 #f "" ((1990 "L13"))))
   )
  )
  (cons 'F1120
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1172 #(none "F1120" "Cash" 1 #f "" ((1990 "L1"))))
    (cons 'N1174 #(none "F1120" "Accts. Rec. and trade notes." 1 #f "" ((1990 "L2a"))))
    (cons 'N1176 #(none "F1120" "Allowance for bad debts" 1 #f "" ((1990 "L2b"))))
    (cons 'N1180 #(none "F1120" "U.S. government obligations" 1 #f "" ((1990 "L4"))))
    (cons 'N1182 #(none "F1120" "Tax-exempt securities" 1 #f "" ((1990 "L5"))))
    (cons 'N1184 #(none "F1120" "Other current assets" 1 #f "" ((1990 "L6"))))
    (cons 'N1186 #(none "F1120" "Loans to stockholders" 1 #f "" ((1990 "L7"))))
    (cons 'N1188 #(none "F1120" "Mortgage/real estate loans" 1 #f "" ((1990 "L8"))))
    (cons 'N1190 #(none "F1120" "Other investments" 1 #f "" ((1990 "L9"))))
    (cons 'N1192 #(none "F1120" "Buildings/oth. depr. assets" 1 #f "" ((1990 "L10a"))))
    (cons 'N1194 #(none "F1120" "Accumulated depreciation" 1 #f "" ((1990 "L10b"))))
    (cons 'N1196 #(none "F1120" "Depletable assets" 1 #f "" ((1990 "L11a"))))
    (cons 'N1198 #(none "F1120" "Accumulated depletion" 1 #f "" ((1990 "L11b"))))
    (cons 'N1200 #(none "F1120" "Land" 1 #f "" ((1990 "L12"))))
    (cons 'N1202 #(none "F1120" "Intangible assets" 1 #f "" ((1990 "L13a"))))
    (cons 'N1204 #(none "F1120" "Accumulated amortization" 1 #f "" ((1990 "L13b"))))
    (cons 'N1206 #(none "F1120" "Other assets" 1 #f "" ((1990 "L14"))))
   )
  )
  (cons 'F1120S
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1535 #(none "F1120S" "Cash" 1 #f "" ((1990 "L1"))))
    (cons 'N1537 #(none "F1120S" "Accts. Rec. and trade notes" 1 #f "" ((1990 "L2a"))))
    (cons 'N1539 #(none "F1120S" "Allowance for bad debts" 1 #f "" ((1990 "L2b"))))
    (cons 'N1543 #(none "F1120S" "U.S. government obligations" 1 #f "" ((1990 "L4"))))
    (cons 'N1545 #(none "F1120S" "Tax-exempt securities" 1 #f "" ((1990 "L5"))))
    (cons 'N1547 #(none "F1120S" "Other current assets" 1 #f "" ((1990 "L6"))))
    (cons 'N1549 #(none "F1120S" "Loans to shareholders" 1 #f "" ((1990 "L7"))))
    (cons 'N1551 #(none "F1120S" "Mortgage/real estate loans" 1 #f "" ((1990 "L8"))))
    (cons 'N1553 #(none "F1120S" "Other investments" 1 #f "" ((1990 "L9"))))
    (cons 'N1555 #(none "F1120S" "Buildings/oth. depr. assets" 1 #f "" ((1990 "L10a"))))
    (cons 'N1557 #(none "F1120S" "Accumulated depreciation" 1 #f "" ((1990 "L10b"))))
    (cons 'N1559 #(none "F1120S" "Depletable assets" 1 #f "" ((1990 "L11a"))))
    (cons 'N1561 #(none "F1120S" "Accumulated depletion" 1 #f "" ((1990 "L11b"))))
    (cons 'N1563 #(none "F1120S" "Land" 1 #f "" ((1990 "L12"))))
    (cons 'N1565 #(none "F1120S" "Intangible assets" 1 #f "" ((1990 "L13a"))))
    (cons 'N1567 #(none "F1120S" "Accumulated amortization" 1 #f "" ((1990 "L13b"))))
    (cons 'N1569 #(none "F1120S" "Other assets" 1 #f "" ((1990 "L14"))))
   )
  )
  (cons 'Other
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
   )
  )
 )
)

(define txf-liab-eq-categories
 (list
  (cons 'PFCAE
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
    (cons 'L1000 #(parent "IVA" "IVA cobrado" 0 #f ""))
   )
  )
  (cons 'F1065
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1884 #(none "F1065" "Accounts payable" 1 #f "" ((1990 "L15"))))
    (cons 'N1886 #(none "F1065" "S-T Mortgage/note/bonds pay." 1 #f "" ((1990 "L16"))))
    (cons 'N1888 #(none "F1065" "Other current liabilities" 1 #f "" ((1990 "L17"))))
    (cons 'N1890 #(none "F1065" "All nonrecourse loans" 1 #f "" ((1990 "L18"))))
    (cons 'N1892 #(none "F1065" "L-T Mortgage/note/bonds pay." 1 #f "" ((2011 "L19b") (1990 "L19"))))
    (cons 'N1894 #(none "F1065" "Other liabilities" 1 #f "" ((1990 "L20"))))
   )
  )
  (cons 'F1120
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1209 #(none "F1120" "Accounts payable" 1 #f "" ((1990 "L16"))))
    (cons 'N1211 #(none "F1120" "S-T Mortgage/note/bonds pay." 1 #f "" ((1990 "L17"))))
    (cons 'N1213 #(none "F1120" "Other current liabilities" 1 #f "" ((1990 "L18"))))
    (cons 'N1215 #(none "F1120" "Loans from stockholders" 1 #f "" ((1990 "L19"))))
    (cons 'N1217 #(none "F1120" "L-T Mortgage/note/bonds pay." 1 #f "" ((1990 "L20"))))
    (cons 'N1219 #(none "F1120" "Other liabilities" 1 #f "" ((1990 "L21"))))
    (cons 'N1221 #(none "F1120" "Capital Stock - Preferred Stk." 1 #f "" ((1990 "L22a"))))
    (cons 'N1223 #(none "F1120" "Capital Stock - Common Stk." 1 #f "" ((1990 "L22b"))))
    (cons 'N1225 #(none "F1120" "Paid-in or capital surplus" 1 #f "" ((1990 "L23"))))
    (cons 'N1231 #(none "F1120" "Cost of Treasury stock" 1 #f "" ((1997 "L27") (1990 "L26"))))
   )
  )
  (cons 'F1120S
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))

    (cons 'N1573 #(none "F1120S" "Accounts payable" 1 #f "" ((1990 "L16"))))
    (cons 'N1575 #(none "F1120S" "S-T Mortgage/note/bonds pay." 1 #f "" ((1990 "L17"))))
    (cons 'N1577 #(none "F1120S" "Other current liabilities" 1 #f "" ((1990 "L18"))))
    (cons 'N1579 #(none "F1120S" "Loans from shareholders" 1 #f "" ((1990 "L19"))))
    (cons 'N1581 #(none "F1120S" "L-T Mortgage/note/bonds pay." 1 #f "" ((1990 "L20"))))
    (cons 'N1583 #(none "F1120S" "Other liabilities" 1 #f "" ((1990 "L21"))))
    (cons 'N1585 #(none "F1120S" "Capital stock" 1 #f "" ((1992 "L22") (1990 "L22a"))))
    (cons 'N1587 #(none "F1120S" "Paid-in or capital surplus" 1 #f "" ((1990 "L23"))))
    (cons 'N1591 #(none "F1120S" "Treasury stock" 1 #f "" ((1997 "L26") (1990 "L25"))))
   )
  )
  (cons 'Other
   (list
    (cons 'N000 #(none "" "Sólo reporte de impuestos - No exportar TXF" 0 #f ""))
   )
  )
 )
)