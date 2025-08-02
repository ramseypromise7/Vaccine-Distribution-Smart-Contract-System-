(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_FACILITY_NOT_FOUND (err u201))
(define-constant ERR_INVALID_PERIOD (err u202))

(define-data-var analytics-enabled bool true)

(define-map facility-inventory
  { facility: principal }
  {
    total-received: uint,
    total-administered: uint,
    current-stock: uint,
    last-updated: uint
  }
)

(define-map distribution-metrics
  { facility: principal, period: uint }
  {
    vaccines-received: uint,
    vaccines-administered: uint,
    distribution-rate: uint,
    efficiency-score: uint
  }
)

(define-map supply-alerts
  { facility: principal }
  {
    low-stock-threshold: uint,
    alert-active: bool,
    last-alert-date: uint,
    projected-depletion: uint
  }
)

(define-public (record-vaccine-receipt (facility principal) (quantity uint))
  (begin
    (asserts! (var-get analytics-enabled) (ok true))
    (let
      (
        (current-data (default-to 
          { total-received: u0, total-administered: u0, current-stock: u0, last-updated: u0 }
          (map-get? facility-inventory { facility: facility })
        ))
      )
      (map-set facility-inventory
        { facility: facility }
        {
          total-received: (+ (get total-received current-data) quantity),
          total-administered: (get total-administered current-data),
          current-stock: (+ (get current-stock current-data) quantity),
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

(define-public (record-vaccine-administration (facility principal) (quantity uint))
  (begin
    (asserts! (var-get analytics-enabled) (ok true))
    (let
      (
        (current-data (default-to 
          { total-received: u0, total-administered: u0, current-stock: u0, last-updated: u0 }
          (map-get? facility-inventory { facility: facility })
        ))
      )
      (map-set facility-inventory
        { facility: facility }
        {
          total-received: (get total-received current-data),
          total-administered: (+ (get total-administered current-data) quantity),
          current-stock: (if (>= (get current-stock current-data) quantity)
                           (- (get current-stock current-data) quantity)
                           u0),
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

(define-public (set-low-stock-alert (facility principal) (threshold uint))
  (begin
    (map-set supply-alerts
      { facility: facility }
      {
        low-stock-threshold: threshold,
        alert-active: false,
        last-alert-date: u0,
        projected-depletion: u0
      }
    )
    (ok true)
  )
)

(define-read-only (get-facility-inventory (facility principal))
  (map-get? facility-inventory { facility: facility })
)

(define-read-only (calculate-distribution-efficiency (facility principal))
  (let
    (
      (inventory-data (map-get? facility-inventory { facility: facility }))
    )
    (match inventory-data
      data (if (> (get total-received data) u0)
             (ok (/ (* (get total-administered data) u100) (get total-received data)))
             (ok u0))
      (ok u0)
    )
  )
)

(define-read-only (check-low-stock-alert (facility principal))
  (let
    (
      (inventory-data (map-get? facility-inventory { facility: facility }))
      (alert-settings (map-get? supply-alerts { facility: facility }))
    )
    (match (tuple inventory-data alert-settings)
      { inventory-data: (some inventory), alert-settings: (some settings) }
        (ok {
          alert-needed: (< (get current-stock inventory) (get low-stock-threshold settings)),
          current-stock: (get current-stock inventory),
          threshold: (get low-stock-threshold settings)
        })
      (ok { alert-needed: false, current-stock: u0, threshold: u0 })
    )
  )
)
