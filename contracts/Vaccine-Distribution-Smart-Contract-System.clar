(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VACCINE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_TEMPERATURE (err u102))
(define-constant ERR_VACCINE_EXPIRED (err u103))
(define-constant ERR_VACCINE_ALREADY_USED (err u104))
(define-constant ERR_INVALID_STATUS (err u105))
(define-constant ERR_FACILITY_NOT_AUTHORIZED (err u106))
(define-constant ERR_BATCH_ALREADY_RECALLED (err u107))
(define-constant ERR_BATCH_NOT_RECALLED (err u108))

(define-data-var next-recall-id uint u1)

(define-data-var next-vaccine-id uint u1)
(define-data-var next-batch-id uint u1)

(define-map vaccines
  { vaccine-id: uint }
  {
    batch-id: uint,
    manufacturer: principal,
    vaccine-type: (string-ascii 50),
    production-date: uint,
    expiry-date: uint,
    current-temperature: int,
    min-temp: int,
    max-temp: int,
    current-location: (string-ascii 100),
    status: (string-ascii 20),
    administered-to: (optional principal),
    administered-date: (optional uint),
    cold-chain-violations: uint
  }
)

(define-map batches
  { batch-id: uint }
  {
    manufacturer: principal,
    vaccine-type: (string-ascii 50),
    quantity: uint,
    production-date: uint,
    expiry-date: uint,
    min-temp: int,
    max-temp: int
  }
)

(define-map authorized-facilities
  { facility: principal }
  {
    name: (string-ascii 100),
    facility-type: (string-ascii 50),
    authorized: bool
  }
)

(define-map temperature-logs
  { vaccine-id: uint, timestamp: uint }
  {
    temperature: int,
    location: (string-ascii 100),
    recorded-by: principal
  }
)

(define-map facility-permissions
  { facility: principal }
  { can-manufacture: bool, can-distribute: bool, can-administer: bool }
)

(define-public (authorize-facility (facility principal) (name (string-ascii 100)) (facility-type (string-ascii 50)) (can-manufacture bool) (can-distribute bool) (can-administer bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-facilities
      { facility: facility }
      { name: name, facility-type: facility-type, authorized: true }
    )
    (map-set facility-permissions
      { facility: facility }
      { can-manufacture: can-manufacture, can-distribute: can-distribute, can-administer: can-administer }
    )
    (ok true)
  )
)

(define-public (create-batch (vaccine-type (string-ascii 50)) (quantity uint) (expiry-date uint) (min-temp int) (max-temp int))
  (let
    (
      (batch-id (var-get next-batch-id))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
    )
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-manufacture (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (map-set batches
      { batch-id: batch-id }
      {
        manufacturer: tx-sender,
        vaccine-type: vaccine-type,
        quantity: quantity,
        production-date: stacks-block-height,
        expiry-date: expiry-date,
        min-temp: min-temp,
        max-temp: max-temp
      }
    )
    (var-set next-batch-id (+ batch-id u1))
    (ok batch-id)
  )
)

(define-public (produce-vaccine (batch-id uint) (initial-location (string-ascii 100)) (initial-temp int))
  (let
    (
      (vaccine-id (var-get next-vaccine-id))
      (batch-data (map-get? batches { batch-id: batch-id }))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
    )
    (asserts! (is-some batch-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-manufacture (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (let
      (
        (batch (unwrap-panic batch-data))
      )
      (asserts! (is-eq (get manufacturer batch) tx-sender) ERR_UNAUTHORIZED)
      (asserts! (and (>= initial-temp (get min-temp batch)) (<= initial-temp (get max-temp batch))) ERR_INVALID_TEMPERATURE)
      (map-set vaccines
        { vaccine-id: vaccine-id }
        {
          batch-id: batch-id,
          manufacturer: (get manufacturer batch),
          vaccine-type: (get vaccine-type batch),
          production-date: (get production-date batch),
          expiry-date: (get expiry-date batch),
          current-temperature: initial-temp,
          min-temp: (get min-temp batch),
          max-temp: (get max-temp batch),
          current-location: initial-location,
          status: "produced",
          administered-to: none,
          administered-date: none,
          cold-chain-violations: u0
        }
      )
      (map-set temperature-logs
        { vaccine-id: vaccine-id, timestamp: stacks-block-height }
        { temperature: initial-temp, location: initial-location, recorded-by: tx-sender }
      )
      (var-set next-vaccine-id (+ vaccine-id u1))
      (ok vaccine-id)
    )
  )
)

(define-public (update-temperature (vaccine-id uint) (new-temp int) (location (string-ascii 100)))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
    )
    (asserts! (is-some vaccine-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-distribute (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (let
      (
        (vaccine (unwrap-panic vaccine-data))
        (temp-violation (if (or (< new-temp (get min-temp vaccine)) (> new-temp (get max-temp vaccine))) u1 u0))
      )
      (map-set vaccines
        { vaccine-id: vaccine-id }
        (merge vaccine {
          current-temperature: new-temp,
          current-location: location,
          cold-chain-violations: (+ (get cold-chain-violations vaccine) temp-violation)
        })
      )
      (map-set temperature-logs
        { vaccine-id: vaccine-id, timestamp: stacks-block-height }
        { temperature: new-temp, location: location, recorded-by: tx-sender }
      )
      (ok true)
    )
  )
)

(define-public (transfer-vaccine (vaccine-id uint) (to-facility principal) (new-location (string-ascii 100)))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
      (recipient-perms (map-get? facility-permissions { facility: to-facility }))
    )
    (asserts! (is-some vaccine-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (is-some recipient-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-distribute (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (let
      (
        (vaccine (unwrap-panic vaccine-data))
      )
      (asserts! (< stacks-block-height (get expiry-date vaccine)) ERR_VACCINE_EXPIRED)
      (asserts! (not (is-eq (get status vaccine) "administered")) ERR_VACCINE_ALREADY_USED)
      (map-set vaccines
        { vaccine-id: vaccine-id }
        (merge vaccine {
          current-location: new-location,
          status: "in-transit"
        })
      )
      (ok true)
    )
  )
)

(define-public (administer-vaccine (vaccine-id uint) (patient principal))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
    )
    (asserts! (is-some vaccine-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-administer (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (let
      (
        (vaccine (unwrap-panic vaccine-data))
      )
      (asserts! (< stacks-block-height (get expiry-date vaccine)) ERR_VACCINE_EXPIRED)
      (asserts! (not (is-eq (get status vaccine) "administered")) ERR_VACCINE_ALREADY_USED)
      (asserts! (is-eq (get cold-chain-violations vaccine) u0) ERR_INVALID_TEMPERATURE)
      (map-set vaccines
        { vaccine-id: vaccine-id }
        (merge vaccine {
          status: "administered",
          administered-to: (some patient),
          administered-date: (some stacks-block-height)
        })
      )
      (ok true)
    )
  )
)

(define-read-only (get-vaccine (vaccine-id uint))
  (map-get? vaccines { vaccine-id: vaccine-id })
)

(define-read-only (get-batch (batch-id uint))
  (map-get? batches { batch-id: batch-id })
)

(define-read-only (get-facility-info (facility principal))
  (map-get? authorized-facilities { facility: facility })
)

(define-read-only (get-facility-permissions (facility principal))
  (map-get? facility-permissions { facility: facility })
)

(define-read-only (get-temperature-log (vaccine-id uint) (timestamp uint))
  (map-get? temperature-logs { vaccine-id: vaccine-id, timestamp: timestamp })
)

(define-read-only (verify-cold-chain (vaccine-id uint))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
    )
    (match vaccine-data
      vaccine (ok {
        vaccine-id: vaccine-id,
        cold-chain-maintained: (is-eq (get cold-chain-violations vaccine) u0),
        violations: (get cold-chain-violations vaccine),
        current-temp: (get current-temperature vaccine),
        temp-range: { min: (get min-temp vaccine), max: (get max-temp vaccine) }
      })
      ERR_VACCINE_NOT_FOUND
    )
  )
)

(define-read-only (get-vaccine-history (vaccine-id uint))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
    )
    (match vaccine-data
      vaccine (ok {
        vaccine-id: vaccine-id,
        batch-id: (get batch-id vaccine),
        manufacturer: (get manufacturer vaccine),
        vaccine-type: (get vaccine-type vaccine),
        production-date: (get production-date vaccine),
        expiry-date: (get expiry-date vaccine),
        current-location: (get current-location vaccine),
        status: (get status vaccine),
        administered-to: (get administered-to vaccine),
        administered-date: (get administered-date vaccine),
        cold-chain-violations: (get cold-chain-violations vaccine)
      })
      ERR_VACCINE_NOT_FOUND
    )
  )
)

(define-map batch-recalls
  { batch-id: uint }
  {
    recall-id: uint,
    recalled-by: principal,
    recall-date: uint,
    reason: (string-ascii 200),
    severity: (string-ascii 20),
    active: bool
  }
)

(define-map recall-registry
  { recall-id: uint }
  {
    batch-id: uint,
    recalled-by: principal,
    recall-date: uint,
    reason: (string-ascii 200),
    severity: (string-ascii 20),
    affected-vaccines: uint,
    administered-count: uint
  }
)

(define-public (issue-batch-recall (batch-id uint) (reason (string-ascii 200)) (severity (string-ascii 20)))
  (let
    (
      (recall-id (var-get next-recall-id))
      (batch-data (map-get? batches { batch-id: batch-id }))
      (facility-perms (map-get? facility-permissions { facility: tx-sender }))
      (existing-recall (map-get? batch-recalls { batch-id: batch-id }))
    )
    (asserts! (is-some batch-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some facility-perms) ERR_FACILITY_NOT_AUTHORIZED)
    (asserts! (get can-manufacture (unwrap-panic facility-perms)) ERR_UNAUTHORIZED)
    (asserts! (is-none existing-recall) ERR_BATCH_ALREADY_RECALLED)
    (map-set batch-recalls
      { batch-id: batch-id }
      {
        recall-id: recall-id,
        recalled-by: tx-sender,
        recall-date: stacks-block-height,
        reason: reason,
        severity: severity,
        active: true
      }
    )
    (map-set recall-registry
      { recall-id: recall-id }
      {
        batch-id: batch-id,
        recalled-by: tx-sender,
        recall-date: stacks-block-height,
        reason: reason,
        severity: severity,
        affected-vaccines: u0,
        administered-count: u0
      }
    )
    (var-set next-recall-id (+ recall-id u1))
    (ok recall-id)
  )
)

(define-read-only (get-batch-recall-status (batch-id uint))
  (map-get? batch-recalls { batch-id: batch-id })
)

(define-read-only (get-recall-info (recall-id uint))
  (map-get? recall-registry { recall-id: recall-id })
)

(define-read-only (is-vaccine-recalled (vaccine-id uint))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
    )
    (match vaccine-data
      vaccine (is-some (map-get? batch-recalls { batch-id: (get batch-id vaccine) }))
      false
    )
  )
)

(define-constant ERR_ALERT_ALREADY_EXISTS (err u109))
(define-constant ERR_ALERT_NOT_FOUND (err u110))
(define-constant ERR_INVALID_THRESHOLD (err u111))

(define-data-var next-alert-id uint u1)

(define-map expiration-alerts
  { alert-id: uint }
  {
    vaccine-id: uint,
    batch-id: uint,
    expiry-date: uint,
    alert-level: (string-ascii 20),
    threshold-blocks: uint,
    created-at: uint,
    acknowledged: bool,
    current-facility: principal
  }
)

(define-map alert-thresholds
  { level: (string-ascii 20) }
  { blocks-before-expiry: uint, description: (string-ascii 100) }
)

(define-public (set-alert-threshold (level (string-ascii 20)) (blocks-before-expiry uint) (description (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> blocks-before-expiry u0) ERR_INVALID_THRESHOLD)
    (map-set alert-thresholds
      { level: level }
      { blocks-before-expiry: blocks-before-expiry, description: description }
    )
    (ok true)
  )
)

(define-public (check-vaccine-expiration (vaccine-id uint))
  (let
    (
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
      (current-block stacks-block-height)
    )
    (asserts! (is-some vaccine-data) ERR_VACCINE_NOT_FOUND)
    (let
      (
        (vaccine (unwrap-panic vaccine-data))
        (expiry-date (get expiry-date vaccine))
        (blocks-until-expiry (- expiry-date current-block))
      )
      (if (and (<= blocks-until-expiry u1000) (not (is-eq (get status vaccine) "administered")))
        (create-expiration-alert vaccine-id vaccine blocks-until-expiry)
        (ok u0)
      )
    )
  )
)

(define-private (create-expiration-alert (vaccine-id uint) (vaccine-data { batch-id: uint, manufacturer: principal, vaccine-type: (string-ascii 50), production-date: uint, expiry-date: uint, current-temperature: int, min-temp: int, max-temp: int, current-location: (string-ascii 100), status: (string-ascii 20), administered-to: (optional principal), administered-date: (optional uint), cold-chain-violations: uint }) (blocks-until-expiry uint))
  (let
    (
      (alert-id (var-get next-alert-id))
      (alert-level (determine-alert-level blocks-until-expiry))
    )
    (map-set expiration-alerts
      { alert-id: alert-id }
      {
        vaccine-id: vaccine-id,
        batch-id: (get batch-id vaccine-data),
        expiry-date: (get expiry-date vaccine-data),
        alert-level: alert-level,
        threshold-blocks: blocks-until-expiry,
        created-at: stacks-block-height,
        acknowledged: false,
        current-facility: (get manufacturer vaccine-data)
      }
    )
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

(define-private (determine-alert-level (blocks-until-expiry uint))
  (if (<= blocks-until-expiry u144)
    "critical"
    (if (<= blocks-until-expiry u720)
      "warning"
      "notice"
    )
  )
)

(define-public (acknowledge-alert (alert-id uint))
  (let
    (
      (alert-data (map-get? expiration-alerts { alert-id: alert-id }))
    )
    (asserts! (is-some alert-data) ERR_ALERT_NOT_FOUND)
    (map-set expiration-alerts
      { alert-id: alert-id }
      (merge (unwrap-panic alert-data) { acknowledged: true })
    )
    (ok true)
  )
)

(define-read-only (get-expiration-alert (alert-id uint))
  (map-get? expiration-alerts { alert-id: alert-id })
)

(define-read-only (get-alert-threshold (level (string-ascii 20)))
  (map-get? alert-thresholds { level: level })
)

(define-constant ERR_INVALID_EMISSION_DATA (err u112))
(define-constant ERR_CARBON_REPORT_NOT_FOUND (err u113))

(define-data-var next-carbon-report-id uint u1)

(define-map carbon-emission-factors
  { activity-type: (string-ascii 30) }
  { 
    co2-per-unit: uint,
    unit-description: (string-ascii 50)
  }
)

(define-map vaccine-carbon-footprint
  { vaccine-id: uint }
  {
    total-emissions: uint,
    transport-emissions: uint,
    storage-emissions: uint,
    refrigeration-emissions: uint,
    last-updated: uint
  }
)

(define-map carbon-activity-log
  { vaccine-id: uint, activity-id: uint }
  {
    activity-type: (string-ascii 30),
    distance-km: uint,
    duration-hours: uint,
    emissions-generated: uint,
    facility: principal,
    timestamp: uint
  }
)

(define-public (initialize-emission-factors)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set carbon-emission-factors { activity-type: "transport-truck" } { co2-per-unit: u162, unit-description: "grams CO2 per km" })
    (map-set carbon-emission-factors { activity-type: "transport-air" } { co2-per-unit: u500, unit-description: "grams CO2 per km" })
    (map-set carbon-emission-factors { activity-type: "cold-storage" } { co2-per-unit: u45, unit-description: "grams CO2 per hour" })
    (map-set carbon-emission-factors { activity-type: "ultra-cold" } { co2-per-unit: u120, unit-description: "grams CO2 per hour" })
    (ok true)
  )
)

(define-public (track-carbon-activity (vaccine-id uint) (activity-type (string-ascii 30)) (distance-km uint) (duration-hours uint))
  (let
    (
      (activity-id (var-get next-carbon-report-id))
      (emission-factor (map-get? carbon-emission-factors { activity-type: activity-type }))
      (vaccine-data (map-get? vaccines { vaccine-id: vaccine-id }))
      (current-footprint (default-to { total-emissions: u0, transport-emissions: u0, storage-emissions: u0, refrigeration-emissions: u0, last-updated: u0 }
                                     (map-get? vaccine-carbon-footprint { vaccine-id: vaccine-id })))
    )
    (asserts! (is-some vaccine-data) ERR_VACCINE_NOT_FOUND)
    (asserts! (is-some emission-factor) ERR_INVALID_EMISSION_DATA)
    (let
      (
        (factor (unwrap-panic emission-factor))
        (emissions (if (> distance-km u0) 
                     (* distance-km (get co2-per-unit factor))
                     (* duration-hours (get co2-per-unit factor))))
        (activity-category (if (is-eq activity-type "transport-truck") "transport"
                             (if (is-eq activity-type "transport-air") "transport" "storage")))
      )
      (map-set carbon-activity-log
        { vaccine-id: vaccine-id, activity-id: activity-id }
        {
          activity-type: activity-type,
          distance-km: distance-km,
          duration-hours: duration-hours,
          emissions-generated: emissions,
          facility: tx-sender,
          timestamp: stacks-block-height
        }
      )
      (map-set vaccine-carbon-footprint
        { vaccine-id: vaccine-id }
        {
          total-emissions: (+ (get total-emissions current-footprint) emissions),
          transport-emissions: (+ (get transport-emissions current-footprint) 
                                  (if (is-eq activity-category "transport") emissions u0)),
          storage-emissions: (+ (get storage-emissions current-footprint) 
                                (if (is-eq activity-category "storage") emissions u0)),
          refrigeration-emissions: (+ (get refrigeration-emissions current-footprint)
                                     (if (is-eq activity-type "ultra-cold") emissions u0)),
          last-updated: stacks-block-height
        }
      )
      (var-set next-carbon-report-id (+ activity-id u1))
      (ok activity-id)
    )
  )
)

(define-read-only (get-vaccine-carbon-footprint (vaccine-id uint))
  (map-get? vaccine-carbon-footprint { vaccine-id: vaccine-id })
)