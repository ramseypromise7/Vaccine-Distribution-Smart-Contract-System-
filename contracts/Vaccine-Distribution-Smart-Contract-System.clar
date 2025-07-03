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
