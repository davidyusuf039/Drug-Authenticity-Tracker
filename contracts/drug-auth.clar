
;; title: drug-auth

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))

(define-data-var next-batch-id uint u1)

(define-map drug-batches
    { batch-id: uint }
    {
        manufacturer: principal,
        drug-name: (string-ascii 64),
        production-date: uint,
        expiry-date: uint,
        current-holder: principal,
        status: (string-ascii 20)
    }
)

(define-map transfer-history
    { batch-id: uint, transfer-id: uint }
    {
        from: principal,
        to: principal,
        timestamp: uint,
        location: (string-ascii 64)
    }
)

(define-map batch-transfer-count
    { batch-id: uint }
    { count: uint }
)

(define-public (register-drug-batch 
    (drug-name (string-ascii 64))
    (production-date uint)
    (expiry-date uint))
    (let
        ((batch-id (var-get next-batch-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert drug-batches
            { batch-id: batch-id }
            {
                manufacturer: tx-sender,
                drug-name: drug-name,
                production-date: production-date,
                expiry-date: expiry-date,
                current-holder: tx-sender,
                status: "manufactured"
            }
        )
        (map-insert batch-transfer-count
            { batch-id: batch-id }
            { count: u0 }
        )
        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)
    )
)

(define-public (transfer-batch
    (batch-id uint)
    (recipient principal)
    (location (string-ascii 64)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (transfer-count (unwrap! (map-get? batch-transfer-count { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { 
                current-holder: recipient,
                status: "in-transit"
            })
        )
        (map-set transfer-history
            { 
                batch-id: batch-id,
                transfer-id: (get count transfer-count)
            }
            {
                from: tx-sender,
                to: recipient,
                timestamp: stacks-block-height,
                location: location
            }
        )
        (map-set batch-transfer-count
            { batch-id: batch-id }
            { count: (+ (get count transfer-count) u1) }
        )
        (ok true)
    )
)

(define-public (confirm-receipt
    (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "received" })
        )
        (ok true)
    )
)

(define-read-only (get-batch-details (batch-id uint))
    (map-get? drug-batches { batch-id: batch-id })
)

(define-read-only (get-transfer-history (batch-id uint) (transfer-id uint))
    (map-get? transfer-history { batch-id: batch-id, transfer-id: transfer-id })
)

(define-read-only (get-batch-transfer-count (batch-id uint))
    (map-get? batch-transfer-count { batch-id: batch-id })
)