
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


(define-constant err-invalid-batch (err u104))
(define-constant err-already-recalled (err u105))

(define-map recalled-batches
    { batch-id: uint }
    {
        recall-reason: (string-ascii 256),
        recall-date: uint,
        severity-level: (string-ascii 20)
    }
)

(define-public (recall-batch 
    (batch-id uint)
    (recall-reason (string-ascii 256))
    (severity-level (string-ascii 20)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? recalled-batches { batch-id: batch-id })) err-already-recalled)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "recalled" })
        )
        (map-set recalled-batches
            { batch-id: batch-id }
            {
                recall-reason: recall-reason,
                recall-date: stacks-block-height,
                severity-level: severity-level
            }
        )
        (ok true)
    )
)

(define-read-only (get-recall-details (batch-id uint))
    (map-get? recalled-batches { batch-id: batch-id })
)


(define-constant err-invalid-verification (err u106))

(define-map quality-verifications
    { batch-id: uint, verification-id: uint }
    {
        verifier: principal,
        timestamp: uint,
        location: (string-ascii 64),
        temperature: int,
        humidity: int,
        passed: bool
    }
)

(define-map batch-verification-count
    { batch-id: uint }
    { count: uint }
)

(define-public (add-quality-verification
    (batch-id uint)
    (location (string-ascii 64))
    (temperature int)
    (humidity int)
    (passed bool))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (verification-count (default-to { count: u0 } (map-get? batch-verification-count { batch-id: batch-id }))))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set quality-verifications
            {
                batch-id: batch-id,
                verification-id: (get count verification-count)
            }
            {
                verifier: tx-sender,
                timestamp: stacks-block-height,
                location: location,
                temperature: temperature,
                humidity: humidity,
                passed: passed
            }
        )
        (map-set batch-verification-count
            { batch-id: batch-id }
            { count: (+ (get count verification-count) u1) }
        )
        (ok true)
    )
)

(define-read-only (get-quality-verification 
    (batch-id uint)
    (verification-id uint))
    (map-get? quality-verifications { batch-id: batch-id, verification-id: verification-id })
)