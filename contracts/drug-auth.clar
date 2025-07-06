
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

(define-constant err-already-expired (err u107))
(define-constant err-invalid-alert-period (err u108))

(define-map expiry-alerts
    { batch-id: uint }
    {
        alert-before-days: uint,
        alert-set-by: principal,
        alert-timestamp: uint
    }
)

(define-map expired-batches
    { batch-id: uint }
    {
        expired-timestamp: uint,
        marked-by: principal
    }
)

(define-public (set-expiry-alert
    (batch-id uint)
    (alert-before-days uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (asserts! (> alert-before-days u0) err-invalid-alert-period)
        (map-set expiry-alerts
            { batch-id: batch-id }
            {
                alert-before-days: alert-before-days,
                alert-set-by: tx-sender,
                alert-timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (mark-batch-expired
    (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (>= stacks-block-height (get expiry-date batch)) err-invalid-status)
        (asserts! (is-none (map-get? expired-batches { batch-id: batch-id })) err-already-expired)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "expired" })
        )
        (map-set expired-batches
            { batch-id: batch-id }
            {
                expired-timestamp: stacks-block-height,
                marked-by: tx-sender
            }
        )
        (ok true)
    )
)

(define-read-only (is-batch-expired (batch-id uint))
    (match (map-get? drug-batches { batch-id: batch-id })
        batch (>= stacks-block-height (get expiry-date batch))
        false
    )
)

(define-read-only (is-batch-expiring-soon (batch-id uint))
    (match (map-get? drug-batches { batch-id: batch-id })
        batch (match (map-get? expiry-alerts { batch-id: batch-id })
            alert (let
                ((alert-threshold (- (get expiry-date batch) (get alert-before-days alert))))
                (and 
                    (>= stacks-block-height alert-threshold)
                    (< stacks-block-height (get expiry-date batch))
                )
            )
            false
        )
        false
    )
)

(define-read-only (get-expiry-alert (batch-id uint))
    (map-get? expiry-alerts { batch-id: batch-id })
)

(define-read-only (get-expired-batch-details (batch-id uint))
    (map-get? expired-batches { batch-id: batch-id })
)

(define-read-only (get-batch-expiry-status (batch-id uint))
    (let
        ((is-expired (is-batch-expired batch-id))
         (is-expiring-soon (is-batch-expiring-soon batch-id)))
        (if is-expired
            "expired"
            (if is-expiring-soon
                "expiring-soon"
                "valid"
            )
        )
    )
)

(define-constant err-invalid-score (err u109))
(define-constant max-score u1000)
(define-constant base-score u500)

(define-map holder-reputation
    { holder: principal }
    {
        successful-transfers: uint,
        failed-verifications: uint,
        total-batches-handled: uint,
        reputation-score: uint
    }
)

(define-map batch-authenticity-score
    { batch-id: uint }
    {
        current-score: uint,
        last-updated: uint,
        factors: {
            quality-score: uint,
            transfer-score: uint,
            holder-score: uint
        }
    }
)

(define-public (calculate-batch-score (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (verification-count (default-to { count: u0 } (map-get? batch-verification-count { batch-id: batch-id })))
         (transfer-count (default-to { count: u0 } (map-get? batch-transfer-count { batch-id: batch-id })))
         (holder-reputation-score (match (map-get? holder-reputation { holder: (get current-holder batch) })
            reputation (get reputation-score reputation)
            base-score))
         (quality-score (calculate-quality-score batch-id (get count verification-count)))
         (transfer-score (calculate-transfer-score (get count transfer-count)))
         (holder-score (if (< holder-reputation-score u300) holder-reputation-score u300)))
        (let
            ((total-score (if (< (+ quality-score transfer-score holder-score) max-score) (+ quality-score transfer-score holder-score) max-score)))
            (map-set batch-authenticity-score
                { batch-id: batch-id }
                {
                    current-score: total-score,
                    last-updated: stacks-block-height,
                    factors: {
                        quality-score: quality-score,
                        transfer-score: transfer-score,
                        holder-score: holder-score
                    }
                }
            )
            (ok total-score)
        )
    )
)

(define-private (calculate-quality-score (batch-id uint) (verification-count uint))
    (if (> verification-count u0)
        (let
            ((base-quality-score (if (<= verification-count u3)
                u150
                (if (<= verification-count u6)
                    u200
                    u250
                )
            )))
            (if (< base-quality-score u300) base-quality-score u300)
        )
        u100
    )
)

(define-private (calculate-transfer-score (transfer-count uint))
    (if (is-eq transfer-count u0)
        u200
        (if (<= transfer-count u3)
            u200
            (if (<= transfer-count u6)
                u150
                u100
            )
        )
    )
)



(define-public (update-holder-reputation (holder principal) (successful-transfer bool))
    (let
        ((current-rep (default-to 
            { successful-transfers: u0, failed-verifications: u0, total-batches-handled: u0, reputation-score: base-score }
            (map-get? holder-reputation { holder: holder }))))
        (let
            ((new-successful (if successful-transfer 
                (+ (get successful-transfers current-rep) u1)
                (get successful-transfers current-rep)))
             (new-failed (if successful-transfer
                (get failed-verifications current-rep)
                (+ (get failed-verifications current-rep) u1)))
             (new-total (+ (get total-batches-handled current-rep) u1)))
            (let
                ((new-score (calculate-reputation-score new-successful new-failed new-total)))
                (map-set holder-reputation
                    { holder: holder }
                    {
                        successful-transfers: new-successful,
                        failed-verifications: new-failed,
                        total-batches-handled: new-total,
                        reputation-score: new-score
                    }
                )
                (ok new-score)
            )
        )
    )
)

(define-private (calculate-reputation-score (successful uint) (failed uint) (total uint))
    (if (is-eq total u0)
        base-score
        (let
            ((success-rate (/ (* successful u1000) total)))
            (if (>= success-rate u800)
                (if (< (+ base-score u200) max-score) (+ base-score u200) max-score)
                (if (>= success-rate u600)
                    (if (< (+ base-score u100) max-score) (+ base-score u100) max-score)
                    (if (>= success-rate u400)
                        base-score
                        (if (> (- base-score u100) u100) (- base-score u100) u100)
                    )
                )
            )
        )
    )
)

(define-public (rate-batch-quality (batch-id uint) (quality-rating uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (asserts! (<= quality-rating u10) err-invalid-score)
        (unwrap-panic (update-holder-reputation tx-sender (>= quality-rating u7)))
        (ok true)
    )
)

(define-read-only (get-batch-authenticity-score (batch-id uint))
    (map-get? batch-authenticity-score { batch-id: batch-id })
)

(define-read-only (get-holder-reputation (holder principal))
    (map-get? holder-reputation { holder: holder })
)

(define-read-only (get-batch-trust-level (batch-id uint))
    (match (map-get? batch-authenticity-score { batch-id: batch-id })
        score (let
            ((current-score (get current-score score)))
            (if (>= current-score u800)
                "high-trust"
                (if (>= current-score u600)
                    "medium-trust"
                    (if (>= current-score u400)
                        "low-trust"
                        "untrusted"
                    )
                )
            )
        )
        "unscored"
    )
)