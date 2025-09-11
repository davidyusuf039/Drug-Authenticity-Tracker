;; Supply Chain Analytics Contract
;; Provides performance metrics, efficiency tracking, and operational insights for the drug supply chain

;; Error constants
(define-constant err-unauthorized (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-period (err u202))
(define-constant err-invalid-data (err u203))

;; Data variables for analytics
(define-data-var analytics-enabled bool true)
(define-data-var total-supply-chain-events uint u0)

;; Supply chain performance metrics
(define-map supply-chain-metrics
    { period-start: uint, period-end: uint }
    {
        total-batches-processed: uint,
        average-transfer-time: uint,
        successful-transfers: uint,
        failed-transfers: uint,
        quality-compliance-rate: uint,
        contamination-incidents: uint,
        average-batch-journey-time: uint
    }
)

;; Holder performance analytics
(define-map holder-performance-metrics
    { holder: principal, period: uint }
    {
        batches-handled: uint,
        average-handling-time: uint,
        quality-score: uint,
        on-time-transfers: uint,
        late-transfers: uint,
        contamination-reports: uint,
        efficiency-rating: uint
    }
)

;; Transfer efficiency tracking
(define-map transfer-efficiency-data
    { from-holder: principal, to-holder: principal }
    {
        total-transfers: uint,
        average-duration: uint,
        success-rate: uint,
        last-transfer-date: uint,
        efficiency-score: uint
    }
)

;; Drug category performance metrics
(define-map drug-category-metrics
    { drug-category: (string-ascii 64), period: uint }
    {
        total-batches: uint,
        average-shelf-life-utilization: uint,
        recall-rate: uint,
        contamination-rate: uint,
        quality-pass-rate: uint
    }
)

;; Geographic performance data
(define-map location-performance
    { location: (string-ascii 100) }
    {
        total-transfers: uint,
        average-processing-time: uint,
        quality-incidents: uint,
        efficiency-score: uint,
        last-activity: uint
    }
)

;; Real-time supply chain status
(define-map supply-chain-status
    { status-type: (string-ascii 20) }
    {
        current-count: uint,
        trend-direction: (string-ascii 10),
        last-updated: uint
    }
)

;; Record supply chain event for analytics
(define-public (record-supply-chain-event
    (event-type (string-ascii 30))
    (batch-id uint)
    (holder principal)
    (location (string-ascii 100))
    (processing-time uint))
    (begin
        (asserts! (var-get analytics-enabled) (ok true))
        ;; Increment total events counter
        (var-set total-supply-chain-events (+ (var-get total-supply-chain-events) u1))
        
        ;; Update location performance
        (update-location-performance location processing-time)
        
        ;; Update real-time status
        (update-real-time-status event-type)
        (ok true)
    )
)

;; Update location performance metrics
(define-private (update-location-performance 
    (location (string-ascii 100)) 
    (processing-time uint))
    (let ((current-perf (default-to 
            {
                total-transfers: u0,
                average-processing-time: u0,
                quality-incidents: u0,
                efficiency-score: u100,
                last-activity: u0
            }
            (map-get? location-performance { location: location }))))
        (map-set location-performance
            { location: location }
            (merge current-perf {
                total-transfers: (+ (get total-transfers current-perf) u1),
                average-processing-time: (/ (+ (* (get average-processing-time current-perf) (get total-transfers current-perf)) processing-time) (+ (get total-transfers current-perf) u1)),
                last-activity: stacks-block-height
            })
        )
    )
)

;; Update real-time supply chain status
(define-private (update-real-time-status (event-type (string-ascii 30)))
    (let ((current-status (default-to 
            { current-count: u0, trend-direction: "stable", last-updated: u0 }
            (map-get? supply-chain-status { status-type: "active-batches" }))))
        (map-set supply-chain-status
            { status-type: "active-batches" }
            (merge current-status {
                current-count: (+ (get current-count current-status) u1),
                last-updated: stacks-block-height
            })
        )
    )
)

;; Calculate transfer efficiency between holders
(define-public (calculate-transfer-efficiency
    (from-holder principal)
    (to-holder principal)
    (transfer-duration uint)
    (success bool))
    (let ((current-data (default-to
            {
                total-transfers: u0,
                average-duration: u0,
                success-rate: u100,
                last-transfer-date: u0,
                efficiency-score: u100
            }
            (map-get? transfer-efficiency-data { from-holder: from-holder, to-holder: to-holder }))))
        (let ((new-total (+ (get total-transfers current-data) u1))
              (new-avg-duration (/ (+ (* (get average-duration current-data) (get total-transfers current-data)) transfer-duration) new-total))
              (success-count (+ (* (get success-rate current-data) (get total-transfers current-data)) (if success u100 u0)))
              (new-success-rate (/ success-count new-total))
              (efficiency-score (calculate-efficiency-score new-avg-duration new-success-rate)))
            (map-set transfer-efficiency-data
                { from-holder: from-holder, to-holder: to-holder }
                {
                    total-transfers: new-total,
                    average-duration: new-avg-duration,
                    success-rate: new-success-rate,
                    last-transfer-date: stacks-block-height,
                    efficiency-score: efficiency-score
                }
            )
            (ok efficiency-score)
        )
    )
)

;; Calculate efficiency score based on duration and success rate
(define-private (calculate-efficiency-score (avg-duration uint) (success-rate uint))
    (let ((duration-score (if (<= avg-duration u50)
                             u100
                             (if (<= avg-duration u100)
                                u80
                                (if (<= avg-duration u200)
                                   u60
                                   u40))))
          (combined-score (/ (+ duration-score success-rate) u2)))
        (if (> combined-score u100) u100 combined-score)
    )
)

;; Generate performance report for a holder
(define-public (generate-holder-performance-report
    (holder principal)
    (period uint)
    (batches-handled uint)
    (avg-handling-time uint)
    (quality-score uint))
    (let ((on-time-rate u85)
          (efficiency-rating (calculate-holder-efficiency quality-score on-time-rate)))
        (map-set holder-performance-metrics
            { holder: holder, period: period }
            {
                batches-handled: batches-handled,
                average-handling-time: avg-handling-time,
                quality-score: quality-score,
                on-time-transfers: (/ (* batches-handled on-time-rate) u100),
                late-transfers: (- batches-handled (/ (* batches-handled on-time-rate) u100)),
                contamination-reports: u0,
                efficiency-rating: efficiency-rating
            }
        )
        (ok efficiency-rating)
    )
)

;; Calculate holder efficiency rating
(define-private (calculate-holder-efficiency (quality-score uint) (on-time-rate uint))
    (let ((weighted-score (/ (+ (* quality-score u6) (* on-time-rate u4)) u10)))
        (if (>= weighted-score u90)
            u5  ;; Excellent
            (if (>= weighted-score u80)
                u4  ;; Very Good
                (if (>= weighted-score u70)
                    u3  ;; Good
                    (if (>= weighted-score u60)
                        u2  ;; Fair
                        u1  ;; Poor
                    )
                )
            )
        )
    )
)

;; Get supply chain overview
(define-read-only (get-supply-chain-overview)
    (let ((total-events (var-get total-supply-chain-events))
          (analytics-status (var-get analytics-enabled)))
        {
            total-events: total-events,
            analytics-enabled: analytics-status,
            current-timestamp: stacks-block-height
        }
    )
)

;; Get transfer efficiency data
(define-read-only (get-transfer-efficiency (from-holder principal) (to-holder principal))
    (map-get? transfer-efficiency-data { from-holder: from-holder, to-holder: to-holder })
)

;; Get holder performance metrics
(define-read-only (get-holder-performance (holder principal) (period uint))
    (map-get? holder-performance-metrics { holder: holder, period: period })
)

;; Get location performance data
(define-read-only (get-location-performance (location (string-ascii 100)))
    (map-get? location-performance { location: location })
)

;; Get real-time supply chain status
(define-read-only (get-supply-chain-status (status-type (string-ascii 20)))
    (map-get? supply-chain-status { status-type: status-type })
)

;; Get supply chain metrics for a period
(define-read-only (get-supply-chain-metrics (period-start uint) (period-end uint))
    (map-get? supply-chain-metrics { period-start: period-start, period-end: period-end })
)

;; Toggle analytics collection
(define-public (toggle-analytics (enabled bool))
    (begin
        (var-set analytics-enabled enabled)
        (ok enabled)
    )
)

;; Get analytics summary
(define-read-only (get-analytics-summary)
    (let ((total-events (var-get total-supply-chain-events)))
        {
            total-supply-chain-events: total-events,
            analytics-enabled: (var-get analytics-enabled),
            system-health: (if (> total-events u0) "active" "inactive"),
            last-update: stacks-block-height
        }
    )
)
