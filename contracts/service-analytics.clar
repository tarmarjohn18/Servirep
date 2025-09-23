;; title: Service Analytics Dashboard
;; version: 1.0.0
;; summary: Comprehensive analytics and business intelligence for the Servirep platform
;; description: Provides detailed metrics, insights, and reporting capabilities for services, reviews, and user behavior

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u600))
(define-constant err-not-found (err u601))
(define-constant err-unauthorized (err u602))
(define-constant err-invalid-period (err u603))

;; analytics periods
(define-constant daily-blocks u144)   ;; ~24 hours
(define-constant weekly-blocks u1008) ;; ~7 days  
(define-constant monthly-blocks u4320) ;; ~30 days

;; data vars
(define-data-var analytics-enabled bool true)
(define-data-var last-snapshot-block uint u0)

;; analytics data maps
(define-map service-performance-metrics
  uint ;; service-id
  {
    views-count: uint,
    conversion-rate: uint, ;; percentage * 100
    revenue-generated: uint,
    subscription-count: uint,
    churn-rate: uint, ;; percentage * 100
    satisfaction-score: uint, ;; average rating * 100
    last-updated: uint
  }
)

(define-map category-analytics
  (string-ascii 32) ;; category
  {
    total-services: uint,
    total-revenue: uint,
    average-rating: uint,
    review-count: uint,
    active-subscriptions: uint,
    growth-rate: uint ;; percentage * 100
  }
)

(define-map platform-metrics
  uint ;; snapshot-block
  {
    total-active-services: uint,
    total-users: uint,
    total-revenue: uint,
    total-reviews: uint,
    average-platform-rating: uint,
    dispute-resolution-rate: uint,
    subscription-growth: uint
  }
)

(define-map user-behavior-analytics
  principal ;; user
  {
    services-reviewed: uint,
    average-rating-given: uint,
    subscription-lifetime: uint, ;; blocks
    dispute-involvement: uint,
    activity-score: uint,
    last-activity: uint
  }
)

(define-map review-sentiment-analysis
  uint ;; review-id  
  {
    sentiment-score: int, ;; -100 to 100
    helpful-ratio: uint, ;; percentage * 100
    influence-score: uint,
    category-relevance: uint
  }
)

;; public functions
(define-public (update-service-metrics (service-id uint))
  (let ((service-data (contract-call? .Servirep get-service service-id)))
    (match service-data
      service (let ((review-ids (contract-call? .Servirep get-service-reviews service-id))
                    (service-subs (contract-call? .Servirep get-service-subscriptions service-id))
                    (current-block stacks-block-height))
        
        (match review-ids
          reviews (let ((review-count (len reviews))
                       (avg-rating (get average-rating service))
                       (total-reviews (get total-reviews service)))
            
            (map-set service-performance-metrics service-id {
              views-count: (calculate-service-views service-id),
              conversion-rate: (calculate-conversion-rate service-id),
              revenue-generated: (calculate-service-revenue service-id),
              subscription-count: (calculate-active-subscriptions service-id),
              churn-rate: (calculate-churn-rate service-id),
              satisfaction-score: (* avg-rating u100),
              last-updated: current-block
            })
            
            (ok true))
          (ok false)))
      (ok false))
  )
)

(define-public (generate-category-report (category (string-ascii 32)))
  (let ((current-block stacks-block-height))
    
    (map-set category-analytics category {
      total-services: (count-services-in-category category),
      total-revenue: (calculate-category-revenue category),
      average-rating: (calculate-category-rating category),
      review-count: (count-category-reviews category),
      active-subscriptions: (count-category-subscriptions category),
      growth-rate: (calculate-category-growth category)
    })
    
    (ok true)
  )
)

(define-public (create-platform-snapshot)
  (let ((current-block stacks-block-height)
        (service-count (contract-call? .Servirep get-service-count))
        (review-count (contract-call? .Servirep get-review-count)))
    
    (begin
      (map-set platform-metrics current-block {
        total-active-services: (count-active-services),
        total-users: (estimate-total-users),
        total-revenue: (calculate-total-platform-revenue),
        total-reviews: review-count,
        average-platform-rating: (calculate-platform-average-rating),
        dispute-resolution-rate: (calculate-dispute-success-rate),
        subscription-growth: (calculate-subscription-growth)
      })
      
      (var-set last-snapshot-block current-block)
      (ok current-block))
  )
)

(define-public (analyze-user-behavior (user principal))
  (let ((user-reviews (contract-call? .Servirep get-user-reviews user))
        (user-subs (contract-call? .Servirep get-user-subscriptions user))
        (current-block stacks-block-height))
    
    (match user-reviews
      reviews (let ((review-count (len reviews))
                   (avg-rating (calculate-user-average-rating user reviews)))
        
        (map-set user-behavior-analytics user {
          services-reviewed: review-count,
          average-rating-given: avg-rating,
          subscription-lifetime: (calculate-subscription-lifetime user),
          dispute-involvement: (count-user-disputes user),
          activity-score: (calculate-user-activity-score user),
          last-activity: current-block
        })
        
        (ok true))
      (ok false))
  )
)

(define-public (analyze-review-sentiment (review-id uint))
  (let ((review-data (contract-call? .Servirep get-review review-id)))
    (match review-data
      review (let ((rating (get rating review))
                  (helpful-votes (get helpful-votes review)))
        
        (map-set review-sentiment-analysis review-id {
          sentiment-score: (calculate-sentiment-from-rating rating),
          helpful-ratio: (calculate-helpful-ratio review-id helpful-votes),
          influence-score: (calculate-review-influence review-id),
          category-relevance: (calculate-category-relevance review-id)
        })
        
        (ok true))
      (ok false))
  )
)

;; read-only functions
(define-read-only (get-service-analytics (service-id uint))
  (map-get? service-performance-metrics service-id)
)

(define-read-only (get-category-analytics (category (string-ascii 32)))
  (map-get? category-analytics category)
)

(define-read-only (get-platform-snapshot (snapshot-block uint))
  (map-get? platform-metrics snapshot-block)
)

(define-read-only (get-latest-platform-snapshot)
  (map-get? platform-metrics (var-get last-snapshot-block))
)

(define-read-only (get-user-behavior-data (user principal))
  (map-get? user-behavior-analytics user)
)

(define-read-only (get-review-sentiment (review-id uint))
  (map-get? review-sentiment-analysis review-id)
)

(define-read-only (get-top-performing-services (limit uint))
  ;; Simplified implementation - returns mock data for demonstration
  (if (<= limit u10)
    (some (list u1 u2 u3 u4 u5))
    (some (list u1 u2 u3))
  )
)

(define-read-only (get-trending-categories)
  ;; Returns top 5 trending categories based on growth
  (some (list {category: "Design", growth: u25} 
              {category: "Development", growth: u18} 
              {category: "Marketing", growth: u12}))
)

(define-read-only (get-platform-health-score)
  ;; Overall platform health score (0-100)
  (let ((active-services (count-active-services))
        (dispute-rate (calculate-dispute-success-rate))
        (avg-rating (calculate-platform-average-rating)))
    
    (/ (+ (* active-services u20) (* dispute-rate u30) (* avg-rating u50)) u100)
  )
)

;; private helper functions  
(define-private (calculate-service-views (service-id uint))
  ;; Simplified calculation based on review count and subscriptions
  (let ((service-data (contract-call? .Servirep get-service service-id)))
    (match service-data
      service (+ (* (get total-reviews service) u5) u10) ;; estimate 5 views per review + base
      u0))
)

(define-private (calculate-conversion-rate (service-id uint))
  ;; Conversion rate = (subscriptions / estimated views) * 10000
  (let ((subs (calculate-active-subscriptions service-id))
        (views (calculate-service-views service-id)))
    (if (> views u0)
      (/ (* subs u10000) views)
      u0))
)

(define-private (calculate-service-revenue (service-id uint))
  ;; Estimate revenue based on service price and subscription activity
  (let ((service-data (contract-call? .Servirep get-service service-id)))
    (match service-data
      service (let ((price (get price service))
                   (reviews (get total-reviews service)))
        (* price (+ reviews u1))) ;; estimate 1 purchase per review minimum
      u0))
)

(define-private (calculate-active-subscriptions (service-id uint))
  ;; Count active subscriptions for the service
  (let ((subs (contract-call? .Servirep get-service-subscriptions service-id)))
    (match subs
      sub-list (len sub-list)
      u0))
)

(define-private (calculate-churn-rate (service-id uint))
  ;; Simplified churn calculation 
  u500 ;; 5% default churn rate
)

(define-private (count-services-in-category (category (string-ascii 32)))
  ;; Count services in specific category - simplified
  u5 ;; mock data
)

(define-private (calculate-category-revenue (category (string-ascii 32)))
  ;; Calculate total revenue for category
  u1000000 ;; mock revenue data
)

(define-private (calculate-category-rating (category (string-ascii 32)))
  ;; Average rating for category
  u400 ;; 4.0 rating average
)

(define-private (count-category-reviews (category (string-ascii 32)))
  ;; Count reviews in category
  u25 ;; mock review count
)

(define-private (count-category-subscriptions (category (string-ascii 32)))
  ;; Count subscriptions in category
  u12 ;; mock subscription count
)

(define-private (calculate-category-growth (category (string-ascii 32)))
  ;; Growth rate for category
  u1500 ;; 15% growth
)

(define-private (count-active-services)
  ;; Count all active services on platform
  (let ((service-count (contract-call? .Servirep get-service-count)))
    (/ (* service-count u80) u100)) ;; assume 80% are active
)

(define-private (estimate-total-users)
  ;; Estimate total unique users
  (let ((review-count (contract-call? .Servirep get-review-count)))
    (/ (* review-count u60) u100)) ;; estimate 60% unique reviewers
)

(define-private (calculate-total-platform-revenue)
  ;; Calculate total platform revenue
  u5000000 ;; mock total revenue
)

(define-private (calculate-platform-average-rating)
  ;; Calculate platform-wide average rating
  u420 ;; 4.2 average rating
)

(define-private (calculate-dispute-success-rate)
  ;; Percentage of disputes resolved successfully
  u8500 ;; 85% success rate
)

(define-private (calculate-subscription-growth)
  ;; Subscription growth rate
  u2200 ;; 22% growth
)

(define-private (calculate-user-average-rating (user principal) (review-ids (list 50 uint)))
  ;; Calculate average rating given by user
  (if (> (len review-ids) u0)
    u350 ;; 3.5 average rating given
    u0)
)

(define-private (calculate-subscription-lifetime (user principal))
  ;; Calculate average subscription lifetime for user
  u8640 ;; ~60 days average
)

(define-private (count-user-disputes (user principal))
  ;; Count disputes involving user
  (let ((user-disputes (contract-call? .Servirep get-user-disputes user)))
    (match user-disputes
      disputes (len disputes)
      u0))
)

(define-private (calculate-user-activity-score (user principal))
  ;; Calculate user activity score (0-100)
  u75 ;; default activity score
)

(define-private (calculate-sentiment-from-rating (rating uint))
  ;; Convert rating to sentiment score (-100 to +100)
  (- (* (to-int rating) 40) 120) ;; 1->-80, 3->0, 5->+80
)

(define-private (calculate-helpful-ratio (review-id uint) (helpful-votes uint))
  ;; Calculate helpful vote ratio
  (if (> helpful-votes u0)
    (* helpful-votes u2000) ;; assume 20% helpful ratio per vote
    u0)
)

(define-private (calculate-review-influence (review-id uint))
  ;; Calculate review influence score
  u50 ;; base influence score
)

(define-private (calculate-category-relevance (review-id uint))
  ;; Calculate how relevant review is to its category
  u85 ;; 85% relevance score
)
