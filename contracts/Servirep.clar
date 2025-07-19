(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_SERVICE_NOT_FOUND (err u404))
(define-constant ERR_REVIEW_NOT_FOUND (err u405))
(define-constant ERR_ALREADY_REVIEWED (err u406))
(define-constant ERR_INVALID_RATING (err u407))
(define-constant ERR_SERVICE_INACTIVE (err u408))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u409))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u410))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u411))
(define-constant ERR_SUBSCRIPTION_CANCELLED (err u412))
(define-constant ERR_INVALID_TIER (err u413))
(define-constant ERR_SUBSCRIPTION_EXISTS (err u414))
(define-constant ERR_INVALID_DURATION (err u415))

(define-non-fungible-token service-nft uint)

(define-data-var service-id-counter uint u0)
(define-data-var review-id-counter uint u0)
(define-data-var platform-fee uint u1000)
(define-data-var subscription-id-counter uint u0)

(define-map services uint {
  owner: principal,
  name: (string-ascii 64),
  description: (string-ascii 256),
  category: (string-ascii 32),
  price: uint,
  active: bool,
  created-at: uint,
  total-reviews: uint,
  average-rating: uint
})

(define-map reviews uint {
  service-id: uint,
  reviewer: principal,
  rating: uint,
  comment: (string-ascii 512),
  verified: bool,
  created-at: uint,
  helpful-votes: uint
})

(define-map user-service-reviews {user: principal, service: uint} uint)
(define-map service-reviews uint (list 100 uint))
(define-map user-reviews principal (list 50 uint))
(define-map review-votes {review: uint, voter: principal} bool)

(define-map service-tiers uint {
  service-id: uint,
  tier-name: (string-ascii 32),
  tier-level: uint,
  monthly-price: uint,
  features: (string-ascii 256),
  max-usage: uint,
  active: bool
})

(define-map subscriptions uint {
  subscriber: principal,
  service-id: uint,
  tier-id: uint,
  start-block: uint,
  end-block: uint,
  auto-renew: bool,
  status: (string-ascii 16),
  total-paid: uint,
  usage-count: uint
})

(define-map user-subscriptions principal (list 20 uint))
(define-map service-tier-list uint (list 10 uint))
(define-map service-subscription-list uint (list 50 uint))

(define-public (create-service (name (string-ascii 64)) (description (string-ascii 256)) (category (string-ascii 32)) (price uint))
  (let ((new-service-id (+ (var-get service-id-counter) u1)))
    (begin
      (try! (nft-mint? service-nft new-service-id tx-sender))
      (map-set services new-service-id {
        owner: tx-sender,
        name: name,
        description: description,
        category: category,
        price: price,
        active: true,
        created-at: stacks-block-height,
        total-reviews: u0,
        average-rating: u0
      })
      (var-set service-id-counter new-service-id)
      (ok new-service-id))))

(define-public (update-service (service-id uint) (name (string-ascii 64)) (description (string-ascii 256)) (price uint))
  (let ((service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender (get owner service)) ERR_NOT_AUTHORIZED)
      (map-set services service-id (merge service {
        name: name,
        description: description,
        price: price
      }))
      (ok true))))

(define-public (toggle-service-status (service-id uint))
  (let ((service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender (get owner service)) ERR_NOT_AUTHORIZED)
      (map-set services service-id (merge service {
        active: (not (get active service))
      }))
      (ok true))))

(define-public (submit-review (service-id uint) (rating uint) (comment (string-ascii 512)))
  (let (
    (service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND))
    (new-review-id (+ (var-get review-id-counter) u1))
    (user-review-key {user: tx-sender, service: service-id})
  )
    (begin
      (asserts! (get active service) ERR_SERVICE_INACTIVE)
      (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
      (asserts! (is-none (map-get? user-service-reviews user-review-key)) ERR_ALREADY_REVIEWED)
      
      (map-set reviews new-review-id {
        service-id: service-id,
        reviewer: tx-sender,
        rating: rating,
        comment: comment,
        verified: false,
        created-at: stacks-block-height,
        helpful-votes: u0
      })
      
      (map-set user-service-reviews user-review-key new-review-id)
      
      (let ((current-service-reviews (default-to (list) (map-get? service-reviews service-id))))
        (map-set service-reviews service-id (unwrap! (as-max-len? (append current-service-reviews new-review-id) u100) (err u500))))
      
      (let ((current-user-reviews (default-to (list) (map-get? user-reviews tx-sender))))
        (map-set user-reviews tx-sender (unwrap! (as-max-len? (append current-user-reviews new-review-id) u50) (err u501))))
      
      (var-set review-id-counter new-review-id)
      (try! (update-service-rating service-id))
      (ok new-review-id))))

(define-public (verify-review (review-id uint))
  (let ((review (unwrap! (map-get? reviews review-id) ERR_REVIEW_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
      (map-set reviews review-id (merge review {verified: true}))
      (ok true))))

(define-public (vote-helpful (review-id uint))
  (let (
    (review (unwrap! (map-get? reviews review-id) ERR_REVIEW_NOT_FOUND))
    (vote-key {review: review-id, voter: tx-sender})
  )
    (begin
      (asserts! (is-none (map-get? review-votes vote-key)) (err u410))
      (map-set review-votes vote-key true)
      (map-set reviews review-id (merge review {
        helpful-votes: (+ (get helpful-votes review) u1)
      }))
      (ok true))))

(define-public (tip-reviewer (review-id uint) (amount uint))
  (let ((review (unwrap! (map-get? reviews review-id) ERR_REVIEW_NOT_FOUND)))
    (begin
      (asserts! (> amount u0) ERR_INSUFFICIENT_PAYMENT)
      (try! (stx-transfer? amount tx-sender (get reviewer review)))
      (ok true))))

(define-public (purchase-service (service-id uint))
  (let ((service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND)))
    (let ((service-owner (get owner service))
          (service-price (get price service))
          (fee-amount (/ (* service-price (var-get platform-fee)) u100000)))
      (begin
        (asserts! (get active service) ERR_SERVICE_INACTIVE)
        (asserts! (> service-price u0) ERR_INSUFFICIENT_PAYMENT)
        (try! (stx-transfer? fee-amount tx-sender CONTRACT_OWNER))
        (try! (stx-transfer? (- service-price fee-amount) tx-sender service-owner))
        (ok true)))))

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u10000) (err u411))
    (var-set platform-fee new-fee)
    (ok true)))

(define-private (update-service-rating (service-id uint))
  (let (
    (service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND))
    (review-ids (default-to (list) (map-get? service-reviews service-id)))
  )
    (let (
      (total-rating (fold + (map get-review-rating review-ids) u0))
      (review-count (len review-ids))
      (avg-rating (if (> review-count u0) (/ total-rating review-count) u0))
    )
      (begin
        (map-set services service-id (merge service {
          total-reviews: review-count,
          average-rating: avg-rating
        }))
        (ok true)))))

(define-private (get-review-rating (review-id uint))
  (match (map-get? reviews review-id)
    review (get rating review)
    u0))

(define-read-only (get-service (service-id uint))
  (map-get? services service-id))

(define-read-only (get-review (review-id uint))
  (map-get? reviews review-id))

(define-read-only (get-service-reviews (service-id uint))
  (map-get? service-reviews service-id))

(define-read-only (get-user-reviews (user principal))
  (map-get? user-reviews user))

(define-read-only (get-user-review-for-service (user principal) (service-id uint))
  (map-get? user-service-reviews {user: user, service: service-id}))

(define-read-only (get-service-owner (service-id uint))
  (match (nft-get-owner? service-nft service-id)
    owner (some owner)
    none))

(define-read-only (get-service-count)
  (var-get service-id-counter))

(define-read-only (get-review-count)
  (var-get review-id-counter))

(define-read-only (get-platform-fee)
  (var-get platform-fee))

(define-read-only (has-user-reviewed (user principal) (service-id uint))
  (is-some (map-get? user-service-reviews {user: user, service: service-id})))

(define-read-only (has-voted-helpful (user principal) (review-id uint))
  (is-some (map-get? review-votes {review: review-id, voter: user})))

(define-public (create-service-tier (service-id uint) (tier-name (string-ascii 32)) (tier-level uint) (monthly-price uint) (features (string-ascii 256)) (max-usage uint))
  (let ((service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND))
        (new-tier-id (+ (var-get subscription-id-counter) u1)))
    (begin
      (asserts! (is-eq tx-sender (get owner service)) ERR_NOT_AUTHORIZED)
      (asserts! (> monthly-price u0) ERR_INSUFFICIENT_PAYMENT)
      (asserts! (and (>= tier-level u1) (<= tier-level u5)) ERR_INVALID_TIER)
      
      (map-set service-tiers new-tier-id {
        service-id: service-id,
        tier-name: tier-name,
        tier-level: tier-level,
        monthly-price: monthly-price,
        features: features,
        max-usage: max-usage,
        active: true
      })
      
      (let ((current-tiers (default-to (list) (map-get? service-tier-list service-id))))
        (map-set service-tier-list service-id (unwrap! (as-max-len? (append current-tiers new-tier-id) u10) (err u500))))
      
      (var-set subscription-id-counter new-tier-id)
      (ok new-tier-id))))

(define-public (subscribe-to-service (tier-id uint) (duration-blocks uint) (auto-renew bool))
  (let ((tier (unwrap! (map-get? service-tiers tier-id) ERR_SUBSCRIPTION_NOT_FOUND))
        (service (unwrap! (map-get? services (get service-id tier)) ERR_SERVICE_NOT_FOUND))
        (new-subscription-id (+ (var-get subscription-id-counter) u1))
        (monthly-blocks u4320)
        (total-cost (* (get monthly-price tier) (/ duration-blocks monthly-blocks))))
    (begin
      (asserts! (get active service) ERR_SERVICE_INACTIVE)
      (asserts! (get active tier) ERR_INVALID_TIER)
      (asserts! (>= duration-blocks monthly-blocks) ERR_INVALID_DURATION)
      
      (let ((fee-amount (/ (* total-cost (var-get platform-fee)) u100000))
            (service-amount (- total-cost fee-amount)))
        (try! (stx-transfer? fee-amount tx-sender CONTRACT_OWNER))
        (try! (stx-transfer? service-amount tx-sender (get owner service))))
      
      (map-set subscriptions new-subscription-id {
        subscriber: tx-sender,
        service-id: (get service-id tier),
        tier-id: tier-id,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration-blocks),
        auto-renew: auto-renew,
        status: "active",
        total-paid: total-cost,
        usage-count: u0
      })
      
      (let ((current-user-subs (default-to (list) (map-get? user-subscriptions tx-sender))))
        (map-set user-subscriptions tx-sender (unwrap! (as-max-len? (append current-user-subs new-subscription-id) u20) (err u501))))
      
      (let ((current-service-subs (default-to (list) (map-get? service-subscription-list (get service-id tier)))))
        (map-set service-subscription-list (get service-id tier) (unwrap! (as-max-len? (append current-service-subs new-subscription-id) u50) (err u502))))
      
      (var-set subscription-id-counter new-subscription-id)
      (ok new-subscription-id))))

(define-public (renew-subscription (subscription-id uint) (duration-blocks uint))
  (let ((subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND))
        (tier (unwrap! (map-get? service-tiers (get tier-id subscription)) ERR_SUBSCRIPTION_NOT_FOUND))
        (service (unwrap! (map-get? services (get service-id subscription)) ERR_SERVICE_NOT_FOUND))
        (monthly-blocks u4320)
        (total-cost (* (get monthly-price tier) (/ duration-blocks monthly-blocks))))
    (begin
      (asserts! (is-eq tx-sender (get subscriber subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) "active") ERR_SUBSCRIPTION_CANCELLED)
      (asserts! (>= duration-blocks monthly-blocks) ERR_INVALID_DURATION)
      
      (let ((fee-amount (/ (* total-cost (var-get platform-fee)) u100000))
            (service-amount (- total-cost fee-amount)))
        (try! (stx-transfer? fee-amount tx-sender CONTRACT_OWNER))
        (try! (stx-transfer? service-amount tx-sender (get owner service))))
      
      (map-set subscriptions subscription-id (merge subscription {
        end-block: (+ (get end-block subscription) duration-blocks),
        total-paid: (+ (get total-paid subscription) total-cost)
      }))
      (ok true))))

(define-public (cancel-subscription (subscription-id uint))
  (let ((subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender (get subscriber subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) "active") ERR_SUBSCRIPTION_CANCELLED)
      
      (map-set subscriptions subscription-id (merge subscription {
        status: "cancelled",
        auto-renew: false
      }))
      (ok true))))

(define-public (toggle-tier-status (tier-id uint))
  (let ((tier (unwrap! (map-get? service-tiers tier-id) ERR_SUBSCRIPTION_NOT_FOUND))
        (service (unwrap! (map-get? services (get service-id tier)) ERR_SERVICE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender (get owner service)) ERR_NOT_AUTHORIZED)
      (map-set service-tiers tier-id (merge tier {
        active: (not (get active tier))
      }))
      (ok true))))

(define-public (update-tier-pricing (tier-id uint) (new-monthly-price uint))
  (let ((tier (unwrap! (map-get? service-tiers tier-id) ERR_SUBSCRIPTION_NOT_FOUND))
        (service (unwrap! (map-get? services (get service-id tier)) ERR_SERVICE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender (get owner service)) ERR_NOT_AUTHORIZED)
      (asserts! (> new-monthly-price u0) ERR_INSUFFICIENT_PAYMENT)
      
      (map-set service-tiers tier-id (merge tier {
        monthly-price: new-monthly-price
      }))
      (ok true))))

(define-public (record-service-usage (subscription-id uint))
  (let ((subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND))
        (tier (unwrap! (map-get? service-tiers (get tier-id subscription)) ERR_SUBSCRIPTION_NOT_FOUND))
        (service (unwrap! (map-get? services (get service-id subscription)) ERR_SERVICE_NOT_FOUND)))
    (begin
      (asserts! (or (is-eq tx-sender (get owner service)) (is-eq tx-sender (get subscriber subscription))) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) "active") ERR_SUBSCRIPTION_CANCELLED)
      (asserts! (> (get end-block subscription) stacks-block-height) ERR_SUBSCRIPTION_EXPIRED)
      (asserts! (< (get usage-count subscription) (get max-usage tier)) (err u416))
      
      (map-set subscriptions subscription-id (merge subscription {
        usage-count: (+ (get usage-count subscription) u1)
      }))
      (ok true))))

(define-public (auto-renew-check (subscription-id uint))
  (let ((subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND)))
    (begin
      (asserts! (and 
        (get auto-renew subscription)
        (is-eq (get status subscription) "active")
        (<= (get end-block subscription) stacks-block-height)) 
        ERR_SUBSCRIPTION_EXPIRED)
      
      (map-set subscriptions subscription-id (merge subscription {
        status: "expired"
      }))
      (ok true))))

(define-read-only (get-service-tier (tier-id uint))
  (map-get? service-tiers tier-id))

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions subscription-id))

(define-read-only (get-service-tiers (service-id uint))
  (map-get? service-tier-list service-id))

(define-read-only (get-user-subscriptions (user principal))
  (map-get? user-subscriptions user))

(define-read-only (get-service-subscriptions (service-id uint))
  (map-get? service-subscription-list service-id))

(define-read-only (is-subscription-active (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription (and 
      (is-eq (get status subscription) "active")
      (> (get end-block subscription) stacks-block-height))
    false))

(define-read-only (get-subscription-usage-remaining (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription 
      (match (map-get? service-tiers (get tier-id subscription))
        tier (- (get max-usage tier) (get usage-count subscription))
        u0)
    u0))

(define-read-only (get-subscription-blocks-remaining (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription 
      (if (> (get end-block subscription) stacks-block-height)
        (- (get end-block subscription) stacks-block-height)
        u0)
    u0))
