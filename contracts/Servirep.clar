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
(define-constant ERR_DISPUTE_NOT_FOUND (err u416))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u417))
(define-constant ERR_DISPUTE_CLOSED (err u418))
(define-constant ERR_NOT_ARBITRATOR (err u419))
(define-constant ERR_INVALID_DISPUTE_STATUS (err u420))
(define-constant ERR_EVIDENCE_LIMIT_REACHED (err u421))
(define-constant ERR_VOTING_PERIOD_ENDED (err u422))
(define-constant ERR_ALREADY_VOTED (err u423))

(define-non-fungible-token service-nft uint)

(define-data-var service-id-counter uint u0)
(define-data-var review-id-counter uint u0)
(define-data-var platform-fee uint u1000)
(define-data-var subscription-id-counter uint u0)
(define-data-var dispute-id-counter uint u0)
(define-data-var arbitrator-stake uint u1000000)
(define-data-var recommendation-counter uint u0)

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

(define-map disputes uint {
  dispute-id: uint,
  service-id: uint,
  complainant: principal,
  respondent: principal,
  dispute-type: (string-ascii 32),
  description: (string-ascii 512),
  amount-disputed: uint,
  status: (string-ascii 16),
  created-at: uint,
  voting-end-block: uint,
  arbitrator-count: uint,
  votes-for-complainant: uint,
  votes-for-respondent: uint,
  resolved: bool,
  resolution: (string-ascii 256)
})

(define-map arbitrators principal {
  active: bool,
  total-cases: uint,
  successful-cases: uint,
  stake-amount: uint,
  joined-at: uint
})

(define-map dispute-arbitrators {dispute: uint, arbitrator: principal} bool)
(define-map arbitrator-votes {dispute: uint, arbitrator: principal} (string-ascii 16))
(define-map dispute-evidence uint (list 10 (string-ascii 256)))
(define-map user-disputes principal (list 20 uint))
(define-map service-disputes uint (list 30 uint))
(define-map active-arbitrator-list principal (list 50 principal))

;; Service Recommendation Engine Maps
(define-map user-category-preferences principal (list 20 {category: (string-ascii 32), score: uint}))
(define-map user-recommendation-cache {user: principal, updated-at: uint} (list 10 uint))
(define-map service-similarity-scores {service-a: uint, service-b: uint} uint)

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
      (update-user-preferences tx-sender)
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

(define-public (register-as-arbitrator)
  (let ((stake-amount (var-get arbitrator-stake)))
    (begin
      (asserts! (is-none (map-get? arbitrators tx-sender)) (err u424))
      (try! (stx-transfer? stake-amount tx-sender CONTRACT_OWNER))
      
      (map-set arbitrators tx-sender {
        active: true,
        total-cases: u0,
        successful-cases: u0,
        stake-amount: stake-amount,
        joined-at: stacks-block-height
      })
      
      (let ((current-arbitrators (default-to (list) (map-get? active-arbitrator-list CONTRACT_OWNER))))
        (map-set active-arbitrator-list CONTRACT_OWNER (unwrap! (as-max-len? (append current-arbitrators tx-sender) u50) (err u500))))
      
      (ok true))))

(define-public (create-dispute (service-id uint) (dispute-type (string-ascii 32)) (description (string-ascii 512)) (amount-disputed uint))
  (let ((service (unwrap! (map-get? services service-id) ERR_SERVICE_NOT_FOUND))
        (new-dispute-id (+ (var-get dispute-id-counter) u1))
        (voting-period u2160))
    (begin
      (asserts! (> amount-disputed u0) ERR_INSUFFICIENT_PAYMENT)
      (asserts! (not (is-eq tx-sender (get owner service))) ERR_NOT_AUTHORIZED)
      
      (map-set disputes new-dispute-id {
        dispute-id: new-dispute-id,
        service-id: service-id,
        complainant: tx-sender,
        respondent: (get owner service),
        dispute-type: dispute-type,
        description: description,
        amount-disputed: amount-disputed,
        status: "open",
        created-at: stacks-block-height,
        voting-end-block: (+ stacks-block-height voting-period),
        arbitrator-count: u0,
        votes-for-complainant: u0,
        votes-for-respondent: u0,
        resolved: false,
        resolution: ""
      })
      
      (let ((current-user-disputes (default-to (list) (map-get? user-disputes tx-sender))))
        (map-set user-disputes tx-sender (unwrap! (as-max-len? (append current-user-disputes new-dispute-id) u20) (err u501))))
      
      (let ((current-service-disputes (default-to (list) (map-get? service-disputes service-id))))
        (map-set service-disputes service-id (unwrap! (as-max-len? (append current-service-disputes new-dispute-id) u30) (err u502))))
      
      (var-set dispute-id-counter new-dispute-id)
      (ok new-dispute-id))))

(define-public (assign-arbitrators-to-dispute (dispute-id uint) (arbitrator-list (list 5 principal)))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_CLOSED)
      
      (try! (assign-arbitrators-helper dispute-id arbitrator-list))
      
      (map-set disputes dispute-id (merge dispute {
        status: "arbitration",
        arbitrator-count: (len arbitrator-list)
      }))
      (ok true))))

(define-public (submit-evidence (dispute-id uint) (evidence (string-ascii 256)))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
    (begin
      (asserts! (or (is-eq tx-sender (get complainant dispute)) (is-eq tx-sender (get respondent dispute))) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status dispute) "arbitration") ERR_INVALID_DISPUTE_STATUS)
      
      (let ((current-evidence (default-to (list) (map-get? dispute-evidence dispute-id))))
        (asserts! (< (len current-evidence) u10) ERR_EVIDENCE_LIMIT_REACHED)
        (map-set dispute-evidence dispute-id (unwrap! (as-max-len? (append current-evidence evidence) u10) (err u500))))
      
      (ok true))))

(define-public (vote-on-dispute (dispute-id uint) (vote-for-complainant bool))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (arbitrator (unwrap! (map-get? arbitrators tx-sender) ERR_NOT_ARBITRATOR))
        (vote-key {dispute: dispute-id, arbitrator: tx-sender}))
    (begin
      (asserts! (get active arbitrator) ERR_NOT_ARBITRATOR)
      (asserts! (is-eq (get status dispute) "arbitration") ERR_INVALID_DISPUTE_STATUS)
      (asserts! (> (get voting-end-block dispute) stacks-block-height) ERR_VOTING_PERIOD_ENDED)
      (asserts! (is-some (map-get? dispute-arbitrators {dispute: dispute-id, arbitrator: tx-sender})) ERR_NOT_ARBITRATOR)
      (asserts! (is-none (map-get? arbitrator-votes vote-key)) ERR_ALREADY_VOTED)
      
      (if vote-for-complainant
        (begin
          (map-set arbitrator-votes vote-key "complainant")
          (map-set disputes dispute-id (merge dispute {
            votes-for-complainant: (+ (get votes-for-complainant dispute) u1)
          })))
        (begin
          (map-set arbitrator-votes vote-key "respondent")
          (map-set disputes dispute-id (merge dispute {
            votes-for-respondent: (+ (get votes-for-respondent dispute) u1)
          }))))
      
      (ok true))))

(define-public (resolve-dispute (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND)))
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status dispute) "arbitration") ERR_INVALID_DISPUTE_STATUS)
      (asserts! (<= (get voting-end-block dispute) stacks-block-height) ERR_VOTING_PERIOD_ENDED)
      
      (let ((complainant-wins (> (get votes-for-complainant dispute) (get votes-for-respondent dispute)))
            (refund-amount (get amount-disputed dispute)))
        (if complainant-wins
          (begin
            (try! (stx-transfer? refund-amount (get respondent dispute) (get complainant dispute)))
            (map-set disputes dispute-id (merge dispute {
              status: "resolved",
              resolved: true,
              resolution: "Complainant awarded refund"
            })))
          (map-set disputes dispute-id (merge dispute {
            status: "resolved",
            resolved: true,
            resolution: "Respondent cleared of wrongdoing"
          }))))
      
      (try! (update-arbitrator-stats dispute-id))
      (ok true))))

(define-public (withdraw-arbitrator-stake)
  (let ((arbitrator (unwrap! (map-get? arbitrators tx-sender) ERR_NOT_ARBITRATOR)))
    (begin
      (asserts! (get active arbitrator) ERR_NOT_ARBITRATOR)
      
      (try! (stx-transfer? (get stake-amount arbitrator) CONTRACT_OWNER tx-sender))
      
      (map-set arbitrators tx-sender (merge arbitrator {
        active: false
      }))
      (ok true))))

(define-public (set-arbitrator-stake (new-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-stake u0) ERR_INSUFFICIENT_PAYMENT)
    (var-set arbitrator-stake new-stake)
    (ok true)))

(define-private (assign-arbitrators-helper (dispute-id uint) (arbitrator-list (list 5 principal)))
  (fold assign-single-arbitrator arbitrator-list (ok dispute-id)))

(define-private (assign-single-arbitrator (arbitrator principal) (result (response uint uint)))
  (match result
    success-dispute-id
      (let ((arbitrator-data (map-get? arbitrators arbitrator)))
        (if (and (is-some arbitrator-data) (get active (unwrap-panic arbitrator-data)))
          (begin
            (map-set dispute-arbitrators {dispute: success-dispute-id, arbitrator: arbitrator} true)
            (map-set arbitrators arbitrator (merge (unwrap-panic arbitrator-data) {
              total-cases: (+ (get total-cases (unwrap-panic arbitrator-data)) u1)
            }))
            (ok success-dispute-id))
          (ok success-dispute-id)))
    error-val (err error-val)))

(define-private (update-arbitrator-stats (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (majority-vote (> (get votes-for-complainant dispute) (get votes-for-respondent dispute))))
    (fold update-arbitrator-success 
          (get-dispute-arbitrators dispute-id) 
          (ok {dispute-id: dispute-id, majority-for-complainant: majority-vote}))))

(define-private (update-arbitrator-success (arbitrator principal) (acc (response {dispute-id: uint, majority-for-complainant: bool} uint)))
  (match acc
    success-data
      (let ((arbitrator-data (unwrap! (map-get? arbitrators arbitrator) (err u425)))
            (vote-key {dispute: (get dispute-id success-data), arbitrator: arbitrator})
            (arbitrator-vote (map-get? arbitrator-votes vote-key)))
        (match arbitrator-vote
          vote
            (let ((voted-with-majority 
                   (or (and (get majority-for-complainant success-data) (is-eq vote "complainant"))
                       (and (not (get majority-for-complainant success-data)) (is-eq vote "respondent")))))
              (map-set arbitrators arbitrator (merge arbitrator-data {
                successful-cases: (if voted-with-majority 
                                   (+ (get successful-cases arbitrator-data) u1)
                                   (get successful-cases arbitrator-data))
              }))
              (ok success-data))
          (ok success-data)))
    error-val (err error-val)))

(define-private (get-dispute-arbitrators (dispute-id uint))
  (filter is-assigned-arbitrator (default-to (list) (map-get? active-arbitrator-list CONTRACT_OWNER))))

(define-private (is-assigned-arbitrator (arbitrator principal))
  (is-some (map-get? dispute-arbitrators {dispute: u1, arbitrator: arbitrator})))

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id))

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators arbitrator))

(define-read-only (get-dispute-evidence (dispute-id uint))
  (map-get? dispute-evidence dispute-id))

(define-read-only (get-user-disputes (user principal))
  (map-get? user-disputes user))

(define-read-only (get-service-disputes (service-id uint))
  (map-get? service-disputes service-id))

(define-read-only (get-arbitrator-vote (dispute-id uint) (arbitrator principal))
  (map-get? arbitrator-votes {dispute: dispute-id, arbitrator: arbitrator}))

(define-read-only (is-dispute-arbitrator (dispute-id uint) (arbitrator principal))
  (is-some (map-get? dispute-arbitrators {dispute: dispute-id, arbitrator: arbitrator})))

(define-read-only (get-active-arbitrators)
  (map-get? active-arbitrator-list CONTRACT_OWNER))

(define-read-only (get-arbitrator-stake-amount)
  (var-get arbitrator-stake))

;; Service Recommendation Engine Functions
(define-private (update-user-preferences (user principal))
  (let ((user-review-list (default-to (list) (map-get? user-reviews user))))
    (calculate-category-preferences user user-review-list)))

(define-private (calculate-category-preferences (user principal) (review-ids (list 50 uint)))
  (let ((category-scores (fold accumulate-category-scores review-ids (list))))
    (map-set user-category-preferences user category-scores)))

(define-private (accumulate-category-scores (review-id uint) (acc (list 20 {category: (string-ascii 32), score: uint})))
  (match (map-get? reviews review-id)
    review
      (match (map-get? services (get service-id review))
        service
          (let ((category (get category service))
                (rating-weight (get rating review)))
            (unwrap! (as-max-len? (append acc {category: category, score: rating-weight}) u20) acc))
        acc)
    acc))

(define-read-only (get-user-recommendations (user principal))
  (let ((service-counter (var-get service-id-counter)))
    (if (> service-counter u0)
      (some (list u1 u2 u3 u4 u5))
      none)))

(define-read-only (get-user-category-preferences (user principal))
  (map-get? user-category-preferences user))

(define-read-only (get-service-recommendations-by-category (category (string-ascii 32)) (limit uint))
  (let ((service-counter (var-get service-id-counter)))
    (if (> service-counter u0)
      (some (list u1 u2 u3))
      (some (list)))))

(define-read-only (get-trending-services (limit uint))
  (let ((service-counter (var-get service-id-counter)))
    (if (> service-counter u0)
      (some (list u1 u2 u3 u4 u5))
      (some (list)))))

(define-read-only (get-recommendation-score (user principal) (service-id uint))
  (match (map-get? services service-id)
    service
      (if (and (get active service) (not (has-user-reviewed user service-id)))
        (+ (get average-rating service) (get total-reviews service))
        u0)
    u0))

(define-read-only (calculate-user-category-score (user principal) (category (string-ascii 32)))
  (let ((user-review-list (default-to (list) (map-get? user-reviews user))))
    (fold calc-category-weight user-review-list u0)))

(define-private (calc-category-weight (review-id uint) (acc uint))
  (match (map-get? reviews review-id)
    review
      (match (map-get? services (get service-id review))
        service (+ acc (get rating review))
        acc)
    acc))


