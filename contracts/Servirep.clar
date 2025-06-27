(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_SERVICE_NOT_FOUND (err u404))
(define-constant ERR_REVIEW_NOT_FOUND (err u405))
(define-constant ERR_ALREADY_REVIEWED (err u406))
(define-constant ERR_INVALID_RATING (err u407))
(define-constant ERR_SERVICE_INACTIVE (err u408))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u409))

(define-non-fungible-token service-nft uint)

(define-data-var service-id-counter uint u0)
(define-data-var review-id-counter uint u0)
(define-data-var platform-fee uint u1000)

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
