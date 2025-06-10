;; title: Cryptofuse
;; version: 1.0.0
;; summary: Encrypted Message Bounties - Reward decrypting secure info post-deadline
;; description: A smart contract for creating encrypted message bounties with time-locked rewards

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-deadline-not-reached (err u104))
(define-constant err-already-solved (err u105))
(define-constant err-invalid-deadline (err u106))
(define-constant err-insufficient-funds (err u107))
(define-constant err-bounty-expired (err u108))
(define-constant err-invalid-solution (err u109))

(define-data-var bounty-counter uint u0)
(define-data-var contract-fee-rate uint u250)

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    encrypted-message: (string-ascii 500),
    solution-hash: (buff 32),
    reward-amount: uint,
    deadline: uint,
    solved: bool,
    solver: (optional principal),
    created-at: uint
  }
)

(define-map user-bounties
  { user: principal }
  { bounty-ids: (list 100 uint) }
)

(define-map bounty-attempts
  { bounty-id: uint, user: principal }
  { attempts: uint, last-attempt: uint }
)

(define-public (create-bounty 
  (encrypted-message (string-ascii 500))
  (solution-hash (buff 32))
  (deadline uint)
  (reward-amount uint))
  (let
    (
      (bounty-id (+ (var-get bounty-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> reward-amount u0) err-invalid-amount)
    (asserts! (> deadline current-block) err-invalid-deadline)
    (asserts! (>= (stx-get-balance tx-sender) reward-amount) err-insufficient-funds)
    
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        encrypted-message: encrypted-message,
        solution-hash: solution-hash,
        reward-amount: reward-amount,
        deadline: deadline,
        solved: false,
        solver: none,
        created-at: current-block
      }
    )
    
    (let
      (
        (current-bounties (default-to { bounty-ids: (list) } 
          (map-get? user-bounties { user: tx-sender })))
        (updated-bounties (unwrap! 
          (as-max-len? (append (get bounty-ids current-bounties) bounty-id) u100)
          err-invalid-amount))
      )
      (map-set user-bounties
        { user: tx-sender }
        { bounty-ids: updated-bounties }
      )
    )
    
    (var-set bounty-counter bounty-id)
    (ok bounty-id)
  )
)

(define-public (submit-solution (bounty-id uint) (solution (string-ascii 200)))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-block stacks-block-height)
      (solution-hash (sha256 (unwrap-panic (to-consensus-buff? solution))))
      (attempts-data (default-to { attempts: u0, last-attempt: u0 }
        (map-get? bounty-attempts { bounty-id: bounty-id, user: tx-sender })))
    )
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (>= current-block (get deadline bounty)) err-deadline-not-reached)
    (asserts! (is-eq solution-hash (get solution-hash bounty)) err-invalid-solution)
    
    (map-set bounty-attempts
      { bounty-id: bounty-id, user: tx-sender }
      { 
        attempts: (+ (get attempts attempts-data) u1),
        last-attempt: current-block
      }
    )
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { solved: true, solver: (some tx-sender) })
    )
    
    (let
      (
        (fee-amount (/ (* (get reward-amount bounty) (var-get contract-fee-rate)) u10000))
        (payout-amount (- (get reward-amount bounty) fee-amount))
      )
      (try! (as-contract (stx-transfer? payout-amount tx-sender tx-sender)))
      (try! (as-contract (stx-transfer? fee-amount tx-sender contract-owner)))
    )
    
    (ok true)
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (< current-block (get deadline bounty)) err-bounty-expired)
    
    (try! (as-contract (stx-transfer? (get reward-amount bounty) tx-sender (get creator bounty))))
    
    (map-delete bounties { bounty-id: bounty-id })
    (ok true)
  )
)

(define-public (extend-deadline (bounty-id uint) (new-deadline uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (> new-deadline (get deadline bounty)) err-invalid-deadline)
    (asserts! (> new-deadline current-block) err-invalid-deadline)
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { deadline: new-deadline })
    )
    (ok true)
  )
)

(define-public (increase-bounty-reward (bounty-id uint) (additional-amount uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (> additional-amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) additional-amount) err-insufficient-funds)
    
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { reward-amount: (+ (get reward-amount bounty) additional-amount) })
    )
    (ok true)
  )
)

(define-public (set-contract-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount)
    (var-set contract-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-bounty-count)
  (var-get bounty-counter)
)

(define-read-only (get-user-bounties (user principal))
  (map-get? user-bounties { user: user })
)

(define-read-only (get-bounty-attempts (bounty-id uint) (user principal))
  (map-get? bounty-attempts { bounty-id: bounty-id, user: user })
)

(define-read-only (get-contract-fee-rate)
  (var-get contract-fee-rate)
)

(define-read-only (is-bounty-active (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (and 
      (not (get solved bounty))
      (< stacks-block-height (get deadline bounty)))
    false
  )
)

(define-read-only (is-bounty-solvable (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (and 
      (not (get solved bounty))
      (>= stacks-block-height (get deadline bounty)))
    false
  )
)

(define-read-only (get-active-bounties-count)
  (let
    (
      (total-bounties (var-get bounty-counter))
      (current-block stacks-block-height)
    )
    (fold count-active-bounties (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
  )
)

(define-private (count-active-bounties (bounty-id uint) (acc uint))
  (if (is-bounty-active bounty-id)
    (+ acc u1)
    acc
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (calculate-solution-hash (solution (string-ascii 200)))
  (sha256 (unwrap-panic (to-consensus-buff? solution)))
)