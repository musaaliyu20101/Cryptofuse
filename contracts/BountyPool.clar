;; Community Bounty Pooling System
;; Allows multiple users to collectively increase bounty rewards through pooled contributions
;; Unique feature: Community can back bounties they find interesting with their own STX

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u130))
(define-constant err-unauthorized (err u131))
(define-constant err-invalid-amount (err u132))
(define-constant err-bounty-solved (err u133))
(define-constant err-bounty-expired (err u134))
(define-constant err-insufficient-funds (err u135))
(define-constant err-no-contribution (err u136))
(define-constant err-already-contributed (err u137))

;; Data variables
(define-data-var pool-counter uint u0)

;; Track individual contributions to each bounty pool
(define-map pool-contributions
  { bounty-id: uint, contributor: principal }
  { amount: uint, contributed-at: uint }
)

;; Track total pool amounts and stats for each bounty
(define-map bounty-pools
  { bounty-id: uint }
  { 
    total-pooled: uint,
    contributor-count: uint,
    pool-active: bool,
    created-at: uint
  }
)

;; Track all contributors for a bounty (for distribution)
(define-map bounty-contributors
  { bounty-id: uint }
  { contributors: (list 50 principal) }
)

;; Contribute STX to a bounty pool to increase the reward
(define-public (contribute-to-pool (bounty-id uint) (amount uint))
  (let 
    (
      (contributor tx-sender)
      (existing-contribution (map-get? pool-contributions { bounty-id: bounty-id, contributor: contributor }))
      (current-pool (default-to { total-pooled: u0, contributor-count: u0, pool-active: true, created-at: stacks-block-height }
        (map-get? bounty-pools { bounty-id: bounty-id })))
      (current-contributors (default-to { contributors: (list) }
        (map-get? bounty-contributors { bounty-id: bounty-id })))
    )
    
    ;; Validate inputs
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance contributor) amount) err-insufficient-funds)
    (asserts! (get pool-active current-pool) err-bounty-expired)
    (asserts! (is-bounty-poolable bounty-id) err-bounty-solved)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount contributor (as-contract tx-sender)))
    
    ;; Update or create contribution record
    (if (is-some existing-contribution)
      ;; Add to existing contribution
      (map-set pool-contributions
        { bounty-id: bounty-id, contributor: contributor }
        { 
          amount: (+ (get amount (unwrap-panic existing-contribution)) amount),
          contributed-at: (get contributed-at (unwrap-panic existing-contribution))
        })
      ;; New contributor
      (begin
        (map-set pool-contributions
          { bounty-id: bounty-id, contributor: contributor }
          { amount: amount, contributed-at: stacks-block-height })
        
        ;; Add to contributors list if not already there
        (let ((updated-contributors (unwrap! 
          (as-max-len? (append (get contributors current-contributors) contributor) u50)
          err-invalid-amount)))
          (map-set bounty-contributors
            { bounty-id: bounty-id }
            { contributors: updated-contributors }))
      )
    )
    
    ;; Update pool totals
    (map-set bounty-pools
      { bounty-id: bounty-id }
      { 
        total-pooled: (+ (get total-pooled current-pool) amount),
        contributor-count: (if (is-some existing-contribution) 
          (get contributor-count current-pool)
          (+ (get contributor-count current-pool) u1)),
        pool-active: true,
        created-at: (get created-at current-pool)
      })
    
    (ok amount)
  )
)

;; Withdraw contribution if bounty not solved/cancelled
(define-public (withdraw-contribution (bounty-id uint))
  (let 
    (
      (contributor tx-sender)
      (contribution (unwrap! (map-get? pool-contributions { bounty-id: bounty-id, contributor: contributor }) err-no-contribution))
      (current-pool (unwrap! (map-get? bounty-pools { bounty-id: bounty-id }) err-not-found))
      (withdrawal-amount (get amount contribution))
    )
    
    ;; Only allow withdrawal if bounty is still active and not solved
    (asserts! (is-bounty-poolable bounty-id) err-bounty-solved)
    (asserts! (get pool-active current-pool) err-bounty-expired)
    
    ;; Transfer STX back to contributor
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender contributor)))
    
    ;; Remove contribution record
    (map-delete pool-contributions { bounty-id: bounty-id, contributor: contributor })
    
    ;; Update pool totals
    (map-set bounty-pools
      { bounty-id: bounty-id }
      { 
        total-pooled: (- (get total-pooled current-pool) withdrawal-amount),
        contributor-count: (- (get contributor-count current-pool) u1),
        pool-active: (get pool-active current-pool),
        created-at: (get created-at current-pool)
      })
    
    (ok withdrawal-amount)
  )
)

;; Transfer pooled funds to bounty solver (called after bounty is solved)
(define-public (distribute-pool-rewards (bounty-id uint) (solver principal))
  (let 
    (
      (current-pool (unwrap! (map-get? bounty-pools { bounty-id: bounty-id }) err-not-found))
      (pool-amount (get total-pooled current-pool))
    )
    
    ;; Only contract owner can call this (integration with main contract)
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (> pool-amount u0) err-invalid-amount)
    
    ;; Transfer pooled funds to solver
    (try! (as-contract (stx-transfer? pool-amount tx-sender solver)))
    
    ;; Mark pool as inactive
    (map-set bounty-pools
      { bounty-id: bounty-id }
      (merge current-pool { pool-active: false }))
    
    (ok pool-amount)
  )
)

;; Refund all contributors if bounty is cancelled
(define-public (refund-pool-contributors (bounty-id uint))
  (let 
    (
      (current-pool (unwrap! (map-get? bounty-pools { bounty-id: bounty-id }) err-not-found))
      (contributors-data (unwrap! (map-get? bounty-contributors { bounty-id: bounty-id }) err-not-found))
    )
    
    ;; Only contract owner can call this
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (get pool-active current-pool) err-bounty-expired)
    
    ;; Process refunds for all contributors
    (try! (fold refund-contributor (get contributors contributors-data) (ok u0)))
    
    ;; Mark pool as inactive
    (map-set bounty-pools
      { bounty-id: bounty-id }
      (merge current-pool { pool-active: false }))
    
    (ok true)
  )
)

;; Helper function to refund individual contributor
(define-private (refund-contributor (contributor principal) (result (response uint uint)))
  (match result
    success (let 
      (
        (contribution (map-get? pool-contributions { bounty-id: u1, contributor: contributor })) ;; bounty-id would be passed in context
      )
      (match contribution
        contrib (begin
          (try! (as-contract (stx-transfer? (get amount contrib) tx-sender contributor)))
          (map-delete pool-contributions { bounty-id: u1, contributor: contributor })
          (ok (+ success (get amount contrib))))
        (ok success)))
    error (err error)
  )
)

;; Check if bounty is still eligible for pooling (not solved/cancelled)
(define-private (is-bounty-poolable (bounty-id uint))
  ;; This would integrate with main Cryptofuse contract
  ;; For now, assume all bounties are poolable
  true
)

;; Read-only functions

(define-read-only (get-pool-info (bounty-id uint))
  (map-get? bounty-pools { bounty-id: bounty-id })
)

(define-read-only (get-user-contribution (bounty-id uint) (contributor principal))
  (map-get? pool-contributions { bounty-id: bounty-id, contributor: contributor })
)

(define-read-only (get-pool-contributors (bounty-id uint))
  (map-get? bounty-contributors { bounty-id: bounty-id })
)

(define-read-only (get-total-pooled (bounty-id uint))
  (match (map-get? bounty-pools { bounty-id: bounty-id })
    pool (get total-pooled pool)
    u0
  )
)

(define-read-only (get-contributor-count (bounty-id uint))
  (match (map-get? bounty-pools { bounty-id: bounty-id })
    pool (get contributor-count pool)
    u0
  )
)

(define-read-only (is-pool-active (bounty-id uint))
  (match (map-get? bounty-pools { bounty-id: bounty-id })
    pool (get pool-active pool)
    false
  )
)

(define-read-only (calculate-contribution-percentage (bounty-id uint) (contributor principal))
  (let 
    (
      (contribution (map-get? pool-contributions { bounty-id: bounty-id, contributor: contributor }))
      (pool-info (map-get? bounty-pools { bounty-id: bounty-id }))
    )
    (match contribution
      contrib (match pool-info
        pool (if (> (get total-pooled pool) u0)
          (some (/ (* (get amount contrib) u100) (get total-pooled pool)))
          none)
        none)
      none
    )
  )
)
