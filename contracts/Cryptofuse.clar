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
(define-constant err-collaboration-closed (err u110))
(define-constant err-invalid-contribution (err u111))
(define-constant err-already-voted (err u112))
(define-constant err-insufficient-votes (err u113))
(define-constant err-invalid-reward-share (err u114))
(define-constant err-contribution-not-found (err u115))
(define-constant err-self-vote (err u116))
(define-constant err-escalation-not-enabled (err u117))
(define-constant err-invalid-escalation-type (err u118))
(define-constant err-escalation-limit-reached (err u119))
(define-constant err-invalid-escalation-params (err u120))
(define-constant err-insufficient-escalation-funds (err u121))

;; Escalation type constants
(define-constant escalation-linear u1)
(define-constant escalation-exponential u2)
(define-constant escalation-stepped u3)

(define-data-var bounty-counter uint u0)
(define-data-var contract-fee-rate uint u250)
(define-data-var contribution-counter uint u0)
(define-data-var min-votes-required uint u3)
(define-data-var max-escalation-multiplier uint u500) ;; 5x maximum escalation

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

(define-map bounty-contributions
  { contribution-id: uint }
  {
    bounty-id: uint,
    contributor: principal,
    content: (string-ascii 300),
    contribution-type: (string-ascii 20),
    reward-share: uint,
    votes-for: uint,
    votes-against: uint,
    approved: bool,
    created-at: uint
  }
)

(define-map contribution-votes
  { contribution-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

(define-map bounty-collaborators
  { bounty-id: uint, collaborator: principal }
  { total-contributions: uint, total-reward-share: uint }
)

(define-map bounty-collaboration-settings
  { bounty-id: uint }
  {
    collaboration-enabled: bool,
    max-collaborators: uint,
    min-contribution-votes: uint,
    collaboration-deadline: uint
  }
)

;; Reward escalation data structures
(define-map bounty-escalation-settings
  { bounty-id: uint }
  {
    escalation-enabled: bool,
    escalation-type: uint, ;; 1=linear, 2=exponential, 3=stepped
    escalation-rate: uint, ;; percentage increase per interval
    escalation-interval: uint, ;; blocks between escalations
    max-reward: uint, ;; maximum reward cap
    escalation-fund: uint, ;; available funds for escalation
    last-escalation-block: uint
  }
)

;; Track escalation history for analytics
(define-map escalation-history
  { bounty-id: uint, escalation-number: uint }
  {
    previous-reward: uint,
    new-reward: uint,
    escalation-block: uint,
    escalation-type: uint
  }
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

(define-public (enable-bounty-collaboration 
  (bounty-id uint)
  (max-collaborators uint)
  (collaboration-deadline uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (> max-collaborators u0) err-invalid-amount)
    (asserts! (and (> collaboration-deadline current-block) (< collaboration-deadline (get deadline bounty))) err-invalid-deadline)
    
    (map-set bounty-collaboration-settings
      { bounty-id: bounty-id }
      {
        collaboration-enabled: true,
        max-collaborators: max-collaborators,
        min-contribution-votes: (var-get min-votes-required),
        collaboration-deadline: collaboration-deadline
      }
    )
    (ok true)
  )
)

(define-public (submit-contribution 
  (bounty-id uint)
  (content (string-ascii 300))
  (contribution-type (string-ascii 20))
  (reward-share uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (collaboration-settings (unwrap! (map-get? bounty-collaboration-settings { bounty-id: bounty-id }) err-collaboration-closed))
      (current-block stacks-block-height)
      (contribution-id (+ (var-get contribution-counter) u1))
      (collaborator-data (default-to { total-contributions: u0, total-reward-share: u0 }
        (map-get? bounty-collaborators { bounty-id: bounty-id, collaborator: tx-sender })))
    )
    (asserts! (get collaboration-enabled collaboration-settings) err-collaboration-closed)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (< current-block (get collaboration-deadline collaboration-settings)) err-collaboration-closed)
    (asserts! (> (len content) u0) err-invalid-contribution)
    (asserts! (and (> reward-share u0) (<= reward-share u50)) err-invalid-reward-share)
    (asserts! (not (is-eq tx-sender (get creator bounty))) err-unauthorized)
    
    (map-set bounty-contributions
      { contribution-id: contribution-id }
      {
        bounty-id: bounty-id,
        contributor: tx-sender,
        content: content,
        contribution-type: contribution-type,
        reward-share: reward-share,
        votes-for: u0,
        votes-against: u0,
        approved: false,
        created-at: current-block
      }
    )
    
    (map-set bounty-collaborators
      { bounty-id: bounty-id, collaborator: tx-sender }
      {
        total-contributions: (+ (get total-contributions collaborator-data) u1),
        total-reward-share: (+ (get total-reward-share collaborator-data) reward-share)
      }
    )
    
    (var-set contribution-counter contribution-id)
    (ok contribution-id)
  )
)

(define-public (vote-on-contribution (contribution-id uint) (vote bool))
  (let
    (
      (contribution (unwrap! (map-get? bounty-contributions { contribution-id: contribution-id }) err-contribution-not-found))
      (bounty (unwrap! (map-get? bounties { bounty-id: (get bounty-id contribution) }) err-not-found))
      (existing-vote (map-get? contribution-votes { contribution-id: contribution-id, voter: tx-sender }))
      (current-block stacks-block-height)
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (not (is-eq tx-sender (get contributor contribution))) err-self-vote)
    (asserts! (not (get solved bounty)) err-already-solved)
    
    (map-set contribution-votes
      { contribution-id: contribution-id, voter: tx-sender }
      { vote: vote, voted-at: current-block }
    )
    
    (if vote
      (map-set bounty-contributions
        { contribution-id: contribution-id }
        (merge contribution { votes-for: (+ (get votes-for contribution) u1) })
      )
      (map-set bounty-contributions
        { contribution-id: contribution-id }
        (merge contribution { votes-against: (+ (get votes-against contribution) u1) })
      )
    )
    (ok true)
  )
)

(define-public (approve-contribution (contribution-id uint))
  (let
    (
      (contribution (unwrap! (map-get? bounty-contributions { contribution-id: contribution-id }) err-contribution-not-found))
      (bounty (unwrap! (map-get? bounties { bounty-id: (get bounty-id contribution) }) err-not-found))
      (collaboration-settings (unwrap! (map-get? bounty-collaboration-settings { bounty-id: (get bounty-id contribution) }) err-collaboration-closed))
      (min-votes (get min-contribution-votes collaboration-settings))
      (total-votes (+ (get votes-for contribution) (get votes-against contribution)))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get approved contribution)) err-already-solved)
    (asserts! (>= total-votes min-votes) err-insufficient-votes)
    (asserts! (> (get votes-for contribution) (get votes-against contribution)) err-invalid-contribution)
    
    (map-set bounty-contributions
      { contribution-id: contribution-id }
      (merge contribution { approved: true })
    )
    (ok true)
  )
)

(define-public (distribute-collaborative-rewards (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (collaboration-settings (unwrap! (map-get? bounty-collaboration-settings { bounty-id: bounty-id }) err-collaboration-closed))
      (total-reward (get reward-amount bounty))
      (fee-amount (/ (* total-reward (var-get contract-fee-rate)) u10000))
      (collaborative-pool (/ (- total-reward fee-amount) u2))
      (solver-reward (- total-reward fee-amount collaborative-pool))
    )
    (asserts! (get solved bounty) err-not-found)
    (asserts! (get collaboration-enabled collaboration-settings) err-collaboration-closed)
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    
    (try! (as-contract (stx-transfer? solver-reward tx-sender (unwrap-panic (get solver bounty)))))
    (try! (as-contract (stx-transfer? fee-amount tx-sender contract-owner)))
    
    (ok collaborative-pool)
  )
)

(define-public (claim-collaboration-reward (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (collaborator-data (unwrap! (map-get? bounty-collaborators { bounty-id: bounty-id, collaborator: tx-sender }) err-not-found))
      (total-reward (get reward-amount bounty))
      (fee-amount (/ (* total-reward (var-get contract-fee-rate)) u10000))
      (collaborative-pool (/ (- total-reward fee-amount) u2))
      (collaborator-reward (/ (* collaborative-pool (get total-reward-share collaborator-data)) u100))
    )
    (asserts! (get solved bounty) err-not-found)
    (asserts! (> (get total-reward-share collaborator-data) u0) err-invalid-reward-share)
    
    (map-delete bounty-collaborators { bounty-id: bounty-id, collaborator: tx-sender })
    (try! (as-contract (stx-transfer? collaborator-reward tx-sender tx-sender)))
    
    (ok collaborator-reward)
  )
)

(define-public (set-min-votes-required (new-min-votes uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-min-votes u1) (<= new-min-votes u10)) err-invalid-amount)
    (var-set min-votes-required new-min-votes)
    (ok true)
  )
)

(define-read-only (get-contribution (contribution-id uint))
  (map-get? bounty-contributions { contribution-id: contribution-id })
)

(define-read-only (get-collaboration-settings (bounty-id uint))
  (map-get? bounty-collaboration-settings { bounty-id: bounty-id })
)

(define-read-only (get-collaborator-data (bounty-id uint) (collaborator principal))
  (map-get? bounty-collaborators { bounty-id: bounty-id, collaborator: collaborator })
)

(define-read-only (get-contribution-vote (contribution-id uint) (voter principal))
  (map-get? contribution-votes { contribution-id: contribution-id, voter: voter })
)

(define-read-only (get-contribution-count)
  (var-get contribution-counter)
)

(define-read-only (get-min-votes-required)
  (var-get min-votes-required)
)

(define-read-only (is-collaboration-enabled (bounty-id uint))
  (match (map-get? bounty-collaboration-settings { bounty-id: bounty-id })
    settings (get collaboration-enabled settings)
    false
  )
)

(define-read-only (calculate-collaborator-reward (bounty-id uint) (collaborator principal))
  (match (map-get? bounty-collaborators { bounty-id: bounty-id, collaborator: collaborator })
    collaborator-data
      (match (map-get? bounties { bounty-id: bounty-id })
        bounty
          (let
            (
              (total-reward (get reward-amount bounty))
              (fee-amount (/ (* total-reward (var-get contract-fee-rate)) u10000))
              (collaborative-pool (/ (- total-reward fee-amount) u2))
              (reward-share (get total-reward-share collaborator-data))
            )
            (some (/ (* collaborative-pool reward-share) u100))
          )
        none
      )
    none
  )
)

;; Dynamic Reward Escalation Functions

(define-public (setup-bounty-escalation
  (bounty-id uint)
  (escalation-type uint)
  (escalation-rate uint) 
  (escalation-interval uint)
  (max-reward uint)
  (escalation-fund uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (or (is-eq escalation-type escalation-linear) 
                  (or (is-eq escalation-type escalation-exponential) 
                      (is-eq escalation-type escalation-stepped))) err-invalid-escalation-type)
    (asserts! (and (> escalation-rate u0) (<= escalation-rate u100)) err-invalid-escalation-params)
    (asserts! (> escalation-interval u0) err-invalid-escalation-params)
    (asserts! (> max-reward (get reward-amount bounty)) err-invalid-escalation-params)
    (asserts! (>= (stx-get-balance tx-sender) escalation-fund) err-insufficient-escalation-funds)
    
    ;; Transfer escalation funds to contract
    (try! (stx-transfer? escalation-fund tx-sender (as-contract tx-sender)))
    
    (map-set bounty-escalation-settings
      { bounty-id: bounty-id }
      {
        escalation-enabled: true,
        escalation-type: escalation-type,
        escalation-rate: escalation-rate,
        escalation-interval: escalation-interval,
        max-reward: max-reward,
        escalation-fund: escalation-fund,
        last-escalation-block: current-block
      }
    )
    (ok true)
  )
)

(define-public (trigger-escalation (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (escalation-settings (unwrap! (map-get? bounty-escalation-settings { bounty-id: bounty-id }) err-escalation-not-enabled))
      (current-block stacks-block-height)
      (blocks-since-last (- current-block (get last-escalation-block escalation-settings)))
      (current-reward (get reward-amount bounty))
      (new-reward (calculate-escalated-reward current-reward escalation-settings))
      (escalation-cost (- new-reward current-reward))
    )
    (asserts! (get escalation-enabled escalation-settings) err-escalation-not-enabled)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (>= blocks-since-last (get escalation-interval escalation-settings)) err-invalid-escalation-params)
    (asserts! (<= new-reward (get max-reward escalation-settings)) err-escalation-limit-reached)
    (asserts! (>= (get escalation-fund escalation-settings) escalation-cost) err-insufficient-escalation-funds)
    
    ;; Update bounty reward
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { reward-amount: new-reward })
    )
    
    ;; Update escalation settings
    (map-set bounty-escalation-settings
      { bounty-id: bounty-id }
      (merge escalation-settings 
        { 
          escalation-fund: (- (get escalation-fund escalation-settings) escalation-cost),
          last-escalation-block: current-block
        })
    )
    
    ;; Record escalation history
    (map-set escalation-history
      { bounty-id: bounty-id, escalation-number: (get-escalation-count bounty-id) }
      {
        previous-reward: current-reward,
        new-reward: new-reward,
        escalation-block: current-block,
        escalation-type: (get escalation-type escalation-settings)
      }
    )
    
    (ok new-reward)
  )
)

(define-public (add-escalation-funds (bounty-id uint) (additional-funds uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (escalation-settings (unwrap! (map-get? bounty-escalation-settings { bounty-id: bounty-id }) err-escalation-not-enabled))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    (asserts! (> additional-funds u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) additional-funds) err-insufficient-funds)
    
    (try! (stx-transfer? additional-funds tx-sender (as-contract tx-sender)))
    
    (map-set bounty-escalation-settings
      { bounty-id: bounty-id }
      (merge escalation-settings 
        { escalation-fund: (+ (get escalation-fund escalation-settings) additional-funds) })
    )
    (ok true)
  )
)

(define-public (disable-escalation (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (escalation-settings (unwrap! (map-get? bounty-escalation-settings { bounty-id: bounty-id }) err-escalation-not-enabled))
      (remaining-funds (get escalation-fund escalation-settings))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (not (get solved bounty)) err-already-solved)
    
    ;; Return remaining escalation funds to creator
    (if (> remaining-funds u0)
      (try! (as-contract (stx-transfer? remaining-funds tx-sender (get creator bounty))))
      true
    )
    
    (map-set bounty-escalation-settings
      { bounty-id: bounty-id }
      (merge escalation-settings 
        { 
          escalation-enabled: false,
          escalation-fund: u0
        })
    )
    (ok remaining-funds)
  )
)

(define-public (emergency-escalation-halt (bounty-id uint))
  (let
    (
      (escalation-settings (unwrap! (map-get? bounty-escalation-settings { bounty-id: bounty-id }) err-escalation-not-enabled))
      (remaining-funds (get escalation-fund escalation-settings))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Return remaining funds to bounty creator
    (if (> remaining-funds u0)
      (try! (as-contract (stx-transfer? remaining-funds tx-sender contract-owner)))
      true
    )
    
    (map-delete bounty-escalation-settings { bounty-id: bounty-id })
    (ok remaining-funds)
  )
)

(define-public (set-max-escalation-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-multiplier u100) (<= new-multiplier u1000)) err-invalid-amount)
    (var-set max-escalation-multiplier new-multiplier)
    (ok true)
  )
)

;; Private helper functions for escalation calculations
(define-private (calculate-escalated-reward (current-reward uint) (settings (tuple (escalation-enabled bool) (escalation-type uint) (escalation-rate uint) (escalation-interval uint) (max-reward uint) (escalation-fund uint) (last-escalation-block uint))))
  (let
    (
      (escalation-type (get escalation-type settings))
      (escalation-rate (get escalation-rate settings))
      (max-reward-cap (get max-reward settings))
    )
    (if (is-eq escalation-type escalation-linear)
      ;; Linear escalation: current + (current * rate / 100)
      (let ((new-reward (+ current-reward (/ (* current-reward escalation-rate) u100))))
        (if (<= new-reward max-reward-cap) new-reward max-reward-cap))
      (if (is-eq escalation-type escalation-exponential)
        ;; Exponential escalation: current * (1 + rate/100)
        (let ((new-reward (/ (* current-reward (+ u100 escalation-rate)) u100)))
          (if (<= new-reward max-reward-cap) new-reward max-reward-cap))
        ;; Stepped escalation: current + fixed amount based on rate
        (let ((new-reward (+ current-reward (/ (* current-reward escalation-rate) u50))))
          (if (<= new-reward max-reward-cap) new-reward max-reward-cap))
      )
    )
  )
)

(define-private (get-escalation-count (bounty-id uint))
  (let
    (
      (check-escalation-1 (map-get? escalation-history { bounty-id: bounty-id, escalation-number: u1 }))
      (check-escalation-2 (map-get? escalation-history { bounty-id: bounty-id, escalation-number: u2 }))
      (check-escalation-3 (map-get? escalation-history { bounty-id: bounty-id, escalation-number: u3 }))
      (check-escalation-4 (map-get? escalation-history { bounty-id: bounty-id, escalation-number: u4 }))
      (check-escalation-5 (map-get? escalation-history { bounty-id: bounty-id, escalation-number: u5 }))
    )
    (if (is-some check-escalation-5) u6
      (if (is-some check-escalation-4) u5
        (if (is-some check-escalation-3) u4
          (if (is-some check-escalation-2) u3
            (if (is-some check-escalation-1) u2
              u1
            )
          )
        )
      )
    )
  )
)

;; Read-only functions for escalation system
(define-read-only (get-escalation-settings (bounty-id uint))
  (map-get? bounty-escalation-settings { bounty-id: bounty-id })
)

(define-read-only (get-escalation-history (bounty-id uint) (escalation-number uint))
  (map-get? escalation-history { bounty-id: bounty-id, escalation-number: escalation-number })
)

(define-read-only (is-escalation-due (bounty-id uint))
  (match (map-get? bounty-escalation-settings { bounty-id: bounty-id })
    settings
      (if (get escalation-enabled settings)
        (let
          (
            (current-block stacks-block-height)
            (blocks-since-last (- current-block (get last-escalation-block settings)))
            (interval (get escalation-interval settings))
          )
          (>= blocks-since-last interval)
        )
        false
      )
    false
  )
)

(define-read-only (calculate-next-reward (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty
      (match (map-get? bounty-escalation-settings { bounty-id: bounty-id })
        settings
          (if (get escalation-enabled settings)
            (some (calculate-escalated-reward (get reward-amount bounty) settings))
            none
          )
        none
      )
    none
  )
)

(define-read-only (get-max-escalation-multiplier)
  (var-get max-escalation-multiplier)
)

(define-read-only (get-escalation-stats (bounty-id uint))
  (match (map-get? bounty-escalation-settings { bounty-id: bounty-id })
    settings
      (some {
        escalation-enabled: (get escalation-enabled settings),
        escalation-count: (- (get-escalation-count bounty-id) u1),
        remaining-funds: (get escalation-fund settings),
        next-escalation-due: (is-escalation-due bounty-id)
      })
    none
  )
)





