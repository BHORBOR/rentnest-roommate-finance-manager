;; rentnest-core.clar
;; RentNest Roommate Finance Manager - Core Contract

;; This contract manages roommate households, expenses, and payment settlements on the Stacks blockchain.
;; It allows roommates to create shared households, track recurring and one-time expenses, 
;; maintain running balances between members, and facilitate payment settlements.

;; =============== Error Constants ===============

(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-HOUSEHOLD-EXISTS (err u1002))
(define-constant ERR-HOUSEHOLD-NOT-FOUND (err u1003))
(define-constant ERR-USER-NOT-IN-HOUSEHOLD (err u1004))
(define-constant ERR-USER-ALREADY-IN-HOUSEHOLD (err u1005))
(define-constant ERR-EXPENSE-NOT-FOUND (err u1006))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1007))
(define-constant ERR-INVALID-AMOUNT (err u1008))
(define-constant ERR-INVALID-ALLOCATION (err u1009))
(define-constant ERR-INVALID-EXPENSE-TYPE (err u1010))
(define-constant ERR-MEMBER-HAS-BALANCE (err u1011))
(define-constant ERR-INVALID-PAYMENT (err u1012))
(define-constant ERR-INVALID-PARAMETER (err u1013))

;; =============== Data Structures ===============

;; Keeps track of all household IDs and their creators
(define-map households
  { household-id: uint }
  { 
    name: (string-ascii 100),
    creator: principal,
    created-at: uint,
    active: bool
  }
)

;; Tracks membership of users in households
(define-map household-members
  { household-id: uint, member: principal }
  {
    joined-at: uint,
    allocation-bps: uint,   ;; Basis points for expense allocation (100 = 1%, 10000 = 100%)
    active: bool
  }
)

;; Maps household IDs to a list of member principals
(define-map household-member-list
  { household-id: uint }
  { members: (list 20 principal) }
)

;; Stores expense information
(define-map expenses
  { household-id: uint, expense-id: uint }
  {
    name: (string-ascii 100),
    amount: uint,
    paid-by: principal,
    expense-type: (string-ascii 20),   ;; "one-time" or "recurring"
    recurrence-period: uint,           ;; 0 for one-time, otherwise period in blocks
    created-at: uint,
    allocation-type: (string-ascii 10), ;; "equal" or "custom"
    settled: bool
  }
)

;; Custom expense allocations for when members pay different amounts
(define-map expense-allocations
  { household-id: uint, expense-id: uint, member: principal }
  { allocation-bps: uint }
)

;; Tracks running balances between members (what each owes to others)
(define-map member-balances
  { household-id: uint, from-member: principal, to-member: principal }
  { amount: uint }
)

;; Tracks payment settlements between members
(define-map settlements
  { household-id: uint, settlement-id: uint }
  {
    from-member: principal,
    to-member: principal,
    amount: uint,
    timestamp: uint,
    tx-id: (optional (buff 32))
  }
)

;; Counter for household IDs
(define-data-var next-household-id uint u1)

;; Counters for expense and settlement IDs (per household)
(define-map household-counters
  { household-id: uint }
  { 
    next-expense-id: uint,
    next-settlement-id: uint
  }
)

;; =============== Private Functions ===============

;; Get the next household ID and increment the counter
(define-private (get-next-household-id)
  (let ((current-id (var-get next-household-id)))
    (var-set next-household-id (+ current-id u1))
    current-id
  )
)

;; Get the next expense ID for a household
(define-private (get-next-expense-id (household-id uint))
  (let (
    (counters (default-to { next-expense-id: u1, next-settlement-id: u1 } 
               (map-get? household-counters { household-id: household-id })))
    (next-id (get next-expense-id counters))
  )
    (map-set household-counters 
      { household-id: household-id } 
      (merge counters { next-expense-id: (+ next-id u1) })
    )
    next-id
  )
)

;; Get the next settlement ID for a household
(define-private (get-next-settlement-id (household-id uint))
  (let (
    (counters (default-to { next-expense-id: u1, next-settlement-id: u1 } 
               (map-get? household-counters { household-id: household-id })))
    (next-id (get next-settlement-id counters))
  )
    (map-set household-counters 
      { household-id: household-id } 
      (merge counters { next-settlement-id: (+ next-id u1) })
    )
    next-id
  )
)

;; Check if a user is a member of a household
(define-private (is-member (household-id uint) (user principal))
  (match (map-get? household-members { household-id: household-id, member: user })
    member (and (get active member) true)
    false
  )
)

;; Check if user is authorized to manage a household (currently only the creator)
(define-private (is-household-admin (household-id uint) (user principal))
  (match (map-get? households { household-id: household-id })
    household (is-eq (get creator household) user)
    false
  )
)

;; Calculate equal allocation in basis points for all members
(define-private (calculate-equal-allocation (household-id uint))
  (match (map-get? household-member-list { household-id: household-id })
    member-list (let ((member-count (len (get members member-list))))
      (if (> member-count u0)
        (/ u10000 member-count)  ;; Equal division (10000 basis points = 100%)
        u0
      ))
    u0
  )
)

;; Update the balance between two members
(define-private (update-balance (household-id uint) (from principal) (to principal) (amount uint))
  (let (
    (current-balance (default-to { amount: u0 } 
                     (map-get? member-balances { household-id: household-id, from-member: from, to-member: to })))
    (new-amount (+ (get amount current-balance) amount))
  )
    (map-set member-balances
      { household-id: household-id, from-member: from, to-member: to }
      { amount: new-amount }
    )
    (ok true)
  )
)

;; Add member to the household member list
(define-private (add-to-member-list (household-id uint) (new-member principal))
  (let (
    (current-list-struct (default-to { members: (list) } 
                  (map-get? household-member-list { household-id: household-id })))
    (updated-members-list (unwrap! (as-max-len? (append (get members current-list-struct) new-member) u20) ERR-INVALID-PARAMETER))
  )
    (map-set household-member-list 
      { household-id: household-id } 
      { members: updated-members-list }
    )
    (ok true)
  )
)

;; Helper for fold in has-outstanding-balance
(define-private (check-member-balance-accumulator (other-member principal) (params (tuple (household-id uint) (member principal) (has-balance bool))))
  (let ((h-id (get household-id params))
        (current-member (get member params))
        (current-has-balance (get has-balance params)))
    (if current-has-balance
      true ;; If a balance was already found, no need to check further
      (let (
        (from-balance (default-to { amount: u0 } 
                      (map-get? member-balances { 
                        household-id: h-id, 
                        from-member: current-member, 
                        to-member: other-member 
                      })))
        (to-balance (default-to { amount: u0 } 
                    (map-get? member-balances { 
                      household-id: h-id, 
                      from-member: other-member, 
                      to-member: current-member 
                    })))
      )
        (or (> (get amount from-balance) u0) (> (get amount to-balance) u0))
      )
    )
  )
)

;; =============== Read-Only Functions ===============

;; Get household information
(define-read-only (get-household (household-id uint))
  (map-get? households { household-id: household-id })
)

;; Get member information for a household
(define-read-only (get-household-member (household-id uint) (member principal))
  (map-get? household-members { household-id: household-id, member: member })
)

;; Get all members of a household
(define-read-only (get-household-members (household-id uint))
  (map-get? household-member-list { household-id: household-id })
)

;; Get an expense's details
(define-read-only (get-expense (household-id uint) (expense-id uint))
  (map-get? expenses { household-id: household-id, expense-id: expense-id })
)

;; Get a member's allocation for a specific expense
(define-read-only (get-expense-allocation (household-id uint) (expense-id uint) (member principal))
  (map-get? expense-allocations { household-id: household-id, expense-id: expense-id, member: member })
)

;; Get the balance between two members
(define-read-only (get-member-balance (household-id uint) (from principal) (to principal))
  (default-to { amount: u0 } 
    (map-get? member-balances { household-id: household-id, from-member: from, to-member: to })
  )
)

;; Get a settlement's details
(define-read-only (get-settlement (household-id uint) (settlement-id uint))
  (map-get? settlements { household-id: household-id, settlement-id: settlement-id })
)

;; Check if a household exists
(define-read-only (household-exists (household-id uint))
  (is-some (map-get? households { household-id: household-id }))
)

;; =============== Public Functions ===============

;; Create a new household
(define-public (create-household (name (string-ascii 100)))
  (let (
    (household-id (get-next-household-id))
    (caller tx-sender)
    (block-height block-height)
  )
    ;; Set household details
    (map-set households 
      { household-id: household-id }
      { 
        name: name,
        creator: caller,
        created-at: block-height,
        active: true
      }
    )
    
    ;; Initialize counters for this household
    (map-set household-counters
      { household-id: household-id }
      { next-expense-id: u1, next-settlement-id: u1 }
    )
    
    ;; Add creator as first member with 100% allocation
    (map-set household-members
      { household-id: household-id, member: caller }
      {
        joined-at: block-height,
        allocation-bps: u10000,  ;; 100% allocation until more members are added
        active: true
      }
    )
    
    ;; Initialize member list with the creator
    (map-set household-member-list
      { household-id: household-id }
      { members: (list caller) }
    )
    
    (ok household-id)
  )
)

;; Add a member to a household
(define-public (add-member (household-id uint) (new-member principal))
  (let (
    (caller tx-sender)
    (block-height block-height)
  )
    ;; Verify caller is admin
    (asserts! (is-household-admin household-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Verify household exists
    (asserts! (household-exists household-id) ERR-HOUSEHOLD-NOT-FOUND)
    
    ;; Verify new member isn't already a member
    (asserts! (not (is-member household-id new-member)) ERR-USER-ALREADY-IN-HOUSEHOLD)
    
    ;; Add member with equal allocation
    (try! (add-to-member-list household-id new-member))
    
    ;; Calculate equal allocation for all members
    (let ((equal-allocation (calculate-equal-allocation household-id)))
      ;; Update all existing members to have equal allocation
      (map-set household-members
        { household-id: household-id, member: new-member }
        {
          joined-at: block-height,
          allocation-bps: equal-allocation,
          active: true
        }
      )
      
      ;; Return the ID of the newly added member
      (ok true)
    )
  )
)

;; Update a member's allocation percentage
(define-public (update-member-allocation (household-id uint) (member principal) (allocation-bps uint))
  (let (
    (caller tx-sender)
  )
    ;; Verify caller is admin
    (asserts! (is-household-admin household-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Verify household exists
    (asserts! (household-exists household-id) ERR-HOUSEHOLD-NOT-FOUND)
    
    ;; Verify member exists in household
    (asserts! (is-member household-id member) ERR-USER-NOT-IN-HOUSEHOLD)
    
    ;; Verify allocation is valid (0-10000)
    (asserts! (<= allocation-bps u10000) ERR-INVALID-ALLOCATION)
    
    ;; Update member allocation
    ;; (match (map-get? household-members { household-id: household-id, member: member })
    ;;   member-info 
    ;;     (map-set household-members
    ;;       { household-id: household-id, member: member }
    ;;       (merge member-info { allocation-bps: allocation-bps })
    ;;     )
    ;;   (err ERR-USER-NOT-IN-HOUSEHOLD)
    ;; )
    
    (ok true)
  )
)

;; Settle a payment between members
(define-public (settle-payment (household-id uint) (to-member principal) (amount uint))
  (let (
    (caller tx-sender)
    (settlement-id (get-next-settlement-id household-id))
    (block-height block-height)
  )
    ;; Verify members are in the household
    (asserts! (is-member household-id caller) ERR-USER-NOT-IN-HOUSEHOLD)
    (asserts! (is-member household-id to-member) ERR-USER-NOT-IN-HOUSEHOLD)
    
    ;; Verify household exists
    (asserts! (household-exists household-id) ERR-HOUSEHOLD-NOT-FOUND)
    
    ;; Verify amount is greater than zero
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Get current balance
    (let (
      (current-balance (get amount (get-member-balance household-id caller to-member)))
    )
      ;; Verify caller has sufficient balance to settle
      (asserts! (>= current-balance amount) ERR-INSUFFICIENT-FUNDS)
      
      ;; Update balance (reduce what caller owes)
      (map-set member-balances
        { household-id: household-id, from-member: caller, to-member: to-member }
        { amount: (- current-balance amount) }
      )
      
      ;; Record the settlement
      (map-set settlements
        { household-id: household-id, settlement-id: settlement-id }
        {
          from-member: caller,
          to-member: to-member,
          amount: amount,
          timestamp: block-height,
          tx-id: none
        }
      )
      
      (ok settlement-id)
    )
  )
)