;; ------------------------------------------------------------
;; council-governance.clar
;; STX-compatible council governance (multisig DAO)
;; ------------------------------------------------------------

(define-constant ERR-NOT-COUNCIL u100)
(define-constant ERR-PROPOSAL-NOT-FOUND u101)
(define-constant ERR-ALREADY-VOTED u102)
(define-constant ERR-VOTING-CLOSED u103)
(define-constant ERR-INVALID u104)
(define-constant ERR-INVALID-INPUT u105)

;; ------------------------------------------------------------
;; Council state
;; ------------------------------------------------------------

(define-data-var council-size uint u0)
(define-data-var proposal-count uint u0)

;; council members
(define-map council
  { member: principal }
  { active: bool }
)

(define-read-only (is-council (u principal))
  (is-some (map-get? council { member: u }))
)

;; ------------------------------------------------------------
;; Proposals
;; ------------------------------------------------------------

(define-map proposals
  { id: uint }
  {
    creator: principal,
    description: (string-ascii 200),
    start: uint,
    end: uint,
    yes: uint,
    no: uint,
    open: bool
  }
)

;; vote tracking
(define-map votes
  { id: uint, voter: principal }
  { voted: bool }
)

;; ------------------------------------------------------------
;; Initialization (explicit calls, STX-safe)
;; ------------------------------------------------------------

(define-public (add-initial-member (member principal))
  (if (> (var-get council-size) u0)
      (err ERR-INVALID)
      (if (is-eq member tx-sender)
          (err ERR-INVALID-INPUT)
          (begin
            (map-set council { member: member } { active: true })
            (var-set council-size u1)
            (ok true)
          )
      )
  )
)

;; ------------------------------------------------------------
;; Council management
;; ------------------------------------------------------------

(define-public (add-member (new-member principal))
  (if (not (is-council tx-sender))
      (err ERR-NOT-COUNCIL)
      (if (is-eq new-member tx-sender)
          (err ERR-INVALID-INPUT)
          (match (map-get? council { member: new-member })
            some-val (err ERR-INVALID)
            (begin
              (map-set council { member: new-member } { active: true })
              (var-set council-size (+ (var-get council-size) u1))
              (ok true)
            )
          )
      )
  )
)

(define-public (remove-member (member principal))
  (if (not (is-council tx-sender))
      (err ERR-NOT-COUNCIL)
      (if (is-eq member tx-sender)
          (err ERR-INVALID-INPUT)
          (if (<= (var-get council-size) u1)
              (err ERR-INVALID)
              (begin
                (map-delete council { member: member })
                (var-set council-size (- (var-get council-size) u1))
                (ok true)
              )
          )
      )
  )
)

;; ------------------------------------------------------------
;; Create proposal
;; ------------------------------------------------------------

(define-public (create-proposal (description (string-ascii 200)) (duration uint))
  (if (not (is-council tx-sender))
      (err ERR-NOT-COUNCIL)
      (if (or (is-eq duration u0) (is-eq description ""))
          (err ERR-INVALID-INPUT)
          (let ((id (+ (var-get proposal-count) u1)))
            (begin
              (var-set proposal-count id)
              (map-set proposals
                { id: id }
                {
                  creator: tx-sender,
                  description: description,
                  start: u0,
                  end: duration,
                  yes: u0,
                  no: u0,
                  open: true
                }
              )
              (ok id)
            )
          )
      )
  )
)

;; ------------------------------------------------------------
;; Vote
;; ------------------------------------------------------------

(define-public (vote (proposal-id uint) (support bool))
  (if (not (is-council tx-sender))
      (err ERR-NOT-COUNCIL)
      (if (is-eq proposal-id u0)
          (err ERR-INVALID-INPUT)
          (match (map-get? proposals { id: proposal-id })
            some-p
              (if (or (not (get open some-p))
                      (>= u0 (get end some-p)))
                  (err ERR-VOTING-CLOSED)
                  (match (map-get? votes
                           { id: proposal-id, voter: tx-sender })
                    vote-entry (err ERR-ALREADY-VOTED)
                    (begin
                      (map-set votes
                        { id: proposal-id, voter: tx-sender }
                        { voted: true }
                      )
                      (if support
                          (map-set proposals { id: proposal-id }
                            {
                              creator: (get creator some-p),
                              description: (get description some-p),
                              start: (get start some-p),
                              end: (get end some-p),
                              yes: (+ (get yes some-p) u1),
                              no: (get no some-p),
                              open: true
                            })
                          (map-set proposals { id: proposal-id }
                            {
                              creator: (get creator some-p),
                              description: (get description some-p),
                              start: (get start some-p),
                              end: (get end some-p),
                              yes: (get yes some-p),
                              no: (+ (get no some-p) u1),
                              open: true
                            })
                      )
                      (ok true)
                    )
                  )
              )
            (err ERR-PROPOSAL-NOT-FOUND)
          )
      )
  )
)

;; ------------------------------------------------------------
;; Finalize (simple majority)
;; ------------------------------------------------------------

(define-public (finalize (proposal-id uint))
  (if (is-eq proposal-id u0)
      (err ERR-INVALID-INPUT)
      (match (map-get? proposals { id: proposal-id })
        some-p
          (if (not (get open some-p))
              (err ERR-VOTING-CLOSED)
              (begin
                (map-set proposals { id: proposal-id }
                  {
                    creator: (get creator some-p),
                    description: (get description some-p),
                    start: (get start some-p),
                    end: (get end some-p),
                    yes: (get yes some-p),
                    no: (get no some-p),
                    open: false
                  })
                (if (> (get yes some-p) (get no some-p))
                    (ok { result: "passed" })
                    (ok { result: "rejected" })
                )
              )
          )
        (err ERR-PROPOSAL-NOT-FOUND)
      )
  )
)

;; ------------------------------------------------------------
;; Read-only helpers
;; ------------------------------------------------------------

(define-read-only (get-proposal (id uint))
  (map-get? proposals { id: id })
)

(define-read-only (get-council-size)
  (ok (var-get council-size))
)

(define-read-only (is-member (user principal))
  (is-council user)
)
