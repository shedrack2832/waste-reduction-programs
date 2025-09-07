;; recycling-coordinator
;; A smart contract for waste reduction programs that handles pickup scheduling,
;; sorting verification, material tracking, and environmental impact measurement

;; constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PICKUP_NOT_FOUND (err u101))
(define-constant ERR_INVALID_MATERIAL_TYPE (err u102))
(define-constant ERR_INVALID_QUANTITY (err u103))
(define-constant ERR_PICKUP_ALREADY_VERIFIED (err u104))
(define-constant ERR_INVALID_STATUS (err u105))
(define-constant CONTRACT_OWNER tx-sender)

;; Material types enum
(define-constant PLASTIC u1)
(define-constant GLASS u2)
(define-constant METAL u3)
(define-constant PAPER u4)
(define-constant ORGANIC u5)
(define-constant ELECTRONIC u6)

;; Pickup status enum
(define-constant STATUS_SCHEDULED u1)
(define-constant STATUS_COLLECTED u2)
(define-constant STATUS_VERIFIED u3)
(define-constant STATUS_PROCESSED u4)

;; data maps and vars
(define-map pickup-schedules 
  { pickup-id: uint }
  {
    requester: principal,
    location: (string-ascii 100),
    scheduled-date: uint,
    status: uint,
    materials: (list 20 { material-type: uint, estimated-quantity: uint }),
    created-at: uint
  }
)

(define-map verified-materials
  { pickup-id: uint, material-type: uint }
  {
    actual-quantity: uint,
    quality-score: uint,
    verified-by: principal,
    verified-at: uint
  }
)

(define-map environmental-impact
  { pickup-id: uint }
  {
    total-weight: uint,
    carbon-offset: uint,
    recycling-efficiency: uint,
    calculated-at: uint
  }
)

(define-map authorized-verifiers principal bool)

(define-data-var next-pickup-id uint u1)
(define-data-var total-waste-processed uint u0)
(define-data-var total-carbon-offset uint u0)

;; private functions
(define-private (is-valid-material-type (material-type uint))
  (or 
    (is-eq material-type PLASTIC)
    (is-eq material-type GLASS)
    (is-eq material-type METAL)
    (is-eq material-type PAPER)
    (is-eq material-type ORGANIC)
    (is-eq material-type ELECTRONIC)
  )
)

(define-private (is-valid-status (status uint))
  (or
    (is-eq status STATUS_SCHEDULED)
    (is-eq status STATUS_COLLECTED)
    (is-eq status STATUS_VERIFIED)
    (is-eq status STATUS_PROCESSED)
  )
)

(define-private (calculate-carbon-offset (material-type uint) (quantity uint))
  ;; Simplified carbon offset calculation based on material type
  (if (is-eq material-type PLASTIC)
    (* quantity u2)
    (if (is-eq material-type GLASS)
      (* quantity u1)
      (if (is-eq material-type METAL)
        (* quantity u3)
        (if (is-eq material-type PAPER)
          (* quantity u1)
          (if (is-eq material-type ORGANIC)
            (* quantity u4)
            (* quantity u5) ;; electronic
          )
        )
      )
    )
  )
)

;; public functions
(define-public (schedule-pickup 
  (location (string-ascii 100))
  (scheduled-date uint)
  (materials (list 20 { material-type: uint, estimated-quantity: uint }))
)
  (let 
    (
      (pickup-id (var-get next-pickup-id))
    )
    ;; Validate all material types
    (asserts! (is-eq (len (filter is-valid-material-type-in-list materials)) (len materials)) ERR_INVALID_MATERIAL_TYPE)
    
    ;; Create pickup schedule
    (map-set pickup-schedules
      { pickup-id: pickup-id }
      {
        requester: tx-sender,
        location: location,
        scheduled-date: scheduled-date,
        status: STATUS_SCHEDULED,
        materials: materials,
        created-at: stacks-block-height
      }
    )
    
    ;; Increment pickup ID counter
    (var-set next-pickup-id (+ pickup-id u1))
    
    (ok pickup-id)
  )
)

(define-private (is-valid-material-type-in-list (material { material-type: uint, estimated-quantity: uint }))
  (is-valid-material-type (get material-type material))
)

(define-public (update-pickup-status (pickup-id uint) (new-status uint))
  (let 
    (
      (pickup (unwrap! (map-get? pickup-schedules { pickup-id: pickup-id }) ERR_PICKUP_NOT_FOUND))
    )
    ;; Validate status
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    
    ;; Only requester or authorized verifier can update status
    (asserts! 
      (or 
        (is-eq tx-sender (get requester pickup))
        (default-to false (map-get? authorized-verifiers tx-sender))
      )
      ERR_NOT_AUTHORIZED
    )
    
    ;; Update pickup status
    (map-set pickup-schedules
      { pickup-id: pickup-id }
      (merge pickup { status: new-status })
    )
    
    (ok true)
  )
)

(define-public (verify-materials
  (pickup-id uint)
  (material-type uint)
  (actual-quantity uint)
  (quality-score uint)
)
  (let
    (
      (pickup (unwrap! (map-get? pickup-schedules { pickup-id: pickup-id }) ERR_PICKUP_NOT_FOUND))
    )
    ;; Only authorized verifiers can verify materials
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) ERR_NOT_AUTHORIZED)
    
    ;; Validate material type
    (asserts! (is-valid-material-type material-type) ERR_INVALID_MATERIAL_TYPE)
    
    ;; Check if pickup is in collected status
    (asserts! (is-eq (get status pickup) STATUS_COLLECTED) ERR_INVALID_STATUS)
    
    ;; Validate quantity
    (asserts! (> actual-quantity u0) ERR_INVALID_QUANTITY)
    
    ;; Record verified materials
    (map-set verified-materials
      { pickup-id: pickup-id, material-type: material-type }
      {
        actual-quantity: actual-quantity,
        quality-score: quality-score,
        verified-by: tx-sender,
        verified-at: stacks-block-height
      }
    )
    
    ;; Update total waste processed
    (var-set total-waste-processed (+ (var-get total-waste-processed) actual-quantity))
    
    (ok true)
  )
)

(define-public (calculate-environmental-impact (pickup-id uint))
  (let
    (
      (pickup (unwrap! (map-get? pickup-schedules { pickup-id: pickup-id }) ERR_PICKUP_NOT_FOUND))
      (materials (get materials pickup))
    )
    ;; Only authorized verifiers can calculate impact
    (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) ERR_NOT_AUTHORIZED)
    
    ;; Calculate total weight and carbon offset from verified materials
    (let
      (
        (impact-data (fold calculate-material-impact materials { total-weight: u0, total-offset: u0, pickup-id: pickup-id }))
      )
      ;; Store environmental impact
      (map-set environmental-impact
        { pickup-id: pickup-id }
        {
          total-weight: (get total-weight impact-data),
          carbon-offset: (get total-offset impact-data),
          recycling-efficiency: u85, ;; Simplified efficiency calculation
          calculated-at: stacks-block-height
        }
      )
      
      ;; Update total carbon offset
      (var-set total-carbon-offset (+ (var-get total-carbon-offset) (get total-offset impact-data)))
      
      (ok impact-data)
    )
  )
)

(define-private (calculate-material-impact 
  (material { material-type: uint, estimated-quantity: uint })
  (acc { total-weight: uint, total-offset: uint, pickup-id: uint })
)
  (let
    (
      (verified-material (map-get? verified-materials { pickup-id: (get pickup-id acc), material-type: (get material-type material) }))
    )
    (match verified-material
      verified
      {
        total-weight: (+ (get total-weight acc) (get actual-quantity verified)),
        total-offset: (+ (get total-offset acc) (calculate-carbon-offset (get material-type material) (get actual-quantity verified))),
        pickup-id: (get pickup-id acc)
      }
      acc ;; Return accumulator unchanged if no verified material found
    )
  )
)

(define-public (add-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

(define-public (remove-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-pickup-details (pickup-id uint))
  (map-get? pickup-schedules { pickup-id: pickup-id })
)

(define-read-only (get-verified-materials (pickup-id uint) (material-type uint))
  (map-get? verified-materials { pickup-id: pickup-id, material-type: material-type })
)

(define-read-only (get-environmental-impact (pickup-id uint))
  (map-get? environmental-impact { pickup-id: pickup-id })
)

(define-read-only (get-total-stats)
  {
    next-pickup-id: (var-get next-pickup-id),
    total-waste-processed: (var-get total-waste-processed),
    total-carbon-offset: (var-get total-carbon-offset)
  }
)

(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)
