;; OgaLand - Decentralized Land Registry for Africa
;; Commit 2: Add title verification and dispute resolution
;; Tackles land disputes and fake documentation issues prevalent in African cities

;; ============================================
;; CONSTANTS & ERROR CODES
;; ============================================

;; Contract administrator
(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_TITLE_DISPUTED (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_TRANSFER_BLOCKED (err u106))
(define-constant ERR_ALREADY_VERIFIED (err u107))
(define-constant ERR_CANNOT_DISPUTE_OWN (err u108))

;; Title status constants
(define-constant STATUS_PENDING u1)
(define-constant STATUS_VERIFIED u2)
(define-constant STATUS_DISPUTED u3)
(define-constant STATUS_TRANSFERRED u4)

;; ============================================
;; DATA VARIABLES
;; ============================================

;; Global counters
(define-data-var next-title-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var total-titles uint u0)
(define-data-var total-verified-titles uint u0)
(define-data-var total-disputes uint u0)

;; Contract settings
(define-data-var contract-active bool true)
(define-data-var verification-required bool true)

;; ============================================
;; DATA MAPS
;; ============================================

;; Land title registry
(define-map land-titles
  uint ;; title-id
  {
    owner: principal,
    land-address: (string-utf8 256),
    coordinates: (string-ascii 100), ;; GPS coordinates
    size-sqm: uint, ;; Size in square meters
    title-deed-hash: (buff 32), ;; Hash of title deed document
    status: uint,
    registration-block: uint,
    verification-block: uint,
    verifier: (optional principal),
    metadata-uri: (optional (string-utf8 256))
  }
)

;; Title lookup by owner
(define-map owner-titles
  principal
  (list 50 uint) ;; List of title IDs
)

;; Title deed hash to title ID mapping (prevents duplicate registrations)
(define-map deed-hash-to-title
  (buff 32)
  uint
)

;; Authorized verifiers registry
(define-map authorized-verifiers
  principal
  {
    name: (string-utf8 100),
    active: bool,
    titles-verified: uint,
    registration-block: uint
  }
)

;; Dispute registry
(define-map disputes
  uint ;; dispute-id
  {
    title-id: uint,
    complainant: principal,
    reason: (string-utf8 500),
    evidence-hash: (buff 32),
    filed-block: uint,
    resolved: bool,
    resolution: (optional (string-utf8 500)),
    resolver: (optional principal)
  }
)

;; Title to disputes mapping
(define-map title-disputes
  uint ;; title-id
  (list 10 uint) ;; List of dispute IDs
)

;; ============================================
;; PRIVATE FUNCTIONS
;; ============================================

;; Check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq contract-caller CONTRACT_OWNER)
)

;; Check if contract is active
(define-private (is-contract-active)
  (var-get contract-active)
)

;; Check if title exists
(define-private (title-exists (title-id uint))
  (is-some (map-get? land-titles title-id))
)

;; Check if verifier is authorized and active
(define-private (is-authorized-verifier (verifier principal))
  (match (map-get? authorized-verifiers verifier)
    verifier-data (get active verifier-data)
    false
  )
)

;; ============================================
;; ADMIN FUNCTIONS
;; ============================================

;; Toggle contract active status
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

;; Add authorized verifier
(define-public (add-verifier (verifier principal) (name (string-utf8 100)))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    
    (map-set authorized-verifiers verifier {
      name: name,
      active: true,
      titles-verified: u0,
      registration-block: burn-block-height
    })
    (ok true)
  )
)

;; Deactivate verifier
(define-public (deactivate-verifier (verifier principal))
  (let
    (
      (verifier-data (unwrap! (map-get? authorized-verifiers verifier) ERR_NOT_FOUND))
    )
    (begin
      (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
      (map-set authorized-verifiers verifier (merge verifier-data { active: false }))
      (ok true)
    )
  )
)

;; ============================================
;; LAND TITLE REGISTRATION
;; ============================================

;; Register a new land title
(define-public (register-land-title
  (land-address (string-utf8 256))
  (coordinates (string-ascii 100))
  (size-sqm uint)
  (title-deed-hash (buff 32))
  (metadata-uri (optional (string-utf8 256)))
)
  (let
    (
      (title-id (var-get next-title-id))
      (owner contract-caller)
      (owner-title-list (default-to (list) (map-get? owner-titles owner)))
    )
    (begin
      ;; Validate contract is active
      (asserts! (is-contract-active) ERR_UNAUTHORIZED)
      
      ;; Validate inputs
      (asserts! (> (len land-address) u0) ERR_INVALID_INPUT)
      (asserts! (> (len coordinates) u0) ERR_INVALID_INPUT)
      (asserts! (> size-sqm u0) ERR_INVALID_INPUT)
      
      ;; Check title deed hash is not already registered
      (asserts! (is-none (map-get? deed-hash-to-title title-deed-hash)) ERR_ALREADY_EXISTS)
      
      ;; Create title record
      (map-set land-titles title-id {
        owner: owner,
        land-address: land-address,
        coordinates: coordinates,
        size-sqm: size-sqm,
        title-deed-hash: title-deed-hash,
        status: STATUS_PENDING,
        registration-block: burn-block-height,
        verification-block: u0,
        verifier: none,
        metadata-uri: metadata-uri
      })
      
      ;; Map deed hash to title ID
      (map-set deed-hash-to-title title-deed-hash title-id)
      
      ;; Add to owner's title list
      (map-set owner-titles owner 
        (unwrap-panic (as-max-len? (append owner-title-list title-id) u50))
      )
      
      ;; Update counters
      (var-set next-title-id (+ title-id u1))
      (var-set total-titles (+ (var-get total-titles) u1))
      
      (ok title-id)
    )
  )
)

;; ============================================
;; TITLE VERIFICATION
;; ============================================

;; Verify a land title (authorized verifiers only)
(define-public (verify-title (title-id uint))
  (let
    (
      (title (unwrap! (map-get? land-titles title-id) ERR_NOT_FOUND))
      (verifier contract-caller)
      (verifier-data (unwrap! (map-get? authorized-verifiers verifier) ERR_UNAUTHORIZED))
    )
    (begin
      ;; Check verifier is active
      (asserts! (get active verifier-data) ERR_UNAUTHORIZED)
      
      ;; Check title is in pending status
      (asserts! (is-eq (get status title) STATUS_PENDING) ERR_ALREADY_VERIFIED)
      
      ;; Update title status
      (map-set land-titles title-id (merge title {
        status: STATUS_VERIFIED,
        verification-block: burn-block-height,
        verifier: (some verifier)
      }))
      
      ;; Update verifier stats
      (map-set authorized-verifiers verifier (merge verifier-data {
        titles-verified: (+ (get titles-verified verifier-data) u1)
      }))
      
      ;; Update global counter
      (var-set total-verified-titles (+ (var-get total-verified-titles) u1))
      
      (ok true)
    )
  )
)

;; ============================================
;; DISPUTE RESOLUTION
;; ============================================

;; File a dispute against a title
(define-public (file-dispute
  (title-id uint)
  (reason (string-utf8 500))
  (evidence-hash (buff 32))
)
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (title (unwrap! (map-get? land-titles title-id) ERR_NOT_FOUND))
      (complainant contract-caller)
      (title-dispute-list (default-to (list) (map-get? title-disputes title-id)))
    )
    (begin
      ;; Validate inputs
      (asserts! (> (len reason) u0) ERR_INVALID_INPUT)
      
      ;; Cannot dispute own title
      (asserts! (not (is-eq complainant (get owner title))) ERR_CANNOT_DISPUTE_OWN)
      
      ;; Create dispute record
      (map-set disputes dispute-id {
        title-id: title-id,
        complainant: complainant,
        reason: reason,
        evidence-hash: evidence-hash,
        filed-block: burn-block-height,
        resolved: false,
        resolution: none,
        resolver: none
      })
      
      ;; Update title status to disputed
      (map-set land-titles title-id (merge title { status: STATUS_DISPUTED }))
      
      ;; Add dispute to title's dispute list
      (map-set title-disputes title-id
        (unwrap-panic (as-max-len? (append title-dispute-list dispute-id) u10))
      )
      
      ;; Update counters
      (var-set next-dispute-id (+ dispute-id u1))
      (var-set total-disputes (+ (var-get total-disputes) u1))
      
      (ok dispute-id)
    )
  )
)

;; Resolve a dispute (admin only)
(define-public (resolve-dispute
  (dispute-id uint)
  (resolution (string-utf8 500))
  (restore-verified bool)
)
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_NOT_FOUND))
      (title (unwrap! (map-get? land-titles (get title-id dispute)) ERR_NOT_FOUND))
    )
    (begin
      (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
      (asserts! (not (get resolved dispute)) ERR_ALREADY_EXISTS)
      
      ;; Update dispute
      (map-set disputes dispute-id (merge dispute {
        resolved: true,
        resolution: (some resolution),
        resolver: (some contract-caller)
      }))
      
      ;; Update title status based on resolution
      (map-set land-titles (get title-id dispute) (merge title {
        status: (if restore-verified STATUS_VERIFIED STATUS_DISPUTED)
      }))
      
      (ok true)
    )
  )
)

;; ============================================
;; READ-ONLY FUNCTIONS
;; ============================================

;; Get land title information
(define-read-only (get-land-title (title-id uint))
  (map-get? land-titles title-id)
)

;; Get titles owned by a principal
(define-read-only (get-owner-titles (owner principal))
  (map-get? owner-titles owner)
)

;; Get title ID by deed hash
(define-read-only (get-title-by-deed-hash (deed-hash (buff 32)))
  (map-get? deed-hash-to-title deed-hash)
)

;; Get verifier information
(define-read-only (get-verifier-info (verifier principal))
  (map-get? authorized-verifiers verifier)
)

;; Get dispute information
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

;; Get disputes for a title
(define-read-only (get-title-disputes (title-id uint))
  (map-get? title-disputes title-id)
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-titles: (var-get total-titles),
    total-verified-titles: (var-get total-verified-titles),
    total-disputes: (var-get total-disputes),
    next-title-id: (var-get next-title-id),
    next-dispute-id: (var-get next-dispute-id),
    contract-active: (var-get contract-active),
    verification-required: (var-get verification-required)
  }
)

;; Check if title is verified
(define-read-only (is-title-verified (title-id uint))
  (match (map-get? land-titles title-id)
    title (is-eq (get status title) STATUS_VERIFIED)
    false
  )
)

;; Check if title is disputed
(define-read-only (is-title-disputed (title-id uint))
  (match (map-get? land-titles title-id)
    title (is-eq (get status title) STATUS_DISPUTED)
    false
  )
)