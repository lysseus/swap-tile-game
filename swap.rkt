#lang racket

;; Implements a swap image puzzle game.

(require racket/generic
         2htdp/universe 
         2htdp/image
         (only-in utils/2htdp/image
                  image-center-x
                  image-center-y
                  image-magnitude
                  scale-image
                  on-board
                  crop/matrix
                  draw-board
                  hilite
                  load-random-image)
         (only-in utils/matrix
                  matrix
                  matrix->list
                  matrix-cols
                  matrix-ref
                  matrix-set
                  matrix-pos->list-pos
                  matrix-map))


;;---------------------------------------------------------------
;; Global variables. 
;; These values are concerned with the game matrix and world sate.
;;---------------------------------------------------------------


(define BOARD-ROWS 6)
(define BOARD-COLS 8)


;;---------------------------------------------------------------
;; Graphic variables. 
;; These values are concerned with the game image.
;;---------------------------------------------------------------


;; The board image is the "original" image before matrix conversion.
(define BOARD-IMG #f)

;; Sub-directory where pictures for the game are stored.
(define IMG-DIR "pictures")

;; These define the shape and size of the board 'squares'.
(define RECT-WIDTH 80)
(define RECT-HEIGHT 80)

;; This is a blank space that is put between the "board" and text images.
(define SPACER (rectangle (* BOARD-COLS RECT-WIDTH) 
                          (quotient RECT-HEIGHT 4)
                          'solid
                          'transparent))

;; Since these appear at the bottom of the scene, no pixel offsets are needed
;; in on-click functions. We scale this text to fit the size of the scene.
(define SHUFFLED-TEXT-IMG (scale-image (* (sub1 BOARD-COLS) RECT-WIDTH)
                                       (quotient RECT-HEIGHT 4)
                                       (text "Press spacebar for hint" 12 'gold)))

(define CLUE-TEXT-IMG (scale-image (* (sub1 BOARD-COLS) RECT-WIDTH)
                                   (quotient RECT-HEIGHT 4)
                                   (text "Please wait" 12 'gold)))


;; A scene crops images placed within it.
(define MT-WIDTH (* BOARD-COLS RECT-WIDTH))
(define MT-HEIGHT (+ (* BOARD-ROWS RECT-HEIGHT)
                     (image-height SPACER)
                     (image-height CLUE-TEXT-IMG)))
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))


;;---------------------------------------------------------------
;; Helper functions/definitions.
;; These functions are concerned with conversions between the game
;; image and the board matrix.
;;---------------------------------------------------------------


;; The clue-reveal time in clock-ticks.
(define REVEAL-TIME 0)

;; compute-reveal-time: img -> integer
;; Attempts to computes a 3-4 second clue-reveal based on the size of the image.
(define (compute-reveal-time img)
  (case (image-magnitude img)
    [(0 1 2 3 4 5) 84]
    [(6) 30]
    [(7 8 9 10) 5]))

;; The world state. 
;;    sq-posn - this is the square to be moved, the one we also hilite.
;;    board - the game board
;;    reveal - a count of ticks remaining in which the original image
;;             is displayed as a "hint".
;;    orig - a matrix of the original unshuffled image.
(struct world (sq-posn shuffled reveal orig) #:mutable #:transparent)

;; A square position.
;;    row - the 1st square's row.
;;    col - the 1st square's col.
(struct sq-posn (row col) #:transparent)

;; The board square state.
;;    selected? - boolean indicates whether the square is to be hilited.
;;    row - the square's original row.
;;    col - the square's original column.
;;    img - the square's cropped image.
(struct sq ((selected? #:mutable) row col img) 
  #:transparent)

;; new-board: image -> matrix
;; Sets up a new board as a matrix of sq structs.
(define (new-board img)
  (define IMGS (crop/matrix BOARD-ROWS 
                            BOARD-COLS 
                            RECT-WIDTH 
                            RECT-HEIGHT
                            img))
  (define F (λ (row col img)
              (sq #f
                  row
                  col
                  img)))
  (matrix-map F IMGS))

;; shuffle-board matrix -> matrix
(define (shuffle-board mtx)
  (define SHUFFLED (shuffle (matrix->list mtx)))
  (matrix SHUFFLED
          #:rows BOARD-ROWS
          #:cols BOARD-COLS))


;;---------------------------------------------------------------
;; on-tick functions.
;; These functions are concerned with clock-ticks and
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; check-for-resume: ws -> ws
;; Counts down to a non-zero clue-reveal, at which point we resume with
;; our shuffled board.
(define (check-for-resume ws)
  (define SQ-POSN (world-sq-posn ws))
  (define BOARD (world-shuffled ws))
  (define REVEAL (world-reveal ws))
  (define ORIG (world-orig ws))
  (cond
    [(zero? (world-reveal ws)) ws]
    [else (world SQ-POSN BOARD (sub1 REVEAL) ORIG)]))


;;---------------------------------------------------------------
;; on-key functions.
;; These functions are concerned with capturing key-strokes and 
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; check-for-clue: ws key -> ws?
;; Set's the clue-reveal in motion.
(define (check-for-clue ws ke)
  (cond
    [(key=? ke " ") (set-world-reveal! ws REVEAL-TIME) ws]
    [else ws]))


;;---------------------------------------------------------------
;; on-mouse functions.
;; These functions are concerned with capturing mouse-clicks and 
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; check-for-swap: ws x y mouse-event -> ws?
;; The gist of this is to select and swap pairsof squares using mouse clicks.
(define (check-for-swap ws x y me)
  ; The position of a previous selection square captured 
  ; from a previous mouse click whose position is stored 
  ; in our world state.
  (define PREV-POSN (world-sq-posn ws))
  
  ; Captures whether the mouse click was 'on the board', and where.
  (define-values (ON-BOARD? ROW COL) 
    (on-board x 
              y 
              RECT-WIDTH
              RECT-HEIGHT
              (world-shuffled ws)))
  
  ; The position of the current sq of our selection pair captured
  ; from the current mouse click.
  (define CURR-POSN (if ON-BOARD? (sq-posn ROW COL) #f))
  
  (cond
    ; We have a clue-reveal in progress - do nothing.
    [(> 0 (world-reveal ws)) ws]
    
    ; We have selected a pair of squares. Swap them.
    [(and (string=? me "button-down") 
          PREV-POSN
          CURR-POSN)
     (swap-squares ws PREV-POSN CURR-POSN)]
    
    ; We have selected only one square so far. Save it.
    [(and (string=? me "button-down") 
          CURR-POSN)
     (select-square ws CURR-POSN)]
    
    ; Anything else just returns the unchanged world state.
    [else ws]))

;; swap-squares: ws prev-posn curr-posn -> ws
;; Swaps the location of the squares within the board matrix. This also
;; removes the selected square position from the world, and de-selects the
;; square that was hilited.
(define (swap-squares ws prev-posn curr-posn)
  (define BOARD (world-shuffled ws))
  (define PREV-ROW (sq-posn-row prev-posn))
  (define PREV-COL (sq-posn-col prev-posn))
  (define PREV-SQ (matrix-ref BOARD PREV-ROW PREV-COL))
  
  (define CURR-ROW (sq-posn-row curr-posn))
  (define CURR-COL (sq-posn-col curr-posn))
  (define CURR-SQ (matrix-ref BOARD CURR-ROW CURR-COL))
  
  (set! BOARD (matrix-set BOARD PREV-ROW PREV-COL CURR-SQ))
  (set! BOARD (matrix-set BOARD CURR-ROW CURR-COL PREV-SQ))
  (set-world-shuffled! ws BOARD)
  (set-world-sq-posn! ws #f)
  (set-sq-selected?! PREV-SQ #f)
  ws)

; select-square: ws sq-posn -> ws
; Sets the sq-posn curr-posn selected? to true and returns
; a new world state with sq-posn set to curr-posn.
(define (select-square ws curr-posn)
  (define BOARD (world-shuffled ws))
  (define ORIG (world-orig ws))
  (define SQ 
    (matrix-ref BOARD
                (sq-posn-row curr-posn)
                (sq-posn-col curr-posn)))
  
  (set-sq-selected?! SQ #t)
  (world curr-posn BOARD 0 ORIG))


;;---------------------------------------------------------------
;; to-draw functions.
;; These functions are concerned with drawing the game world. We
;; break the drawing process up into world, board, row, and square 
;; prpocesses. 
;;---------------------------------------------------------------


;; draw-puzzle-world: ws -> image?
;; Draws the game world.
(define (draw-puzzle-world ws)
  (define SHOW-ORIG? (> (world-reveal ws) 0))
  (define BOARD-SQUARE-F 
    (λ (s)
      (if (sq-selected? s)
          (hilite 'white 3 (sq-img s))
          (sq-img s))))
  (define BOARD-SCENE (draw-board (if SHOW-ORIG?
                                      (world-orig ws)
                                      (world-shuffled ws))
                                  BOARD-SQUARE-F))
  (define WORLD-IMG (above BOARD-SCENE SPACER (if (zero? (world-reveal ws))
                                                  SHUFFLED-TEXT-IMG
                                                  CLUE-TEXT-IMG)))
  
  (place-image WORLD-IMG
               (image-center-x MT)
               (image-center-y MT)
               MT))

;; draw-final-world: ws -> image?
;; Draws the final game world.
(define (draw-final-world ws)
  (define TEXT (scale-image MT-WIDTH
                            MT-HEIGHT
                            (text "WINNER!" 8 'red)))
  (define BOARD-SQUARE-F 
    (λ (s) (sq-img s)))
  
  (define BOARD-SCENE (draw-board (world-shuffled ws) 
                                  BOARD-SQUARE-F))
  (place-image (overlay 
                TEXT
                BOARD-SCENE)
               (image-center-x MT)
               (image-center-y MT)
               MT))


;;---------------------------------------------------------------
;; stop-when functions.
;; These functions are concerned with determining when the game
;; is over. 
;;---------------------------------------------------------------


;; board-complete? ws -> boolean?
;; Indicates whether there are no more squares to swap.
(define (board-complete? ws)
  (define BOARD (world-shuffled ws))
  (define COLS-PER-ROW (matrix-cols BOARD))
  
  (define F (λ (s)
              (matrix-pos->list-pos 
               (sq-row s) 
               (sq-col s)
               #:cols-per-row COLS-PER-ROW)))
  
  (define LST 
    (flatten 
     (matrix-map F BOARD)))
  
  (apply < LST))

;; new-world: image -> ws
;; Sets up a new world struct.
(define (new-world img)
  (define ORIG (new-board img))
  
  ; We create a copy of ORIG so that hilited squares on board 
  ; won't affect squares on ORIG.
  (define COPY (matrix-map (λ (s) (struct-copy sq s)) ORIG))
  
  ; We keep creating a new board if its the same as ORIG.
  (let loop ([board (shuffle-board COPY)])
    (define WORLD (world #f board REVEAL-TIME ORIG))
    (if (not (board-complete? WORLD))
        WORLD
        (loop (new-board ORIG)))))

;; start-game -> ws
;; Launches Big-Bang.
(define (start-game)
  (set! BOARD-IMG (load-random-image IMG-DIR))
  (set! REVEAL-TIME (compute-reveal-time BOARD-IMG))
  
  (big-bang
   ; No selections yet in the world state.
   (new-world BOARD-IMG)
   (on-tick check-for-resume)
   (on-key check-for-clue)
   (on-mouse check-for-swap)
   (to-draw draw-puzzle-world)
   (stop-when board-complete? draw-final-world)
   (name "Click-N-Swap"))
  
  (void))

(start-game)
