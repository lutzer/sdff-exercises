(load "_arity")
(load "../../code-from-book/common/utils.scm")
(load "../../code-from-book/abstracting-a-domain/board.scm")
(load "../../code-from-book/abstracting-a-domain/coords.scm")
(load "../../code-from-book/abstracting-a-domain/game.scm")
(load "../../code-from-book/abstracting-a-domain/pmove.scm")
(load "../../code-from-book/abstracting-a-domain/piece.scm")
(load "../../code-from-book/abstracting-a-domain/game-interpreter.scm")
(load "../../code-from-book/abstracting-a-domain/checkers-shared.scm")
(load "../../code-from-book/abstracting-a-domain/checkers-new.scm")

(board-width (make-board checkers))

(define (print-board board)
  (let* ((width (board-width board))
         (depth (board-depth board))
         (piece->string (lambda (piece)
                          (if piece
                              (if (eqv? (piece-color piece) 'black) "bb " "rr ")
                              "·· ")))
         (board-row-string
          (lambda (rowidx) (fold
                     (lambda (colidx agg)
                       (string-append
                        (piece->string (board-get (make-coords colidx rowidx) board)) agg))
                          ""
                          (iota width)))))
    (fold (lambda (rowidx agg) (string-append agg "\n" (board-row-string rowidx)))
          ""
          (iota depth))))

;; print the intial board state of checkers
(display (print-board (make-board checkers)))

;; next step: add functionality for (make-board chess), so that we can print the
;; intial board state with just four rooks and nothing else.

(define (make-chess moves-generator)
  (make-game 8 8 '(white black) '(rook)
             chess-initial-pieces
             moves-generator
             chess-piece-summary))

(define (chess-piece-summary piece)
  (if piece
      (case (piece-color piece)
        ((black) "bR ")
        ((white) "wR ")
        (else (error "Unknown color: " piece)))
      "·· "))

(define (chess-initial-pieces game)
  (append-map (lambda (color) (make-piece color
                                     'rook
                                     (make-coords 0 (if (eq? color 'black)
                                                        0
                                                        7))))
              (game-colors game)))

(define chess (make-chess generate-moves-using-rule-interpreter))

;; print the intial board state of chess
(display (print-board (make-board chess)))
