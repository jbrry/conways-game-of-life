;;;; Conway's Game of Life
;;;; Adapted from Al Sweigart's Python demo: (https://www.youtube.com/watch?v=Vn8Mug5w7sw&ab_channel=AlSweigart)

(defvar width 79)
(defvar height 20)
(defvar alive "O")
(defvar dead " ")
(defvar choice 0)

;; Create a hash table to store the game state
;; using equal testing condition.
(defvar *next-cells* (make-hash-table :test #'equal))

;; Put random living/dead cells into next-cells to start:
(loop for x from 0 upto width do
  (loop for y from 0 upto height do
    ;; 50/50 chance of starting cell being alive or dead.
    (setf choice (random (length '(0 1))))
    (if (eql choice 0)
      (setf (gethash (list x y) *next-cells*) alive)
      (setf (gethash (list x y) *next-cells*) dead))
))


;; Copy hash table
;; https://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp
(defun copy-table (table)
  (let ((new-table (make-hash-table
		    :test (hash-table-test table)
		    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
		 (setf (gethash key new-table) value))
	     table)
    new-table))


(defun run-game ()
  "Runs Conway's Game of Life
   (press CTRL-C to stop)"
  (defvar left 0)
  (defvar right 0)
  (defvar above 0)
  (defvar below 0)
  (defvar num-neighbours 0)

  ;; Clear the screen
  (dotimes (i 5) (format t "~%"))

  ;; Create a copy of next-cells for the next generation without
  ;; modifying current generation.
  (setq cells (copy-table *next-cells*))

  ;; Print game state in the terminal.
  (loop for y from 0 upto height do
    (loop for x from 0 upto width do
      (format t "~A" (gethash (list x y) *next-cells*))))

  (loop for x from 0 upto width do
    (loop for y from 0 upto height do
      (setq left  (mod (- x 1) width))
      (setq right (mod (+ x 1) width))
      (setq above (mod (- y 1) height))
      (setq below (mod (+ y 1) height))

      ;; Count the number of neighbours
      (setq num-neighbours 0)
      
      ;; top-left neighbour is alive
      (if (equal (gethash (list left above) cells) alive)
   	  (setq num-neighbours (+ num-neighbours 1)))
      ;; above neighbour is alive
      (if (equal (gethash (list x above) cells) alive)
  	  (setq num-neighbours (+ num-neighbours 1)))
      ;; top-right neighbour is alive
      (if (equal (gethash (list right above) cells) alive)
 	  (setq num-neighbours (+ num-neighbours 1)))
      ;; left neighbour is alive
      (if (equal (gethash (list left y) cells) alive)
 	  (setq num-neighbours (+ num-neighbours 1)))
      ;; right neighbour is alive
      (if (equal (gethash (list right y) cells) alive)
	  (setq num-neighbours (+ num-neighbours 1)))
      ;; bottom-left neighbour is alive
      (if (equal (gethash (list left below) cells) alive)
	  (setq num-neighbours (+ num-neighbours 1)))
      ;; bottom neighbour is alive
      (if (equal (gethash (list x below) cells) alive)
	  (setq num-neighbours (+ num-neighbours 1)))
      ;; bottom-right neighbour is alive
      (if (equal (gethash (list right below) cells) alive)
	  (setq num-neighbours (+ num-neighbours 1)))

      ;; Set cell state based on Conway's Game of Life rules:
      (cond
	;; Living cells with 2 or 3 neighbours stay alive:
        ((and (equal (gethash (list x y) cells) alive) (or (equal num-neighbours 2) (equal num-neighbours 3)))
	 (setf (gethash (list x y) *next-cells*) alive))
	;; Dead cells with 3 neighbours become alive:
        ((and (equal (gethash (list x y) cells) dead) (equal num-neighbours 3))
	 (setf (gethash (list x y) *next-cells*) alive))
	;; Everything else dies or stays dead.
        (t (setf (gethash (list x y) *next-cells*) dead)))))

  (sleep 1)
  (run-game))

(run-game)
