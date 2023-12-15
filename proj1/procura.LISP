(defun example_node (n) (node_constructor nil (initial_position (example_state n) 0) 0 nil 0))

;;Node definition and contructer
(defstruct node 
  (parent_node NIL)
  (state)
  (cost)
  (heuristic)
)
(defun node_constructor (parent_node state cost heuristic goal)
  (cond 
    ((NULL state) NIL)
    ((NULL heuristic) 
      (make-node
        :parent_node parent_node
        :state state
        :cost cost
        :heuristic nil
      )
    )
    (T
      (make-node
        :parent_node parent_node
        :state state
        :cost cost
        :heuristic (apply heuristic (list state goal))
      )
    )
  )
)


(defun initial_expand (state)
  (remove nil 
    (list 
      (initial_position state 0)
      (initial_position state 1)
      (initial_position state 2)
      (initial_position state 3)
      (initial_position state 4)
      (initial_position state 5)
      (initial_position state 6)
      (initial_position state 7)
      (initial_position state 8)
      (initial_position state 9)
    )
  )
)
(defun expand (node heuristic goal)
  (let ((state (node-state node)))
    (remove nil 
      (list
        (node_constructor node (move_horse_up_left    state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_up_rigth   state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_rigth_up   state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_rigth_down state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_down_left  state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_down_rigth state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_left_up    state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructor node (move_horse_left_down  state) (+ 1 (node-cost node)) heuristic goal)
      )
    )
  )
)

(defun init_a_star (init_state init_x goal heuristic)
  (cond ((NULL init_state) nil)
        ((NULL init_x)
          (a_star (node_constructor nil init_state -1 nil) goal heuristic '() '())
        )
        (T 
          (a_star (node_constructor nil (initial_position init_state init_x) 0 nil) goal heuristic '() '())
        )
  )

)
(defun a_star (curnode goal heuristic OPEN CLOSED)
;(apply heuristic (list (node-state curnode) goal))
	(cond	((NULL OPEN) (print "empty"))
		(T 	(let ((current_node (remove_min OPEN)))
				(add_to CLOSED  current_node)
				(if (is_goal current_node) (solution current_node))
				(let sucessors ((expand current_node))
					(handle_sucessor sucessors)	
				)
			)
			(a-star curnode open closed)
		)
	)
)
(defun handle_sucessor (sucessors OPEN CLOSE current_node)
	(let ((sucessor (get_list_element sucessors)))
	(cond 	((NULL sucessors) ())
		((list_contains OPEN sucessor) ())
		((list_contains CLOSE sucessor) ())
		(T ())
	))
)

(defun is_goal (state))
(defun remove_min (list))
(defun add_to (list node))
(defun solution (state))
(defun expand (state))
(defun calculate_costs (state1 state2))
(defun update_costs_if_lower (state1 state2))
(defun move_to_open_if_lower_cost (successor, list))
(defun list_contains (list node))


;; functions in common **********************************************************
(defun is-goal (node)
  ;; Implement your goal check here
  (eq node 'goal-node))

(defun solution (node)
  ;; Implement your solution extraction here
  node)

(defun expand (node)
  ;; Implement your node expansion logic here
  (list 'successor1 'successor2))

(defun filter-closed-open (successors closed)
  ;; Filter out successors that are already in closed
  (remove-if (lambda (successor)
               (member successor closed :test #'equal))
             successors))


;;***********************************************************
(defun bfs-graph (initial-node)
  (let ((open (list initial-node))
        (closed '()))
    (loop until (null open)
          do (let* ((current-node (pop open))
                     (successors (expand current-node)))
               (push current-node closed)
               (if (is-goal current-node)
                   (return (solution current-node)))
               (setf open (append open (filter-closed-open successors closed)))
               (setf closed (append closed successors))))
    'failure))

;;**********************************************************
(defun dfs-graph (initial-node depth-limit)
  (let ((open (list initial-node))
        (closed '()))
    (loop until (null open)
          do (let* ((current-node (pop open))
                     (successors (expand current-node)))
               (push current-node closed)
               (if (is-goal current-node)
                   (return (solution current-node)))
               (if (<= (depth current-node) depth-limit)
                   (setf open (append (filter-closed-open successors closed) open)))
               (setf closed (append closed successors))))
    'failure))


    (defun init_game (board position))
    (defun move_piece (state movement_vector)
      (let ((new_position (transform_position (get_s) movement_vector))))
    )

    (defun move_horse_up_left (state)
      (move_piece state '(-1 -2))
    )
    (defun move_horse_up_rigth (state)
      (move_piece state '(1 -2))
    )

    (defun move_horse_rigth_up (state)
      (move_piece state '(2 -1))
    )
    (defun move_horse_rigth_down (state)
      (move_piece state '(2 1))
    )

    (defun move_horse_down_left (state)
      (move_piece state '(-1 2))
    )
    (defun move_horse_down_rigth (state)
      (move_piece state '(1 2))
    )

    (defun move_horse_left_up (state)
      (move_piece state '(-2 1))
    )
    (defun move_horse_left_down (state)
      (move_piece state '(-2 -1))
    )
    (defun transform_position (position vector)
      (let* ( (x (+ (nth 0 position) (nth 0 vector)))
              (y (+ (nth 1 position) (nth 1 vector)))
            )
          (cond 
            ((or (< x 0) (< y 0)) NIL)
            ((or (> x 9) (> y 9)) NIL)
            (T (list x y))
          )
      )
    )
