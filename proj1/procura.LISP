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
(defun get_node_points (node) 
  (if (null node) 0 (state-points (node-state node)))
)
(defun get_node_cost (node)
  (cond ((null node) nil)
        ((null (node-heuristic node)) (node-cost node))
        (T (+ (node-heuristic node) (node-cost node)))
  )
)

(defun compare_node (node1 node2) 
  (if (or (null node1) (null node2))
    nil
    (let* ( (state1 (node-state node1)) 
            (state2 (node-state node2))
          )
      (compare_state state1 state2)
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

(defun initial_expand (state heuristic goal)
  (remove nil 
    (list 
      (node_constructor nil (initial_position state 0) 0 heuristic goal)
      (node_constructor nil (initial_position state 1) 0 heuristic goal)
      (node_constructor nil (initial_position state 2) 0 heuristic goal)
      (node_constructor nil (initial_position state 3) 0 heuristic goal)
      (node_constructor nil (initial_position state 4) 0 heuristic goal)
      (node_constructor nil (initial_position state 5) 0 heuristic goal)
      (node_constructor nil (initial_position state 6) 0 heuristic goal)
      (node_constructor nil (initial_position state 7) 0 heuristic goal)
      (node_constructor nil (initial_position state 8) 0 heuristic goal)
      (node_constructor nil (initial_position state 9) 0 heuristic goal)
    )
  )
)
(defun sort_list_of_node (list)
  (sort list #'< :key #'get_node_cost)
)
(defun init_a_star (init_state init_x goal heuristic)
  (cond ((NULL init_state) nil)
        ((NULL init_x)
          (recursive_a_star (initial_expand init_state heuristic goal) '() goal heuristic )
        )
        (T 
          (recursive_a_star 
            (remove nil 
              (list 
                    (node_constructor nil (initial_position init_state init_x) 0 heuristic goal)
              )
            ) 
            '() 
            goal
            heuristic 
          )
        )
  )

)

(defun recursive_a_star (open closed goal heuristic)
"PROCURA INFORMADA - A*"
  (if (NULL open) nil
   (let* ((curnode (nth 0 (sort_list_of_node open)))
            (successors (expand curnode heuristic goal))
            (filtered_successors (remove-if #'(lambda (node) (member node CLOSED :test #'move_to_open_if_lower_cost)) successors))
            (aux_open (remove-if #'(lambda (node) (member node filtered_successors :test #'remove_if_costs_lower)) (cdr open)))
            (updated_open (union aux_open filtered_successors :test #'compare_node))
          )
      (if (>= (get_node_points curnode) goal)
        curnode
        (recursive_a_star updated_open (cons curnode closed) goal heuristic)
      )
    )    
  )
)

(defun remove_if_costs_lower (opened_node sucessor)
  (cond ((not (compare_node opened_node sucessor)) nil)
        ((< (node-cost sucessor) (node-cost opened_node)) T)
        (T Nil)
  )
)
(defun move_to_open_if_lower_cost (successor closed_node)
  (cond ((not (compare_node successor closed_node)) nil)
        ((< (node-cost successor) (node-cost closed_node)) nil)
        (T T)
  )
)
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