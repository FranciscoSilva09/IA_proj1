;(let ((list '(1 2 3))) (loop while list do (progn (fromat t "~a~%"list) (setf list (cdr list))))
(defun example_node (n) (node_constructer nil (initial_position (example_state n) 0) 0 nil 0))

;result definition and constructer
(defstruct result
  path
  stats
)
(defun result_constructer (node stats)
  (make-result :path (built_path node) :stats stats)
)
(defun built_path (node)
  (cond ((null node)
          nil
        )
        ((null (node-parent_node node))
          (list node)
        )
        (t
          (append (built_path (node-parent_node node)) (list node))
        )
  )
)

;statistics definition
(defstruct statistics
  expanded_nodes
  generated_nodes
)
(defun statistics_constructer (stats expanded_increment generated_increment)
  (if (null stats)
    (make-statistics
      :expanded_nodes expanded_increment
      :generated_nodes generated_increment
    )
    (make-statistics
      :expanded_nodes (+ (statistics-expanded_nodes stats) expanded_increment)
      :generated_nodes (+ (statistics-generated_nodes stats) generated_increment)
    )
  )
)
;calculate stats
(defun check_b_val (t_val res err) 
  (or (= t_val res) (and (< t_val (+ res err)) (> t_val (- res err))))
)
(defun polimial (l_val b_val) 
  (if (= l_val 1) 
    b_val 
    (+ (polimial (- l_val 1) b_val) (expt b_val l_val))
  )
)
(defun bixetion (b_min b_max) 
  (/ (+ b_min b_max) 2)
)
(defun CALC_RAM (l_val t_val b_min b_max) 
  (let* ( (b_new (bixetion b_min b_max))
          (res (polimial l_val b_new))
        )
    (cond 
      ((check_b_val t_val res 0.01) b_new)
      ((> res t_val) (CALC_RAM l_val t_val b_min b_new))
      (T (CALC_RAM l_val t_val b_new b_max))
    )
  )
)

;;Node definition and constructer
(defstruct node 
  (parent_node NIL)
  (state)
  (cost)
  (heuristic)
)
(defun node_constructer (parent_node state cost heuristic goal)
"Construtor de estrutura de dados Node"
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
"Retorna o numero de pontos obtidos no estado"
  (if (null node) 0 (state-points (node-state node)))
)
(defun get_node_cost (node)
"Retorna o custo do Node"
  (cond ((null node) nil)
        ((null (node-heuristic node)) (node-cost node))
        (T (+ (node-heuristic node) (node-cost node)))
  )
)
(defun compare_node (node1 node2) 
"compara 2 Node"
  (if (or (null node1) (null node2))
    nil
    (let* ( (state1 (node-state node1)) 
            (state2 (node-state node2))
          )
      (compare_state state1 state2)
    )
  )
)


;auxiliar functions
(defun expand (node heuristic goal)
"Expande o no em nos sucessores"
  (let ((state (node-state node)))
    (remove nil 
      (list
        (node_constructer node (move_horse_up_left    state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_up_rigth   state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_rigth_up   state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_rigth_down state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_down_left  state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_down_rigth state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_left_up    state) (+ 1 (node-cost node)) heuristic goal)
        (node_constructer node (move_horse_left_down  state) (+ 1 (node-cost node)) heuristic goal)
      )
    )
  )
)
(defun initial_expand (state heuristic goal)
"Cria uma lista com todos os nos iniciais possiveis"
  (remove nil 
    (list 
      (node_constructer nil (initial_position state 0) 0 heuristic goal)
      (node_constructer nil (initial_position state 1) 0 heuristic goal)
      (node_constructer nil (initial_position state 2) 0 heuristic goal)
      (node_constructer nil (initial_position state 3) 0 heuristic goal)
      (node_constructer nil (initial_position state 4) 0 heuristic goal)
      (node_constructer nil (initial_position state 5) 0 heuristic goal)
      (node_constructer nil (initial_position state 6) 0 heuristic goal)
      (node_constructer nil (initial_position state 7) 0 heuristic goal)
      (node_constructer nil (initial_position state 8) 0 heuristic goal)
      (node_constructer nil (initial_position state 9) 0 heuristic goal)
    )
  )
)
(defun sort_list_of_node (list)
  (sort list #'< :key #'get_node_cost)
)
(defun check_depth (node depth) (> (node-cost node) depth))
(defun check (sucessors goal)
  (let ((solutions (remove-if #'(lambda (node) (< (get_node_points node) goal)) sucessors)))
    (cond ((NULL solutions) nil)
          (T (nth 0 (sort_list_of_node solutions)))
    )
  )
)
(defun remove_if_contains (list_to_remove list)
 (remove-if #'(lambda (node) (member node list :test #'compare_node)) list_to_remove)
)

;algs
(defun init_bfs (init_state init_x goal)
  (cond ((NULL init_state) nil)
        ((NULL init_x)
          (let ((nodes (initial_expand init_state nil goal)))
            (bfs nodes '() goal (statistics_constructer nil 0 (length nodes)))
          )
        )
        (T 
          (bfs 
            (remove nil 
              (list 
                    (node_constructer nil (initial_position init_state init_x) 0 nil goal)
              )
            ) 
            '() 
            goal
            (statistics_constructer nil 0 0)
          )
        )
  )
)
(defun bfs (open close goal stats)
  (if (NULL open) nil
    (let* ( (curnode (car open))
            (sucessors (expand curnode nil goal))
            (updated_sucessors 
                (remove_if_contains 
                  (remove_if_contains sucessors (cdr open))
                  close
                )
            )
            (updated_close (append close (list curnode)))
            (updated_open (append (cdr open) updated_sucessors))
            (solution (check updated_sucessors goal))
          )
      (if (NULL solution)
        (bfs updated_open updated_close goal (statistics_constructer stats 1 (length updated_sucessors)))
        (result_constructer solution (statistics_constructer stats 1 (length updated_sucessors)))      
      )
    )
  )
)
(defun init_dfs (init_state init_x goal depth)
  (cond ((NULL init_state) nil)
        ((NULL init_x)
          (let ((nodes (initial_expand init_state nil goal)))
            (dfs nodes '() goal depth (statistics_constructer nil 0 (length nodes)))
          )
        )
        (T 
          (dfs 
            (remove nil 
              (list 
                    (node_constructer nil (initial_position init_state init_x) 0 nil goal)
              )
            ) 
            '() 
            goal 
            depth
            (statistics_constructer nil 0 0)
          )
        )
  )
)
(defun dfs (open close goal depth stats)
  (if (NULL open) nil
    (let ((curnode (car open)))
      (if (check_depth curnode depth)
        (dfs (cdr open) (append close (list curnode)) goal depth stats)
        (let* ( (sucessors (expand curnode nil goal))
                (updated_sucessors 
                    (remove_if_contains 
                      (remove_if_contains sucessors (cdr open))
                      close
                    )
                )
                (updated_close (append close (list curnode)))
                (updated_open (append updated_sucessors (cdr open)))
                (solution (check updated_sucessors goal))
              )
          (if (NULL solution)
            (dfs updated_open updated_close goal depth (statistics_constructer stats 1 (length updated_sucessors)))
            (result_constructer solution (statistics_constructer stats 1 (length updated_sucessors)))      
          )
        )
      )
    )
  )
)
(defun init_a_star (init_state init_x goal heuristic)
  (cond ((NULL init_state) nil)
        ((NULL init_x)
          (let ((nodes (initial_expand init_state heuristic goal)))
            (a* (sort_list_of_node nodes) '() goal heuristic (statistics_constructer nil 0 (length nodes)))
          )
        )
        (T 
          (a* 
            (list 
                  (node_constructer nil (initial_position init_state init_x) 0 nil goal)
            )
            '() 
            goal 
            heuristic
            (statistics_constructer nil 0 0)
          )
        )
  )
)
(defun a* (open close goal heuristic stats)
  (if (NULL open) nil
    (let* ( (curnode (car open))
            (sucessors (expand curnode heuristic goal))
            (updated_sucessors 
                (remove_if_contains 
                  (remove_if_contains sucessors (cdr open))
                  close
                )
            )
            (updated_close (append close (list curnode)))
            (updated_open (append (cdr open) updated_sucessors))
            (solution (check (list curnode) goal))
          ) 
      (cond 
        ((not (NULL solution)) 
          (result_constructer solution (statistics_constructer stats 1 (length updated_sucessors))) 
        )
        ((null sucessors)
          (a* (cdr open) updated_close goal heuristic (statistics_constructer stats 1 0))
        )
        (t
          (a* (sort_list_of_node updated_open) updated_close goal heuristic (statistics_constructer stats 1 (length updated_sucessors)))
        )
             
      )
    )
  )
)

