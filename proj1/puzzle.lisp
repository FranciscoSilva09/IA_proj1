(defun example_state (n) (state_constructer_with_params (example_board n) 0 (position_constructer -1 -1)))
;(move_piece (EXAMPLE_STATE 1) '(1 1))


;;Aplicação
(defun init_game (board_points x alg &rest params)
"board_points - lista dos pontos 
x - posição inicial de 0 a 9 
alg - função do algoritmo 
param - parametros para o algoritmo"
  (apply #'alg params)
)

;;Board definition constructer and auxiliare funcs
(defstruct board
    (points)
)
(defun board_constructer (points_list)
    (make-board :points points_list )
)
(defun compare_board (board1 board2)
  (every #'(lambda (a) (= 0 a)) 
    (mapcar #'- (remove nil (apply #'append (board-points board1)))
                (remove nil (apply #'append (board-points board2)))
    )
  )
)

;;State definition constructer and auxiliare funcs
(defstruct state
    (board (make-board))
    (points 0)
    (position (position_constructer -1 -1))
)
(defun state_constructer (points_list)
    (make-state 
        :board (board_constructer points_list) 
        :current_points 0 
        :position (position_constructer -1 -1) 
    )
)
(defun state_constructer_with_params (points_list points pos)
    (make-state 
        :board (board_constructer points_list)
        :points points
        :position pos
    )
)
(defun get_state_square (state position)
  (nth (horse_position-x position) (nth (horse_position-y position) (get_state_board_points state)))
)
(defun get_state_board_points (state)
    (board-points (state-board state))
)

(defun compare_state (state1 state2)
  (let* ( (points1 (state-points state1)) 
          (points2 (state-points state2))
          (position1 (state-position state1)) 
          (position2 (state-position state2))
          (board1 (state-board state1)) 
          (board2 (state-board state2))
        )
    (and  (= points1 points2) 
          (compare_position position1 position2) 
          (compare_board board1 board2)
    )
  )
)

;;Horse_Position definition constructer and auxiliare funcs
(defstruct horse_position
  (x -1)
  (y -1)
)
(defun position_constructer (x y)
  (make-horse_position
    :x x
    :y y
  )
)
(defun compare_position (pos1 pos2)
  (and 
    (= (horse_position-x pos1) (horse_position-x pos2))
    (= (horse_position-y pos1) (horse_position-y pos2))
  )
)

;;Heuristic definition
(defun calculate_heuristic (current_state goal_points)
"Calcula a heuristica h(x) = o(x)/m(x)
m(x) é a média por casa dos pontos que constam no tabuleiro x,
o(x) é o número de pontos que faltam para atingir o valor definido como objetivo."
    (let (  (size 100) 
            (total_points (sum_total_points (get_state_board_points current_state)))
            (current_points (state-points current_state))
        )
        (/  (- goal_points current_points)
            (/ total_points size)
        )
    )
)
(defun sum_total_points (board_points)
"Soma todos os pontos do tabuleiro"
  (reduce #'+ (remove nil (mapcan #'flatten board_points)))
)
(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t  (append (flatten (car lst))
                    (flatten (cdr lst))
            )
        )
  )
)
;;operadores
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
  (let* ( (x (+ (horse_position-x position) (nth 0 vector)))
          (y (+ (horse_position-y position) (nth 1 vector)))
        )
      (cond 
        ((or (< x 0) (< y 0)) (> x 9) (> y 9) NIL)
        (T (position_constructer x y))
      )
  )
)
(defun move_piece (state movement_vector)
  (let ((new_position (transform_position (state-position state) movement_vector)))
    (if (or (NULL new_position) (NULL (get_state_square state new_position)))
      NIL
      (calculate_new_state state new_position)
    )
  )
)
(defun calculate_new_state (state new_position)
  (state_constructer_with_params 
    (set_board_value (get_state_board_points state) new_position NIL) 
    (+ (state-points state) (get_state_square state new_position))
    new_position
  )
)
(defun set_board_value (board_list position value)
  (let* ( (line (nth (horse_position-y position) board_list))
          (new_line (set_list_value (horse_position-x position) line value))
        )
      (set_list_value (horse_position-y position) board_list new_line)
  )
)
(defun set_list_value (index line value)
  (append (append (subseq line 0 index) (list value))
          (cdr (subseq line index))
  )
)

;;Inicio do jogo
(defun initial_position (init_state x)
  (let* ( (vector (list (+ x 1) 1))
          (new_state (move_piece init_state vector))
        )
    (let ((pos (find_position_inicial_game new_state)))
      (if (NULL pos) new_state
        (state_constructer_with_params 
          (set_board_value 
            (get_state_board_points new_state)  
            pos
            NIL
          )
          (state-points new_state) 
          (state-position new_state) 
        )
      )
    )
  )
)
(defun find_position_inicial_game (state)
  (cond ((NULL state) NIL)
        ((double_algarisms (state-points state)) 
          (find_lowest_double_algarisms 
              (get_state_board_points state)
          )
        )
        (T
          (find_inverse 
              (get_state_board_points state) 
              (state-points state)
          )
        )
  )
)
(defun find_lowest_double_algarisms (board_list)
  (let ((numbs (list 0 11 22 33 44 55 66 77 88 99)))
    (find_one_list_element board_list numbs)
  )
)
(defun find_one_list_element (board_list list)
  (let ((pos (seach_board board_list (car list))))
    (if (or (NULL pos) (<= (length list) 1)) 
      (find_one_list_element board_list (cdr list)) 
      pos
    )
  )
)
(defun find_inverse (board_list value)
  (seach_board  
        board_list 
        (+ (* (mod value 10) 10) (floor (/ value 10)))
  )
)
(defun seach_board (board_list value)
  (let ((index (position value (apply #'append board_list) :test #'equal)))
    (if (NULL index) NIL
      (position_constructer (mod index 10) (floor (/ index 10)))
    )
  )
)
(defun double_algarisms (numb) (= (mod numb 10) (floor (/ numb 10))))
