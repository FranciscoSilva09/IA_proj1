;aux
(defun example_state (n) (state_constructer_with_params (example_board n) 0 (position_constructer -1 -1)))


;;Board definition constructer and auxiliare funcs
(defstruct board
    (points)
)
(defun board_constructer (points_list)
"Construtor da estrutura de dados board"
    (make-board :points points_list )
)
(defun compare_board (board1 board2)
"Compara 2 Board e retorna t se forem iguais"
  (every #'(lambda (a) (= 0 a)) 
    (mapcar 
        #'(lambda (a b) 
          (cond ((and (null a) (null b)) 0)
                ((null a) b)
                ((null b) (* a -1))
                (t (- b a))
          )
        )
        (remove nil (apply #'append (board-points board1)))
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
"Construtor da estrutura de dados State com parametros default"
    (make-state 
        :board (board_constructer points_list) 
        :points 0 
        :position (position_constructer -1 -1) 
    )
)
(defun state_constructer_with_params (points_list points pos)
"Construtor da estrutura de dados State"
    (make-state 
        :board (board_constructer points_list)
        :points points
        :position pos
    )
)
(defun get_state_square (state position)
"Retorna o valor dos pontos em uma determina posição"
  (let ((x (horse_position-x position)) (y (horse_position-y position)))
    (if (or (>= x 10) (< x 0) (>= y 10) (< y 0)) nil
      (nth x (nth y (get_state_board_points state)))
    )
  )
)
(defun get_state_board_points (state)
"Retorna a lista de pontos"
    (board-points (state-board state))
)
(defun compare_state (state1 state2)
"Compara 2 State e retorna t se forem iguais"
  (if (or (null state1) (null state2))
    nil
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
)

;;Horse_Position definition constructer and auxiliare funcs
(defstruct horse_position
  (x -1)
  (y -1)
)
(defun position_constructer (x y)
"Construtor de uma estrutura de dados Horse_position"
  (make-horse_position
    :x x
    :y y
  )
)
(defun compare_position (pos1 pos2)
"Compara 2 Horse_position e retorna t se forem iguais"
  (and 
    (= (horse_position-x pos1) (horse_position-x pos2))
    (= (horse_position-y pos1) (horse_position-y pos2))
  )
)
(defun position_to_string (pos)
  (let ((x (horse_position-x pos)) (y (horse_position-y pos)))
    (cond ((or (< x 0) (< y 0)) (format nil "NN"))
          (T (format nil "~a~a" (nth x '(A B C D E F G H I J)) (+ y 1)))
    )
  )
  
)


;;Heuristic definition
(defun h1 (current_state goal_points)
"Calcula a heuristica h(x) = o(x)/m(x)
m(x) é a média por casa dos pontos que constam no tabuleiro x,
o(x) é o número de pontos que faltam para atingir o valor definido como objetivo."
    (let (  (size 100) 
            (total_points (sum_total_points (get_state_board_points current_state)))
            (current_points (state-points current_state))
        )
        (if (= total_points 0)
          0
          (/  (- goal_points current_points)
              (/ total_points size)
          )
        )
    )
)
(defun h2 (current_state goal_points)
  (let* (  (size 100) 
            (total_points (sum_total_points (get_state_board_points current_state)))
            (current_points (state-points current_state))
            (cur_pos (state-position current_state))
            (x (horse_position-x cur_pos))
            (y (horse_position-y cur_pos))
            (sum_possible_plays_points
              (reduce #'+
                (remove nil (mapcar #'(lambda (pos) (get_state_square current_state pos))
                  (list
                    (position_constructer (+ x 1) (+ y 2))
                    (position_constructer (+ x 1) (+ y -2))
                    (position_constructer (+ x -1) (+ y -2))
                    (position_constructer (+ x -1) (+ y 2))
                    (position_constructer (+ x 2) (+ y 1))
                    (position_constructer (+ x 2) (+ y -1))
                    (position_constructer (+ x -2) (+ y -1))
                    (position_constructer (+ x -2) (+ y 1))
                  ))
                )
              )
            )
        )
        
        (if (= total_points 0)
          0
          (/  (* (- goal_points current_points) (/ sum_possible_plays_points 100))
              (/ total_points size)
          )
        )
  )
)
  

;auxiliary function
(defun sum_total_points (board_points)
"Soma todos os pontos do tabuleiro"
  (reduce #'+ (remove nil (mapcan #'flatten board_points)))
)
(defun flatten (lst)
"Recebe uma lista de listas e retorna uma lista"
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
"Um movimento do cavalo possivel"
  (move_piece state '(-1 -2))
)
(defun move_horse_up_rigth (state)
"Um movimento do cavalo possivel"
  (move_piece state '(1 -2))
)
(defun move_horse_rigth_up (state)
"Um movimento do cavalo possivel"
  (move_piece state '(2 -1))
)
(defun move_horse_rigth_down (state)
"Um movimento do cavalo possivel"
  (move_piece state '(2 1))
)
(defun move_horse_down_left (state)
"Um movimento do cavalo possivel"
  (move_piece state '(-1 2))
)
(defun move_horse_down_rigth (state)
"Um movimento do cavalo possivel"
  (move_piece state '(1 2))
)
(defun move_horse_left_up (state)
"Um movimento do cavalo possivel"
  (move_piece state '(-2 1))
)
(defun move_horse_left_down (state)
"Um movimento do cavalo possivel"
  (move_piece state '(-2 -1))
)
;auxiliary functions
(defun transform_position (position vector)
"Calcula o novo ponto a partir de um vector de movimento"
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
"Calcula a nova posição do cavalo a partir de um vector de movimento"
  (if (null state) 
    nil
    (let ((new_position (transform_position (state-position state) movement_vector)))
      (if (or (NULL new_position) (NULL (get_state_square state new_position)))
        NIL
        (calculate_new_state state new_position)
      )
    )
  )
)
(defun calculate_new_state (state new_position)
"Calcula o novo estado depois do movimento do cavalo"
  (state_constructer_with_params 
    (set_board_value (get_state_board_points state) new_position NIL) 
    (+ (state-points state) (get_state_square state new_position))
    new_position
  )
)
(defun set_board_value (board_list position value)
"Troca o valor de uma celula"
  (let* ( (line (nth (horse_position-y position) board_list))
          (new_line (set_list_value (horse_position-x position) line value))
        )
      (set_list_value (horse_position-y position) board_list new_line)
  )
)
(defun set_list_value (index line value)
"Troca o valor de index da lista"
  (append (append (subseq line 0 index) (list value))
          (cdr (subseq line index))
  )
)

;;Inicio do jogo
(defun initial_position (init_state x)
"Inicia o estado numa das 10 posições no topo do tabuleiro"
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
;auxiliary functions
(defun find_position_inicial_game (state)
"Enforça as resgras do inicio do jogo, se o numero for de duplo algarismo 
troca o de duplo algarimo mais pequeno que encontra por nil caso contrario 
troca o de algarimos inversos por nil"
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
"Encontra o menor numero de duplo algarismo"
  (find_one_list_element board_list (list 0 11 22 33 44 55 66 77 88 99))
)
(defun find_one_list_element (board_list list)
"Encontra um numero de uma lista no Board"
  (let ((pos (seach_board board_list (car list))))
    (if (and (NULL pos) (> (length list) 1)) 
      (find_one_list_element board_list (cdr list)) 
      pos
    )
  )
)
(defun find_inverse (board_list value)
"Encontra o numero com os algarismo invertidos"
  (seach_board  
        board_list 
        (+ (* (mod value 10) 10) (floor (/ value 10)))
  )
)
(defun seach_board (board_list value)
"Encontra um numero no Board"
  (let ((index (position value (apply #'append board_list) :test #'equal)))
    (if (NULL index) NIL
      (position_constructer (mod index 10) (floor (/ index 10)))
    )
  )
)
(defun double_algarisms (numb)
"Verifica se o numero é de duplo algarismo" 
  (= (mod numb 10) (floor (/ numb 10)))
)
