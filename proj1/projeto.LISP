;;caminho "C:\\Users\\ruies\\Documents\\programacao\\3ano\\IA\\proj1\\problemas.dat"
(defun path () (format nil "C:\\Users\\ruies\\Documents\\programacao\\3ano\\IA\\proj1\\problemas.dat"))
(defun example_board (n) (nth n (split_file_into_lists (read_file (path)))))


(defun app_start_whith_default_path ()
  (app_console (split_file_into_lists (read_file "C:\\Users\\ruies\\Documents\\programacao\\3ano\\IA\\proj1\\problemas.dat")))
)

;;Inicio Aplicação
(defun app_start ()
"Inicia a aplicação"
  (app_console (load_boards))
)
;;Consola da Aplicação
(defun app_console (boards)
"Mostra o menu principal."
  (format t "~&Opções:~%")
  (format t "1. Escolher tabuleiro~%")
  (format t "2. Recarregar tabuleiros~%")
  (format t "0. Sair~%")
  (let ((option (show_question "Qual é a sua opção")))
    (cond ((not(integerp option)) (format t "Opção invalida. Tente novamente.~%") (app_console boards))
          ((= option 0) (format t "Saindo do menu.~%"))
          ((= option 1) (format t "Escolher um tabuleiro.~%") (board_console boards))
          ((= option 2) (format t "Recarregando os tabuleiros.~%") (app_console (load_boards)))
          (t (format t "Opção invalida. Tente novamente.~%") (app_console boards))
    )
  )
)
(defun board_console (boards)
"Mostra o Menu para escolher o tabuleiro do jogo"
  (print_board_options boards)
  (format t "~&Opções:~%")
  (format t "N. Numero do tabuleiro escolhido.~%")
  (format t "0. Voltar~%")
  (let ((option (show_question "Qual é a sua opção")))
    (cond ((not(integerp option)) 
            (format t "Opção invalida. Tente novamente.~%") 
            (board_console boards)
          ) 
          ((= option 0) (format t "Voltar ao menu Principal.~%") 
            (app_console boards)
          )
          ((integerp option) 
            (format t "Tabuleiro ~a Escolhido!.~%" option) 
            (let ((board (get_board option boards)))
              (print_board board)
              (algoritm_console board)
            )
            
            (board_console boards)
          )
          (t (format t "Opção invalida. Tente novamente.~%") 
            (board_console boards)
          )
    )
  )
)
(defun algoritm_console (board)
"Mostra o Menu para escolher o Algoritmo"
  (cond ((NULL board) (format t "Tabuleiro não existe. Tente novamente.~%"))
        (T  (format t "~&Opções:~%")
            (format t "1. BFS.~%")
            (format t "2. DFS.~%")
            (format t "3. A*.~%")
            (format t "0. Voltar~%")
            (let ((option (show_question "Qual é a sua opção")))
              (cond ((not(integerp option)) 
                      (print_board board)
                      (algoritm_console board)
                    )
                    ((= option 0) 
                      (format t "Voltando aos tabuleiros")
                    )
                    ((= option 1)
                      (bfs_menu board)
                      (print_board board)
                      (algoritm_console board)
                    )
                    ((= option 2)
                      (dfs_menu board)
                      (print_board board)
                      (algoritm_console board)
                    )
                    ((= option 3)
                      (astar_menu board)
                      (print_board board)
                      (algoritm_console board)
                    )
                    (T  (print_board board)
                        (algoritm_console board)
                    )
              )
            )
        )
  )
)
(defun  bfs_menu(board)
"Menu do algoritmo BFS"
  (format t "~%BFS")
  (show_question "Qual é o objetivo de pontos?")
)
(defun  dfs_menu(board)
"Menu do algoritmo DFS"
  (format t "~%DFS")
  (show_question "Qual é o objetivo de pontos?")
  (show_question "Qual é a profundidade maxima?")
)
(defun  astar_menu(board)
"Menu do algoritmo A*"
  (format t "~%A*")
  (show_question "Qual é o objetivo de pontos?")
  (show_question "Qual é o nome da função heuristica?")

  ;(funcall (symbol-function (read_option)))
)
(defun load_boards ()
"Carrega os dados de um ficheiro em listas"
  (let  ((path (show_question "Qual o caminho para o ficheiro com os dados dos tabuleiros?")))
        (split_file_into_lists (read_file path))
  )
)
(defun show_question (question)
"Mostra na terminal uma pergunta e retorna a resposta do utilizador"
  (format t "~%> ~a" question)
  (read_option)
)
(defun read_option ()
  "Le o input do utilizador."
  (format t "~%>> ")
  (finish-output)
  (let ((opcao (read))) (clear-input) opcao)
)
(defun print_board_options (boards)
"Mostra na terminal todos os tabuleiros recebidos por parametro"
  (print_each_board 0 boards)
)


;;Print Boards
(defun print_board (board)
"Mostra no terminal uma lista de listas de 10*10 em forma de tabela"
  (format t 
  "
         A     B     C     D     E     F     G     H     I     J
  1   ~a
  2   ~a
  3   ~a
  4   ~a
  5   ~a
  6   ~a
  7   ~a
  8   ~a
  9   ~a
  10  ~a
  " 
  (format_line (nth 0 board))
  (format_line (nth 1 board))
  (format_line (nth 2 board))
  (format_line (nth 3 board))
  (format_line (nth 4 board))
  (format_line (nth 5 board))
  (format_line (nth 6 board))
  (format_line (nth 7 board))
  (format_line (nth 8 board))
  (format_line (nth 9 board))
  )
)
(defun format_line (list)
"Formata uma lista"
  (format nil "| ~{~<~a~>~^ ~}" (mapcar (lambda (n) (format nil "~v,' d |" 3 n)) list))
)

(defun print_each_board (index boards)
  (let ((board (get_board index boards)))
    (cond ((NULL board) (format t " "))
          (T  (format t "~%> Tabuleiro - ~a" (+ 1 index))
              (print_board board)
              (print_each_board (+ index 1) boards)
          )
    )
  )
)
(defun get_board (index boards)

  (cond ((OR (NULL boards) (ATOM boards)) NIL)
        ((>= index (length boards)) NIL)
        (T (nth index boards))
  )
)


;;retorna a lista de listas
(defun split_file_into_lists (file_content)
"Converte o conteudo do ficheiro com os dados em listas"
  (mapcar #'read-from-string (split-sequence "*" file_content))
)


;;Ler ficheiro
(defun read_file (file_path)
  "Lê o conteúdo de um arquivo de texto e retorna como uma string."
  (handler-case
      (with-open-file (file file_path :direction :input)
        (let* ((content (make-string (file-length file)))
               (bytes_read (read-sequence content file)))
          (subseq content 0 bytes_read)))
    (serious-condition (e)
      (format *error-output* "Erro ao abrir o arquivo: ~A~%" e)
      nil))
)