import random
points = list(range(100))

# Inicializando uma matriz 10x10 com zeros
rows = 10
cols = 10

# Usando list comprehension para criar a matriz
board = [[0 for _ in range(cols)] for _ in range(rows)]

for i in range(10):
    for j in range(10):
        index = random.randint(0, len(points) - 1)
        value = points[index]
        board[i][j] = value
        points.remove(value)

# Exibindo a matriz
for row in board:
    print(row)