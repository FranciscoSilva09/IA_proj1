AStar(initial_node):
  OPEN = [initial_node]  # Priority queue ordered by f(n)
  CLOSED = {}

  while OPEN is not empty:
    current_node = remove_min(OPEN)
    add_to(CLOSED, current_node)

    if is_goal(current_node):
      return solution(current_node)

    successors = expand(current_node)
    for successor in successors:
      if successor not in OPEN and successor not in CLOSED:
        calculate_costs(successor, current_node)
        add_to(OPEN, successor)
      elif successor in OPEN:
        update_costs_if_lower(successor, current_node)
      elif successor in CLOSED:
        update_costs_if_lower(successor, current_node)
        move_to_open_if_lower_cost(successor, OPEN)

  return failure
