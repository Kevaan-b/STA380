def min_cost(need_x, need_y, price_x, price_y, price_xy):
    best_cost = float('inf')

    for b in range(max(need_x, need_y) + 1):
        remaining_x = max(need_x - b, 0)
        remaining_y = max(need_y - b, 0)

        cost = (b * price_xy) + (remaining_x * price_x) + (remaining_y * price_y)
        best_cost = min(best_cost, cost)

    return best_cost


def min_cost_but_better(need_x, need_y, price_x, price_y, price_xy):
    bestCombo = min(price_xy, price_x + price_y)
    costOne = bestCombo*min(need_x, need_y) + price_x*max(need_x-need_y, 0) + price_y*max(need_y-need_x, 0)
    costTwo = bestCombo*max(need_x, need_y)

    return min(costOne, costTwo)

if __name__ == "__main__":
    need_x = 3
    need_y = 30
    price_x = 30
    price_y = 30
    price_xy = 3000

    result = min_cost(need_x, need_y, price_x, price_y, price_xy)
    resultTwo = min_cost_but_better(need_x, need_y, price_x, price_y, price_xy)
    print(resultTwo)
    print(resultTwo)