from random import randint

# core function
min_row_count = 3
max_row_count = 11

point_to_row = []
for i in range(max_row_count):
    for j in range(i+1):
        point_to_row.append(i)

def row_to_points(row_number):
    min_number = (1 + row_number) * row_number // 2
    max_number = min_number + row_number
    return min_number, max_number

def connect_right(board, point_number):
    points = board['points']
    over = point_number + 1
    dest = over + 1
    row_number = point_to_row[point_number]
    _, row_max_point = row_to_points(row_number)
    if point_number == row_max_point or over == row_max_point:
        return
    points[point_number]['connections'][dest] = over
    points[dest]['connections'][point_number] = over

def connect_down_left(board, point_number):
    points = board['points']
    row_number = point_to_row[point_number]
    if row_number > board['row_count']-3:
        return
    over = point_number + row_number + 1
    dest = over + row_number + 2
    points[point_number]['connections'][dest] = over
    points[dest]['connections'][point_number] = over

def connect_down_right(board, point_number):
    points = board['points']
    row_number = point_to_row[point_number]
    if row_number > board['row_count']-3:
        return
    over = point_number + row_number + 2
    dest = over + row_number + 3
    points[point_number]['connections'][dest] = over
    points[dest]['connections'][point_number] = over

def create_board(row_count):
    point_count = (1 + row_count) * row_count // 2
    points = [dict(pegged=True, connections={}) for i in range(point_count)]
    points[randint(0, point_count-1)]['pegged'] = False
    board = dict(row_count=row_count, points=points)
    for i in range(point_count):
        connect_right(board, i)
        connect_down_left(board, i)
        connect_down_right(board, i)
    return board

# move functions
def can_move(board, from_point, to_point):
    points = board['points']
    if ((0 <= from_point < len(points)) and
        (0 <= to_point < len(points)) and
        points[from_point]['pegged'] and
        not points[to_point]['pegged']):
        over = points[from_point]['connections'].get(to_point, -1)
        if over >= 0 and points[over]['pegged']:
            return True
    return False

def movable(board, point_number):
    points = board['points']
    if 0 <= point_number < len(points):
        for dest in points[point_number]['connections'].keys():
            if can_move(board, point_number, dest):
                return True
    return False

def is_gameover(board):
    points = board['points']
    for i in range(len(points)):
        if movable(board, i):
            return False
    return True

def move(board, from_point, to_point):
    points = board['points']
    over_point = points[from_point]['connections'][to_point]
    points[from_point]['pegged'] = False
    points[over_point]['pegged'] = False
    points[to_point]['pegged'] = True

# UI function
peg_string = '+'
nopeg_string = '-'
point_width = 3
point_string = ([chr(i) for i in range(ord('a'), ord('z')+1)] +
                [chr(i) for i in range(ord('A'), ord('Z')+1)] +
                [chr(i) for i in range(ord('0'), ord('9')+1)] +
                ['@', '#', '$', '%'])
string_to_point = {}
for i, ch in enumerate(point_string):
    string_to_point[ch] = i

def point_str(points, point_number):
    return point_string[point_number] + (peg_string if points[point_number]['pegged'] else nopeg_string) + ' '

def print_board(board):
    row_count = board['row_count']
    points = board['points']
    width = point_width * row_count
    for row_number in range(row_count):
        padding_count = (width - point_width * (row_number+1)) // 2
        padding = ''.join(' ' for i in range(padding_count))
        min_point, max_point = row_to_points(row_number)
        row_string = ''.join(point_str(points, i) for i in range(min_point, max_point+1))
        print(padding+row_string)
        
    
def main():
    line = input("Please input the row count[5]:")
    try:
        row_count = int(line)
    except ValueError:
        row_count = 5
    board = create_board(row_count)
    while True:
        if is_gameover(board):
            pegged = 0
            for p in board['points']:
                if p['pegged']:
                    pegged += 1
            print("Game Over! {} pegged left".format(pegged))
            break
        print_board(board)
        while True:
            line = input("Please specify where to where:")
            ps = [x for x in list(line) if len(x.strip()) > 0]
            if len(ps) == 2:
                from_point = string_to_point.get(ps[0], -1)
                to_point = string_to_point.get(ps[1], -1)
                if from_point >= 0 and to_point >= 0 and can_move(board, from_point, to_point):
                    move(board, from_point, to_point)
                    break
            print("Invalid input:", line)
        

if __name__ == '__main__':
    main()
