size = 21
grid = [[0 for y in range(size)] for x in range(size)]


def indices():
    # [(x, n-x) | x <- [0..n]]
    coords = []
    for n in range(size):
        for x in range(n+1):
            coords.append((x, n-x))
    # [(19-x, 19-(n-x)) | x <- [0..n]]
    coords += [(size-1-x, size-1-y) for (x,y) in reversed(coords)]
    return coords[1:]

grid[0][0] = 1
for (x,y) in indices():
    prevx = grid[x-1][y] if x - 1 >= 0 else 0
    prevy = grid[x][y-1] if y - 1 >= 0 else 0
    pathshere = prevx + prevy
    grid[x][y] = pathshere
    print(x, y, pathshere, prevx, prevy)

print(grid)
print(grid[-1][-1])
