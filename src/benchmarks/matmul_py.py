n = 300
m = [[1.0e-3 * (i + n * j) for j in range(n)] for i in range(n)]  # row lists, m[i][j]
c = m
for k in range(20):
    ct = list(zip(*c))
    c = [[1.0e-3 * sum(x * y for x, y in zip(row, col)) for col in ct] for row in m]
print(sum(map(sum, c)))
