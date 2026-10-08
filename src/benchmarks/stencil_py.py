n = 1_000_000
u = [1.0e-6 * i for i in range(n)]; u = [x * (1.0 - x) for x in u]
for k in range(200):
    u[1:n-1] = [u[i] + 0.25 * (u[i-1] - 2.0 * u[i] + u[i+1]) for i in range(1, n-1)]
print(sum(u))
