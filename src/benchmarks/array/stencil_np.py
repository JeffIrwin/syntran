import numpy as np

def main():
    n = 1_000_000
    u = 1.0e-6 * np.arange(n); u = u * (1.0 - u)
    for k in range(200):
        u[1:n-1] = u[1:n-1] + 0.25 * (u[0:n-2] - 2.0 * u[1:n-1] + u[2:n])
    print(u.sum())

main()
