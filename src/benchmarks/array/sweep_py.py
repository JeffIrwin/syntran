import sys

def main():
    n = int(sys.argv[1])
    x = [0.0] * n; v = [1.0] * n; g = [-9.8] * n; dt = 1.0e-6
    for k in range(60_000_000 // n):
        v = [vi + dt * gi for vi, gi in zip(v, g)]
        x = [xi + dt * vi for xi, vi in zip(x, v)]
    print(sum(x))

main()
