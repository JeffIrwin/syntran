import math

def main():
    n = 5_000_000
    a = [1.0e-6 * i for i in range(n)]; b = [0.0] * n
    for k in range(20): b = [y + math.sqrt(abs(x)) + math.exp(-x * x) for x, y in zip(a, b)]
    print(sum(b))

main()
