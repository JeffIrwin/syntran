import numpy as np

def main():
    n = 5_000_000
    a = 1.0e-6 * np.arange(n); b = np.zeros(n)
    for k in range(20): b = b + np.sqrt(np.abs(a)) + np.exp(-a * a)
    print(b.sum())

main()
