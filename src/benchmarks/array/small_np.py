import numpy as np

def main():
    x = np.zeros(3); v = np.array([1.0, 2.0, 3.0]); g = np.array([0.0, -9.8, 0.0]); dt = 1.0e-6
    for k in range(2_000_000):
        v = v + dt * g
        x = x + dt * v
    print(x.sum())

main()
