import sys
import numpy as np
n = int(sys.argv[1])
x = np.zeros(n); v = np.ones(n); g = np.full(n, -9.8); dt = 1.0e-6
for k in range(60_000_000 // n):
    v = v + dt * g
    x = x + dt * v
print(x.sum())
