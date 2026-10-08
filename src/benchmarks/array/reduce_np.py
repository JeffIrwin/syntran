import numpy as np
n = 10_000_000
a = 1.0e-7 * np.arange(n); b = 1.0 - a; s = 0.0
for k in range(50): s += np.sum(a * b) + (a @ b) + np.max(a - b)
print(s)
