import numpy as np
np.seterr(all="ignore")  # Accelerate sets spurious FP flags in matmul, which numpy reports as warnings
n = 300
m = 1.0e-3 * np.arange(n * n, dtype=float).reshape((n, n), order='F')
c = m
for k in range(20):
    c = 1.0e-3 * (m @ c)
print(c.sum())
