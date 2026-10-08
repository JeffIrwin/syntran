import numpy as np
n = 10_000_000
a = 1.0e-7 * np.arange(n); b = 0.5 * a
for k in range(50): a = a * 0.999 + b
print(a.sum())
