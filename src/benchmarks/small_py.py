x = [0.0, 0.0, 0.0]; v = [1.0, 2.0, 3.0]; g = [0.0, -9.8, 0.0]; dt = 1.0e-6
for k in range(2_000_000):
    v = [vi + dt * gi for vi, gi in zip(v, g)]
    x = [xi + dt * vi for xi, vi in zip(x, v)]
print(sum(x))
