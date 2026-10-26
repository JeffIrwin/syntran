def main():
    n = 10_000_000
    a = [1.0e-7 * i for i in range(n)]; b = [1.0 - x for x in a]; s = 0.0
    for k in range(50):
        s += sum([x * y for x, y in zip(a, b)]) + sum(x * y for x, y in zip(a, b)) + max([x - y for x, y in zip(a, b)])
    print(s)

main()
