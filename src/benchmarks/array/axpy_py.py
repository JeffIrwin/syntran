def main():
    n = 10_000_000
    a = [1.0e-7 * i for i in range(n)]; b = [0.5 * x for x in a]
    for k in range(50): a = [x * 0.999 + y for x, y in zip(a, b)]
    print(sum(a))

main()
