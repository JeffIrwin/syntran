
n = 30000
#n = 100

prime = 0

for i in range(0, n):
    #print("i = ", i)

    is_composite = False;

    for j in range(2, i//2 + 1):
        divisible = j * (i // j) == i
        is_composite = is_composite or divisible

    if not is_composite:
        prime = i

print("prime = ", prime)

