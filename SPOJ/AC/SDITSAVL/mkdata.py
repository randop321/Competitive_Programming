import numpy as np

k = 0
n = 200000
print(n)
for i in range(n):
    if (i&1 == 0):
        k = np.random.randint(1, 1000000000)
    print("%d %d"%((i&1)+1, k))
        
