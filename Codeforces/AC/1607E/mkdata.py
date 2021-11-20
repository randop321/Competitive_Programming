import os,sys
import numpy as np

t = 10000
v = ["L", "R", "D", "U"]

print(str(t)+"\n",end="")
for i in range(t):
    n = np.random.randint(10) + 10
    m = np.random.randint(10) + 10
    print( str(n) + " " + str(m) + "\r\n", end="")
    s = np.random.randint(10) + 10
    cmd = ""
    for j in range(s):
        cmd = cmd + v[np.random.randint(4)]
    print(cmd+"\r\n",end="")


