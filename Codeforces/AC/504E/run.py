#!/usr/bin/env python

import os,sys
os.system('g++ -g mkdata.cpp -o mkdata')
os.system('g++ -g a.cpp -o a.out')
os.system('g++ -g check.cpp -o check')

while (1):
    os.system('./mkdata > inp.txt')
    os.system('./a.out < inp.txt > oup1.txt')
    os.system('./check < inp.txt > oup2.txt')
    a = open('oup1.txt', 'r').readlines()
    b = open('oup2.txt', 'r').readlines()
    if (len(a)!=len(b)):
        exit(0)
    for i in range(len(a)):
        if a[i]!=b[i]:
            exit(0)
