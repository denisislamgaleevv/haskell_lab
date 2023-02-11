


f = open('17.txt')
l = [int(x) for x in f]


c = 0
m = 0 
for i in range (0, len(l)-1):
    for j in range (i, len(l)):
        if ((l[i] -l[j])%2==0) and ((l[i] %31 ==0)or(l[j]%31 ==0)):
            c+=1
            m = max(m,   l[i] +l[j])
            
print(c, m)
