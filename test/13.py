def judge(l1,l2):
    if type(l1)!=list:
        l1=[l1]
    if type(l2)!=list:
        l2=[l2]
    idx=0
    while idx!=len(l1) and idx!=len(l2):
        if type(l1[idx])==list or type(l2[idx])==list:
            res=judge(l1[idx],l2[idx])
            if res!= None:
                return res
        else:
            if l1[idx]>l2[idx]:
                return False
            elif l1[idx] < l2[idx]:
                return True
        idx=idx+1
    if idx==len(l1) and idx==len(l2):
        return None
    if idx==len(l1):
        return True
    return False
file=open("data/13.txt")
result=0
i=0
while True:
    line1=file.readline()
    line2=file.readline()
    line=file.readline()
    l1=eval(line1)
    l2=eval(line2)
    i=i+1
    if judge(l1,l2):
        result=result+i
    if line:
        continue
    else:
        break
file.close()
print(result)
    
file=open("data/13.txt")
ans=[]
while True:
    line1=file.readline()
    line2=file.readline()
    line=file.readline()
    l1=eval(line1)
    l2=eval(line2)
    for k in [l1,l2]:
        isadd=False
        for (i,t) in enumerate(ans):
            if judge(k,t):
                ans.insert(i,k)
                isadd=True
                break
        else:
            ans.append(k)
    if line:
        continue
    else:
        break

for k in [[[2]],[[6]]]:
    isadd=False
    for (i,t) in enumerate(ans):
        if judge(k,t):
            ans.insert(i,k)
            isadd=True
            break
    else:
        ans.append(k)
file.close()
print((ans.index([[2]])+1)*(ans.index([[6]])+1))
