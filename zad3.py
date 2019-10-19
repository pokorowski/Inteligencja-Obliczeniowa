import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt

x = np.random.randint(0, 21, size=100)
y =np.array([0]*21)
y_pos = list(range(0, 21))
i = 0

while i < len(x): 
    y[x[i]] +=1
    i+=1

plt.bar(y_pos,y, align='center')
plt.xticks(y_pos,y_pos)
plt.ylabel('liczba wystapien')
plt.xlabel('liczby')
plt.title('rozklad 100 losowych liczb')
plt.show()
