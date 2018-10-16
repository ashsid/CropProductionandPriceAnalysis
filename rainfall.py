import csv
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import style
import matplotlib.patches as mpatches
import time

mPlace = "JHARKHAND"
mYear = 1901
Year = mYear
max1 =0

style.use("ggplot")

file=open("rainfall in india 1901-2015.csv")
reader = csv.reader(file)

for months in reader:
    break;
months=months[2:14:1]

rainfall=[]
year=[]
month=[]

def annual_rain_state():
    file.seek(0)
    reader = csv.reader(file)
    next(reader)
    rainfall=[]
    year=[]
    for row in reader:
        if(row[0]== mPlace and row[14]!="NA"):
            year.append(row[1])
            rainfall.append(float(row[14]))
    plt.plot(year,rainfall)
    plt.show()
    plt.close()

rain_month=[0,0,0,0,0,0,0,0,0,0,0,0]
def rain_pattern():
    global Year
    global rain_month
    count=0
    file.seek(0)
    reader = csv.reader(file)
    next(reader)
    global max1
    max1=0
    while(Year<=2015):
        for row in reader:
            if(row[0] == mPlace and float(row[1])>= Year):
                count+=1
                #print(row)
                row_count=0
                for rain in row[2:14:1]:
                    if(rain!="NA"):
                        if(max1<float(rain)):
                            max1=float(rain)
                        rain_month[row_count]+=float(rain)
                    else:
                        count-=1
                        break
                    row_count+=1
                break
        Year+=1
    rain_month=[ x/count for x in rain_month]
    print(rain_month)
    
    ani = animation.FuncAnimation(fig,animate,interval=500)
    plt.show()
    plt.close()

fig = plt.figure()
ax1 = plt.subplot(1,1,1)

def animate(i):
    global mYear
    rainfall=[]
    file.seek(0)
    reader = csv.reader(file)
    next(reader)
    for row in reader:
        if(row[0] == mPlace and float(row[1])>= mYear):
            for rain in row[2:14:1]:
                if(rain!="NA"):
                    rainfall.append(float(rain))
                else:
                    break
            break
    if(len(rainfall)==12):
        ax1.clear()
        ax1.set_xlabel(mYear)
        ax1.set_ylabel("mm of rainfall")
        ax1.set_ylim([0,max1*1.1])
        ax1.plot(range(0,12),rainfall,color="r")
        ax1.plot(range(0,12),rain_month,color="b")
    mYear+=1

def rain_state():
    file.seek(0)
    reader = csv.reader(file)
    next(reader)
    class_colors=['b','g','r','c','m','y']*6
    states=[]
    for row in reader:
        states.append(row[0])
    states=list(set(states))
    file.seek(0)
    next(reader)
    states_rain={}
    for state in states:
        count=0
        row_state=[]
        for row in reader:
            if(row[0] == state and row[14]!="NA"):
                count+=1
                row_state.append(float(row[14]))
        states_rain[state]=sum(row_state)/count
        file.seek(0)
        next(reader)
    circles=[]
    for i in range(0,36):
        circle=mpatches.Circle((0,0),2,fc=class_colors[i])
        circles.append(circle)

    plt.xticks(np.arange(36),list(states_rain.keys()),rotation=45)
    plt.bar(range(0,36),list(states_rain.values()),color=class_colors)
    plt.show()

def menu():
    global mPlace
    global mYear,Year,rain_month
    file.seek(0)
    reader = csv.reader(file)
    states=[]
    next(reader)
    for row in reader:
        states.append(row[0])
    states=list(set(states))
##    for i in range(len(states)):
##        print(states[i],"\t",end='')
##        if(i%3==2):
##            print("\n")
    print("press 1 to see the annual rain pattern over the years")
    print("press 2 to see the monthly rain pattern over the years")
    print("press 3 to see the average rain of the state")
    print("Enter any other key to exit")
    a=int(input())
    if a==1:
        state = input("Enter the name of the state")
        mPlace=state
        annual_rain_state()
    elif a==2:
        state = input("Enter the name of the state")
        mPlace=state
        rain_pattern()
    elif a==3:
        rain_state()
                
menu()
