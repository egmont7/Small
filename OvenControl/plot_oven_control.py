#!/usr/bin/env python3
from  datetime import datetime as datetime
import matplotlib.pyplot as plt
import matplotlib as mat

def main():
    with open('outfile.txt','r') as f:
        data = [l.strip() for l in f.readlines()]
    data = [d.split(':') for d in data if '#' not in d]
    temps = [float(d[0]) for d in data]
    dates = [':'.join(d[1:]) for d in data]
    dates = [datetime.strptime(d.split('.')[0],"%Y-%m-%d %H:%M:%S") for d in dates]
    dates = [mat.dates.date2num(d) for d in dates]
    plt.plot_date(dates, temps)
    plt.xlabel('Time')
    plt.ylabel('Temperature')
    plt.show()

if __name__ == '__main__':
    main()
