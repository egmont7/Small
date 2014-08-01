#!/usr/bin/env python3
"""
Reads data from an arduino running the companion oven_control sketch.
Timestamps data and pushes to an output file.
"""
import serial
from datetime import datetime

def read(ser):
    data = bytes()
    endl = '\n'.encode('ascii')
    while True:
        c = ser.read();
        if c == endl:
            return data.decode('ascii').strip()
        else:
            data += c

def process(ser, outfile):
    while True:
        data = read(ser)
        if '#' in data:
            out_data = "{0}\n".format(data)
        else:
            out_data = "{0}:{1}\n".format(data,datetime.now())
        print(out_data,end='')
        outfile.write(out_data)
        outfile.flush()

def main():
    with serial.Serial('/dev/ttyUSB0') as ser:
        with open('outfile.txt','w') as outfile:
            process(ser, outfile)

if __name__ == '__main__':
    main()