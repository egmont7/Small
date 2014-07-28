#!/usr/bin/env python3

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
        out_data = "{0}:{1}\n".format(data,datetime.now())
        print(out_data,end='')
        outfile.write(out_data)
        outfile.flush()



def main():
    with serial.Serial('/dev/ttyUSB1') as ser:
        with open('outfile.txt','w') as outfile:
            process(ser, outfile)


if __name__ == '__main__':
    main()
