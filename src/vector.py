'''
Created on Aug 23, 2014

@author: caleb
'''

import math
class Vector:
    'Represents a 2D vector.'
    def __init__(self, x = 0, y = 0):
        self.x = float(x)
        self.y = float(y)
        
    def __add__(self, val):
        return Vector(self[0] + val[0], self[1] + val[1])
    
    def __sub__(self,val):
        return Vector(self[0] - val[0], self[1] - val[1])
    
    def __truediv__(self, val):
        return Vector(self[0] / val, self[1] / val)
    
    def __mul__(self, val):
        return Vector(self[0] * val, self[1] * val)
    
    def __getitem__(self, key):
        if( key == 0):
            return self.x
        elif( key == 1):
            return self.y
        else:
            raise Exception("Invalid key to Point")
        
    def __setitem__(self, key, value):
        if( key == 0):
            self.x = value
        elif( key == 1):
            self.y = value
        else:
            raise Exception("Invalid key to Point")
        
    def __str__(self):
        return "(" + str(self.x) + "," + str(self.y) + ")"
        
    @staticmethod
    def DistanceSqrd( point1, point2 ):
        return ( (point1[0]-point2[0])**2 + (point1[1]-point2[1])**2)
    
    @staticmethod
    def Distance( point1, point2 ):
        return math.sqrt( Vector.DistanceSqrd(point1,point2) )
        
    @staticmethod
    def LengthSqrd( vec ):
        return vec[0]**2 + vec[1]**2
        
    @staticmethod
    def Length( vec ):
        return math.sqrt( Vector.LengthSqrd(vec) )
        
    @staticmethod
    def Normalize( vec ):
        if( vec[0] == 0. and vec[1] == 0. ):
            return Vector(0.,0.)
        return vec / Vector.Length(vec)
        
    @staticmethod
    def Dot( a,b ):
        return a[0]*b[0] + a[1]*b[1]
        
    @staticmethod
    def ProjectOnto( w,v ):
        return v * Vector.Dot(w,v) / Vector.LengthSqrd(v)
