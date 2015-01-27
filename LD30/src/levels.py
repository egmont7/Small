'''
Created on Aug 24, 2014

@author: caleb
'''
import math
import random
from utils import Body, SafeZone, Player, Vector, frange


class Level:
    
    
    def __init__(self):
        #Physics Parameters(Defaults)
        self.physics = {"G"               : 2,       #Strength of gravity
                        "SPEED_LIMIT"     : 0.2,     #Speed at which to apply damping
                        "OVERSPEED_DAMP"  : 0.9,     #Overspeed damping ratio
                        "NORMAL_DAMP"     : 0.99,    #Normal Damping
                        "DT"              : 0.0001,  #Time Step
                        "THETA"           : 2,       #Barnes-Hutt cutoff param
                        "RESTITUTION"     : 1,       #elastic collision parameter
                        "PLAYER_DAMP"     : 0.85
                        }
    
    def getBodies(self):
        raise NotImplementedError()
    
    def getSafeZones(self):
        raise NotImplementedError()
    def getPlayer(self):
        raise NotImplementedError()

class Level1(Level):
    
    def __init__(self):
        super().__init__()
        self.physics["RESTITUTION"] = 4.
        self.physics["G"] = 10
        self.physics["OVERSPEED_DAMP"] = 0.8
        self.physics["NORMAL_DAMP"] = 0.999
        self.physics["SPEED_LIMIT"] = 60
        
    def getBodies(self):
        bodies = []
        
        for r in [.3]:
            for theta in frange(0, 2*math.pi, .1):
                x = 0.5 + (r+.05)*math.cos(theta)
                y = 0.5 + (r+.05)*math.sin(theta)
                vx = 0.4*math.sin(theta)
                vy = -0.4*math.cos(theta)
                bodies.append(Body(mass=2, pos = Vector(x,y), vel = Vector(vx,vy), rad = 0.01, value = -1))
        good = random.sample(bodies,len(bodies)//3)
        for g in good:
            g.color = (0,255,0)
            g.value = 1
        return bodies
    
    def getSafeZones(self):
        zones = []
       # zones.append(SafeZone(Vector(.25,.25), .1))
       # zones.append(SafeZone(Vector(.25,.75), .1))
       # zones.append(SafeZone(Vector(.75,.25), .1))
       # zones.append(SafeZone(Vector(.75,.75), .1))
       # zones.append(SafeZone(Vector(.5,.5), .05))
        return zones
    
    def getPlayer(self):
        return Player(Vector(.5,.5),Vector(0,0), 150000, 150, 0.02)
    
class Level2(Level):
    
    
    def __init__(self):
        super().__init__()
        self.physics['NORMAL_DAMP'] = 1
        self.physics['G'] = 0
        self.physics['DT'] = .01
    
    def getBodies(self):
        bodies = []
        
        bodies.append(Body(mass = 1, pos = Vector(0.5,0.5), vel = Vector(0,0)))
        bodies.append(Body(mass = 1, pos = Vector(0.7,0.5), vel = Vector(-.19,0)))
        return bodies
    
    def getSafeZones(self):
        zones = []
        zones.append(SafeZone(Vector(.25,.25), .1))
        zones.append(SafeZone(Vector(.25,.75), .1))
        zones.append(SafeZone(Vector(.75,.25), .1))
        zones.append(SafeZone(Vector(.75,.75), .1))
        return zones
    
    def getPlayer(self):
        return Player(Vector(.5,.5),Vector(0,0), 1, 1, 0.2)
    
