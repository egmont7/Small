'''
Created on Aug 23, 2014

@author: caleb
'''
from enum import Enum
from vector import Vector


def frange(start, stop, step):
    i = 0
    while True:
        x = start + step*i
        if x > stop:
            return
        yield x
        i += 1


class Card(Enum):
    NORTHWEST = 0
    NORTHEAST = 1
    SOUTHWEST = 2
    SOUTHEAST = 3

    @staticmethod
    def get_card(origin, pos):
        if pos.x < origin.x:
            if pos.y < origin.y:
                return Card.NORTHWEST
            else:
                return Card.SOUTHWEST
        else:
            if pos.y < origin.y:
                return Card.NORTHEAST
            else:
                return Card.SOUTHEAST


class Sprite:
    def __init__(self, pos, rad):
        self.pos = pos
        self.rad = rad

    axisFns = [["GetLeft", "GetRight"], ["GetTop", "GetBottom"]]

    def GetLeft(self):
        return self.pos.x - self.rad

    def GetRight(self):
        return self.pos.x + self.rad

    def GetTop(self):
        return self.pos.y - self.rad

    def GetBottom(self):
        return self.pos.y + self.rad


class Body(Sprite):
    def __init__(self, mass, pos, vel=0, rad=.01,
                 color=(255, 0, 0), value=1, name=""):
        super().__init__(pos, rad)

        self.name = name
        self.color = color
        self.mass = mass
        self.vel = vel
        self.value = value


class SafeZone(Sprite):
    def __init__(self, pos, rad):
        super().__init__(pos, rad)


class Player(Sprite):
    def __init__(self, pos, vel, power, mass, rad):
        super().__init__(pos, rad)
        self.vel = vel
        self.power = power
        self.mass = mass
        self.dir = Vector()
        self.touching = []
        self.score = 0
        self.inSafe = False

    def clearMotion(self):
        self.dir = Vector()

    def addMotion(self, vect):
        self.dir += vect

    def updateScore(self):
        for t in self.touching:
            self.score += t.value
        temp = self.touching
        self.touching = []
        return temp
