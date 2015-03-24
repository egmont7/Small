'''
Created on Aug 24, 2014

@author: Chris McCormick (http://mccormick.cx/news/entries/RDC.news)
'''
from itertools import combinations
from utils import Vector, Body, Player
import physics


class Boundary:
    def __init__(self, type_, position, obj):
        self.type_ = type_
        self.position = position
        self.obj = obj


class RDC:
    def __init__(self, entities):
        self.axisFns = entities[0].axisFns
        self.entities = entities
        # which axis we are currently checking on in this pass
        self.idx = 0
        # test if there has been a division or not
        self.divided = True

    def DoSort(self, clusters, axis):
        # we're going to replace all clusters with new found ones
        newclusters = []
        # assume that we won't divide any further
        self.divided = False

        # for every sub cluster in our group of clusters
        for c in clusters:
            boundaries = []
            count = 0
            group = []

            # store the intervals for a given axis in a list data structure
            for obj in c:
                boundaries.append(Boundary('o', getattr(obj, axis[0])(), obj))
                boundaries.append(Boundary('c', getattr(obj, axis[1])(), obj))

            # sort our list of all boundaries on position
            boundaries.sort(key=lambda x: x.position)

            # finally, make new chunks out of our existing collection
            for i in range(len(boundaries)):
                b = boundaries[i]
                # if we find a leading edge, increment our count
                # and push it onto our stack of the current group
                if b.type_ == "o":
                    count += 1
                    group.append(b.obj)
                elif b.type_ == "c":
                    count -= 1
                    # if we have finished finding a group
                    # push this group onto the stack on new clusters
                    # empty out our group
                    if count == 0:
                        newclusters.append(group)
                        group = []
                        # if we're not at the very end of our array then
                        # we've just made a new subdivision
                        if i != len(boundaries) - 1:
                            self.divided = True

        return newclusters

    def FindGroups(self, group):
        clusters = [group]
        while (self.divided):
            # find the new clusters
            clusters = self.DoSort(clusters, self.axisFns[self.idx])
            # select the next axis for subdivision
            self.idx = (self.idx + 1) % len(self.axisFns)
        return clusters

    def DoRDC(self):
        groups = self.FindGroups(self.entities)
        for g in range(len(groups)):
            self.BruteForceCircles(groups[g])
        for e in self.entities:
            if isinstance(e, Body):
                if e.pos.x + e.rad > 1:
                    e.vel.x *= -1
                    e.pos.x = 1 - e.rad
                if e.pos.x - e.rad < 0:
                    e.vel.x *= -1
                    e.pos.x = e.rad
                if e.pos.y + e.rad > 1:
                    e.vel.y *= -1
                    e.pos.y = 1 - e.rad
                if e.pos.y - e.rad < 0:
                    e.vel.y *= -1
                    e.pos.y = e.rad
            elif isinstance(e, Player):
                if e.pos.x + e.rad > 1:
                    e.vel.x = 0.
                    e.pos.x = 1 - e.rad
                if e.pos.x - e.rad < 0:
                    e.vel.x = 0.
                    e.pos.x = e.rad
                if e.pos.y + e.rad > 1:
                    e.vel.y = 0.
                    e.pos.y = 1 - e.rad
                if e.pos.y - e.rad < 0:
                    e.vel.y = 0.
                    e.pos.y = e.rad

    def BruteForceCircles(self, group):
        for a, b in combinations(group, 2):
            collided = Vector.Distance(a.pos, b.pos) < (a.rad + b.rad)
            if collided:
                physics.collide(a, b)
