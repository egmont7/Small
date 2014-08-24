'''
Created on Aug 23, 2014

@author: caleb
'''
from enum import Enum
from vector import Vector

from pygame.sprite import Sprite

class Card(Enum):
    NORTHWEST = 0
    NORTHEAST = 1
    SOUTHWEST = 2
    SOUTHEAST = 3
    
    @staticmethod
    def get_card(origin, pos):
        if pos.x < origin.x:
            if pos.y < origin.y:
                return Card.SOUTHWEST
            else:
                return Card.NORTHWEST
        else:
            if pos.y < origin.y:
                return Card.SOUTHEAST
            else:
                return Card.NORTHEAST

class Body(Sprite):
    def __init__(self, mass, pos, vel = 0, name = ""):
        super().__init__()
        
        self.name = name
        self.mass = mass
        self.pos = pos
        self.vel = vel
    
    def update(self, time_passed):
        self.pos = self.pos + self.vel
        self.vel = self.vel + (self.force / self.mass)
    
    def __str__(self):
        return self.name + ' ' + str(self.pos) + '\n'


class QuadTree:
    
    class QuadTreeNode:
        def __init__(self, parent = None, size = 0, pos = Vector(0,0)):
            self.children = {Card.NORTHWEST: None,
                             Card.NORTHEAST: None,
                             Card.SOUTHWEST: None,
                             Card.SOUTHEAST: None}
            self.parent = parent
            
            self.size = size
            self.pos = pos
            
            self.total_mass = 0
            self.center_of_mass = Vector(0,0)
            
        def insert(self, body):
            card = Card.get_card(self.pos, body.pos)
            self.center_of_mass = (self.center_of_mass*self.total_mass + body.pos*body.mass) / (self.total_mass + body.mass)
            self.total_mass += body.mass
            if self.children[card] == None:
                self.children[card] = body
            elif type(self.children[card]) == Body:
                child_pos = QuadTree.getChildPos(card, self.pos, self.size)
                qt = QuadTree.QuadTreeNode(self, self.size/2, child_pos)
                qt.insert(self.children[card])
                qt.insert(body)
                self.children[card] = qt
            else:
                self.children[card].insert(body)
    
    
    @classmethod
    def fromList(cls,bodies):
        tree = QuadTree()
        for b in bodies:
            tree.insert(b)
        return tree
    
    def __init__(self):
        self.root = None
    
    
    def insert(self, body):
        if self.root is None:
            self.root = body
        elif type(self.root) == Body:
            b = self.root
            self.root = QuadTree.QuadTreeNode(size = 1., pos = Vector(0.5,0.5))
            self.root.insert(b)
            self.root.insert(body)
        else:
            self.root.insert(body)
    
    def __iter__(self):
        if self.root is None:
            return []
        else:
            self.iter_stack = []
            self.iter_stack.append(list(self.root.children.values()))
            return self
    
    def __next__(self):
        while len(self.iter_stack) > 0:
            curr_list = self.iter_stack.pop()
            while len(curr_list) > 0:
                curr = curr_list.pop(0)
                if curr is None:
                    continue
                if type(curr) == QuadTree:
                    self.iter_stack.append(curr_list)
                    curr_list = list(curr.children.values())
                    continue
                self.iter_stack.append(curr_list)
                return curr
        raise StopIteration
    
    def getForce(self, body, theta = 0.5):
        if type(self.root) == Body:
            return Vector()
        stack = [self.root]
        force = Vector()
        while len(stack) > 0:
            curr = stack.pop()
            if curr.size/Vector.Distance(curr.center_of_mass, body.pos) < theta:
                force += QuadTree.calcForce(curr.center_of_mass, body.pos, curr.total_mass, body.mass)
            else:
                for c in curr.children.values():
                    if c is None or c is body:
                        continue
                    elif type(c) == Body:
                        force += QuadTree.calcForce(c.pos, body.pos, c.mass, body.mass)
                    else:
                        stack.append(c)
        return force
    
    @staticmethod
    def calcForce(pos1, pos2, mass1, mass2):
        f = (pos1 - pos2) / Vector.Distance(pos1, pos2)**3
        f = f*mass1*mass2 / 1000000
        return f
    
    @staticmethod
    def getChildPos(card, pos, size):
        quarter_size = size/4
        if card == Card.NORTHWEST:
            return Vector(pos.x - quarter_size, pos.y + quarter_size)
        elif card == Card.NORTHEAST:
            return Vector(pos.x + quarter_size, pos.y + quarter_size)
        elif card == Card.SOUTHWEST:
            return Vector(pos.x - quarter_size, pos.y - quarter_size)
        else: #Card.SOUTHEAST
            return Vector(pos.x + quarter_size, pos.y - quarter_size)

