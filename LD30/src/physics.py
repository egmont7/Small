'''
Created on Aug 24, 2014

@author: caleb
'''

from utils import Vector, Sprite, Body, SafeZone, Player, Card


G = 1

SPEED_LIMIT = 1
OVERSPEED_DAMP = 0.9
NORMAL_DAMP = 0.99
PLAYER_DAMP = 0.7
DT = 0.01
THETA = 8
RESTITUTION = 1

def configure_physics(phys_params):
    global G
    global SPEED_LIMIT
    global OVERSPEED_DAMP
    global NORMAL_DAMP
    global DT
    global THETA
    global RESTITUTION
    global PLAYER_DAMP

    G = phys_params['G']
    SPEED_LIMIT = phys_params['SPEED_LIMIT']
    OVERSPEED_DAMP = phys_params['OVERSPEED_DAMP']
    NORMAL_DAMP = phys_params['NORMAL_DAMP']
    DT  = phys_params['DT']
    THETA = phys_params['THETA']
    RESTITUTION = phys_params['RESTITUTION']
    PLAYER_DAMP = phys_params['PLAYER_DAMP']

def collide_body_body(b1, b2):
    delta = b1.pos - b2.pos;
    d = Vector.Length(delta)
    #Minimum translation distance to push balls apart after intersecting
    mtd = delta * ((b1.rad + b2.rad)-d)/d

    #Resolve intersection --
    #Inverse mass quantities
    im1 = 1 / b1.mass
    im2 = 1 / b2.mass

    #push-pull them apart based off their mass
    b1.pos = b1.pos + mtd * (im1 / (im1 + im2))
    b2.pos = b2.pos - mtd * (im2 / (im1 + im2))


    #Impact speed
    v = b1.vel - b2.vel
    vn = Vector.Dot(v, Vector.Normalize(mtd))

    #Sphere intersecting but moving away from each other already
    if vn > 0:
        return
    #Collision impulse
    i = (-(1 + RESTITUTION) * vn) / (im1 + im2)
    impulse = Vector.Normalize(mtd) * i

    #Change in momentum
    b1.vel = b1.vel + impulse*im1
    b2.vel = b2.vel - impulse*im2

def collide_body_static_body(b, sb):
    delta = b.pos - sb.pos;
    d = Vector.Length(delta)
    #Minimum translation distance to push balls apart after intersecting
    mtd = delta * ((b.rad + sb.rad)-d)/d

    #Resolve intersection --
    #push-pull them apart based off their mass
    b.pos = b.pos + mtd

    #Impact speed
    vn = Vector.Dot(b.vel, Vector.Normalize(mtd))

    #Sphere intersecting but moving away from each other already
    if vn > 0:
        return
    #Change in Velocity
    i = -(1 + RESTITUTION) * vn
    b.vel += Vector.Normalize(mtd) * i

def collide_body_player(b,p):
    p.touching.append(b)

def collide(a, b):
    ta = type(a)
    tb = type(b)
    if ta == Body and tb == Body:
        collide_body_body(a,b)
    elif ta == Body and tb == SafeZone:
        collide_body_static_body(a, b)
    elif ta == SafeZone and tb == Body:
        collide_body_static_body(b, a)
    elif ta == Body and tb == Player:
        collide_body_player(a, b)
    elif ta == Player and tb == Body:
        collide_body_player(b, a)
    elif ta == Player and tb == SafeZone:
        a.inSafe = True
    elif ta == SafeZone and tb == Player:
        b.inSafe = True
    else:
        print("AAAAH I CANT COLLIDE THIS!!!!{} {}".format(ta,tb))


def update_player_physics(player):
    player.pos += player.vel * DT
    player.vel += player.dir * player.power * DT
    player.vel *= PLAYER_DAMP

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
            elif isinstance(self.children[card], Sprite):
                child_pos = QuadTree.getChildPos(card, self.pos, self.size)
                qt = QuadTree.QuadTreeNode(self, self.size/2, child_pos)
                if self.children[card].pos == body.pos:
                    body.pos.x += 0.001
                qt.insert(self.children[card])
                qt.insert(body)
                self.children[card] = qt
            else:
                self.children[card].insert(body)

    def __init__(self, entities):
        self.root = None
        self.entities = entities
        for e in entities:
            self.insert(e)


    def insert(self, entity):
        if self.root is None:
            self.root = entity
        elif isinstance(self.root, Sprite):
            e = self.root
            self.root = QuadTree.QuadTreeNode(size = 1., pos = Vector(0.5,0.5))
            self.root.insert(e)
            self.root.insert(entity)
        else:
            self.root.insert(entity)

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

    def doPhysics(self):
        for e in self.entities:
            self.physicsStep(e)

    def physicsStep(self, entity):
        force = Vector()
        if not isinstance(self.root,Sprite):
            stack = [self.root]
            while len(stack) > 0:
                curr = stack.pop()
                if curr.size/Vector.Distance(curr.center_of_mass, entity.pos) < THETA:
                    force += QuadTree.calcForce(curr.center_of_mass, entity.pos, curr.total_mass, entity.mass)
                else:
                    for c in curr.children.values():
                        if c is None or c is entity:
                            continue
                        elif isinstance(c,Sprite):
                            force += QuadTree.calcForce(c.pos, entity.pos, c.mass, entity.mass)
                        else:
                            stack.append(c)
        entity.pos = entity.pos + entity.vel * DT
        entity.vel = entity.vel + (force / entity.mass) * DT
        if Vector.Length(entity.vel) > SPEED_LIMIT:
            entity.vel *= OVERSPEED_DAMP
        else:
            entity.vel *= NORMAL_DAMP

    @staticmethod
    def calcForce(pos1, pos2, mass1, mass2):
        f = (pos1 - pos2) / Vector.Distance(pos1, pos2)**3
        f = f*mass1*mass2 * G
        return f

    @staticmethod
    def getChildPos(card, pos, size):
        quarter_size = size/4
        if card == Card.NORTHWEST:
            return Vector(pos.x - quarter_size, pos.y - quarter_size)
        elif card == Card.NORTHEAST:
            return Vector(pos.x + quarter_size, pos.y - quarter_size)
        elif card == Card.SOUTHWEST:
            return Vector(pos.x - quarter_size, pos.y + quarter_size)
        else: #Card.SOUTHEAST
            return Vector(pos.x + quarter_size, pos.y + quarter_size)

