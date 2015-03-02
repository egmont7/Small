'''
Created on Aug 23, 2014

@author: Caleb Fangmeier
'''
import pygame as pg

from levels import get_next_level
from physics import QuadTree, configure_physics, update_player_physics
from utils import Vector
import rdc
import sys

bodies = []
safe_zones = []
player = None

def load_level(level):
    global bodies, safe_zones, player
    configure_physics(level.physics)
    bodies = level.getBodies()
    safe_zones = level.getSafeZones()
    player = level.getPlayer()
    

def handle_input():
    keys = pg.key.get_pressed()
    player.clearMotion()
    if keys[pg.K_w]:
        player.addMotion(Vector(0,-1))
    if keys[pg.K_s]:
        player.addMotion(Vector(0,1))
    if keys[pg.K_a]:
        player.addMotion(Vector(-1,0))
    if keys[pg.K_d]:
        player.addMotion(Vector(1,0))
    if keys[pg.K_ESCAPE]:
        sys.exit()
    for event in pg.event.get():
        if event.type == pg.QUIT:
            sys.exit()

SCREEN_WIDTH, SCREEN_HEIGHT = 1000, 1000
BG_COLOR = 0, 0, 0

def to_pixels(pos, rad):
    posPix = int(pos.x*SCREEN_WIDTH),int(pos.y*SCREEN_HEIGHT)
    radPix = int(rad*SCREEN_WIDTH)
    return radPix, posPix

def main():
    global bodies
    pg.init()
    screen = pg.display.set_mode(
                (SCREEN_WIDTH, SCREEN_HEIGHT), 0, 32)
    clock = pg.time.Clock()
    myfont = pg.font.SysFont("monospace", 15)
    
    load_level(get_next_level())
    
    while True:
        clock.tick(50)
        handle_input()
        
        # Redraw the background
        screen.fill(BG_COLOR)
        player.inSafe = False
        collTree = rdc.RDC(bodies + safe_zones + [player])
        collTree.DoRDC()
        
        if player.inSafe:
            physTree = QuadTree(bodies)
        else:
            physTree = QuadTree(bodies + [player])
        physTree.doPhysics()
        
        update_player_physics(player)
        hits = player.updateScore()
        for h in hits: bodies.remove(h)

        for body in bodies:
            rad, pos = to_pixels(body.pos, body.rad)
            pg.draw.circle(screen, body.color, pos, rad ,2)
        
        for sz in safe_zones:
            rad, pos = to_pixels(sz.pos, sz.rad)
            pg.draw.circle(screen, (0,0,255), pos, rad , rad)
        
        rad, pos = to_pixels(player.pos, player.rad)
        pg.draw.circle(screen, (255,0,255), pos, rad)
        
        label = myfont.render("Score: {}".format(player.score), 1, (255,255,0))
        screen.blit(label,(50,50))
        
        pg.display.flip()

        if(all(b.value <= 0 for b in bodies)):
            load_level(get_next_level())
    

if __name__ == "__main__":
    main()



