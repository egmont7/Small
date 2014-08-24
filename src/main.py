'''
Created on Aug 23, 2014

@author: Caleb Fangmeier
'''
import pygame as pg

from utils import Vector, QuadTree, Body
from itertools import product
import sys



SCREEN_WIDTH, SCREEN_HEIGHT = 800, 800
BG_COLOR = 150, 150, 80
def main():
    
    pg.init()
    screen = pg.display.set_mode(
                (SCREEN_WIDTH, SCREEN_HEIGHT), 0, 32)
    clock = pg.time.Clock()
    
    bodies = []
    #for x,y in product([0.2,0.4,0.6,0.8],[0.2,0.4,0.6,0.8]):
    #    bodies.append(Body(mass=1, pos=Vector(x,y), vel=Vector()))
    
    bodies.append(Body(mass=10, pos = Vector(0.25,0.5), vel = Vector(0,-.003)))
    bodies.append(Body(mass=10, pos = Vector(0.75,0.5), vel = Vector(0, .003)))
    
    while True:
        #print("loop begin: {} objects".format(len(bodies)))
        time_passed = clock.tick(50)
        
        for event in pg.event.get():
            if event.type == pg.QUIT:
                sys.exit()
        
        # Redraw the background
        screen.fill(BG_COLOR)
        
        
        tree = QuadTree.fromList(bodies)
        for body in bodies: 
            body.force = tree.getForce(body)
            #print(body.force)
            #print(body.vel,'\n')
            body.update(time_passed)
        bodies = [b for b in bodies if  0 < b.pos.x < 1 and 0 < b.pos.y < 1]
        
        for body in bodies:
            x = int(body.pos.x*SCREEN_WIDTH)
            y = int(body.pos.y*SCREEN_HEIGHT)
            pg.draw.circle(screen, (255,255,0), (x, y), 20 ,10)

        pg.display.flip()
    

if __name__ == "__main__":
    main()



