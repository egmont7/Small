#!/usr/bin/env python3
from kivy.app import App
from kivy.graphics import *
from kivy.core.image import Image
from kivy.uix.widget import Widget

from config import TEX_MAP, TEX_ATLAS_FILE


class Tile(Image):
    _instances = {}
    _atlas = None

    def __init__(self, name, *args, **kwargs):
        if name in TEX_MAP:
            row, col = TEX_MAP[name]
        else:
            row, col = 9, 9
        #img = self._atlas.texture.get_region(20*row+1, 20*(row+1),
                                             #20*col+1, 20*(col+1))
        #super().__init__(img, *args, **kwargs)
        super().__init__(self._atlas, *args, **kwargs)

    @classmethod
    def from_name(cls, name):
        if not cls._atlas:
            cls._atlas = Image(TEX_ATLAS_FILE)
        if name not in cls._instances:
            tile = Tile(name)
            cls._instances[name] = tile
        return cls._instances[name]


class Level():
    def __init__(self, name, width=10, height=10):
        self.name = name
        tiles = {}
        for i, j in product(range(height),range(width)):
            tiles[(i,j)] = "grass"


class Sqatch(Widget):
    def before(self, *args, **kwargs):
        super.before(self,*args, **kwargs)
        with self.canvas:
            Color(0.5,0.5,0.)
            Rectangle(pos=(100,100), size=(500, 500))
            #Image("../assets/textures/atlas.png")



class SqatchApp(App):
    def build(self):
        return Sqatch()


if __name__ == '__main__':
    SqatchApp().run()
