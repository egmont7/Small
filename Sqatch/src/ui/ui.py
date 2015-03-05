#!/usr/bin/env python3
from kivy.app import App
from kivy.graphics import *
from kivy.uix.widget import Widget


class Level():
    def __init__(self, name, width=10, height=10):
        self.name = name
        tiles = {}
        for i,j in product(range(height),range(width)):
            tiles[(i,j)] = "grass"





class PongGame(Widget):
    def before(self, *args, **kwargs):
        super.before(self,*args, **kwargs)
        with self.canvas:
            Color(1.,0.,0.)
            Rectangle(pos=(100,100), size=(500,500))


class PongApp(App):
    def build(self):
        return PongGame()


if __name__ == '__main__':
    PongApp().run()
