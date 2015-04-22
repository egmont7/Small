#!/usr/bin/env python3
from kivy.app import App
from kivy.atlas import Atlas
from kivy.clock import Clock
from kivy.core.image import Image as CoreImage
from kivy.uix.widget import Widget
from kivy.core.window import Window # Must import or shader fails to build
from kivy.properties import (ListProperty, NumericProperty, ObjectProperty)
from kivy.graphics import (RenderContext, PushMatrix, PopMatrix,
                           UpdateNormalMatrix, Mesh, Rectangle, Color)
from kivy.graphics.transformation import Matrix

from config import BG_ATLAS_ATLAS, BG_ATLAS_IMAGE, SIMPLE_SHADER
from itertools import product



class Camera():
    position = ListProperty([0,0,1])
    up_direction = ListProperty([0,1,0])
    look_direction = ListProperty([0,0,-1])

    def __init__(self):
        pass

class Level(Widget):

    def __init__(self, grid, **kwargs):
        self.canvas = RenderContext()
        self.canvas.shader.source = SIMPLE_SHADER
        self._atlas = Atlas(BG_ATLAS_ATLAS)
        self._tex = CoreImage(BG_ATLAS_IMAGE).texture

        super().__init__(size_hint=(1,1),**kwargs)
        with self.canvas:
            PushMatrix()
            UpdateNormalMatrix()

            vertices = []
            indices = []
            rows = 40
            cols = 40
            blk_h = 1. / rows
            blk_w = 1. / cols
            n = 0
            tex_names = list(self._atlas.textures.keys())
            for i, j in product(range(cols), range(rows)):
                tex_name = tex_names[(i+j) % len(tex_names)]
                win_x1, win_y1 = blk_w*i-.5,     blk_h*j-.5
                win_x2, win_y2 = blk_w*(i+1)-.5, blk_h*(j+1)-.5
                tex_pos = self._atlas[tex_name].uvpos
                tex_size = self._atlas[tex_name].uvsize
                tex_x1, tex_y1 = tex_pos
                tex_x2, tex_y2 = tex_pos[0]+tex_size[0], tex_pos[1]+tex_size[1]
                vertices.extend([win_x1, win_y1, 0, tex_x1, tex_y1,
                                 win_x2, win_y1, 0, tex_x2, tex_y1,
                                 win_x1, win_y2, 0, tex_x1, tex_y2,
                                 win_x2, win_y2, 0, tex_x2, tex_y2])

                indices.extend([n, n+1, n+2, n+2, n+1, n+3])
                n += 4

            fmt = [(b'v_pos',   3, 'float'),
                   (b'v_uv',    2, 'float')]
            self.mesh = Mesh(texture=self._tex, vertices=vertices,
                             indices=indices, fmt=fmt, mode="triangles")
            PopMatrix()

    def update(self, model_view):
        x1 = view_center[0] - view_scale/2
        x2 = view_center[0] + view_scale/2
        y1 = view_center[1] - view_scale/2
        y2 = view_center[1] + view_scale/2
        proj = Matrix().view_clip(x1, x2, y1, y2, -1, 2, 0)

        print(proj)
        print(self.canvas['projection_mat'])
        print(self.canvas)
        print()
        # self.canvas['projection_mat'] = proj

    @classmethod
    def from_file(cls, filename):
        grid = None  # TODO: Read from file
        l = Level(grid)
        return l

class ViewPort(Widget):
    view_scale = NumericProperty(1.)
    view_center = ListProperty([.5, .5])
    level = ObjectProperty(None)
    camera = ObjectProperty(Camera())

    def __init__(self, **kwargs):

        self._keyboard = Window.request_keyboard(self._keyboard_closed, self)
        self._keyboard.bind(on_key_down=self._on_keyboard_down)

        super().__init__(**kwargs)
        self.level = Level.from_file('')
        self.add_widget(self.level)
        # self.add_widget(Button(text="hello world!"))
        self.bind(view_center=self.update)
        self.bind(view_scale=self.update)

    def _keyboard_closed(self):
        self._keyboard.unbind(on_key_down=self._on_keyboard_down)
        self._keyboard = None

    def _on_keyboard_down(self, keyboard, keycode, text, modifiers):
        print(keyboard, keycode, text, modifiers)
        if keycode[1] == 'w':
            self.view_center[1] += .10
        elif keycode[1] == 's':
            self.view_center[1] -= .10
        elif keycode[1] == 'a':
            self.view_center[0] += .10
        elif keycode[1] == 'd':
            self.view_center[0] -= .10
        elif keycode[1] == '[':
            self.view_scale += .10
        elif keycode[1] == ']':
            self.view_scale -= .10
        elif keycode[1] == 'escape':
            Window.close()
        return True

    def update(self, *args):
        self.level.update(view_center=self.view_center, view_scale=self.view_scale)
        # print(self.level.size)


class SqatchApp(App):
    def build(self):
        v = ViewPort()
        Clock.schedule_interval(v.update, 1/60)
        return v


if __name__ == "__main__":
    SqatchApp().run()
