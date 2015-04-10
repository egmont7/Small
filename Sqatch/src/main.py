#!/usr/bin/env python3
from kivy.app import App
from kivy.atlas import Atlas
from kivy.clock import Clock
from kivy.core.image import Image as CoreImage
from kivy.uix.widget import Widget
from kivy.core.window import Window #must import or shader fails to build
from kivy.properties import (ListProperty, NumericProperty)
from kivy.graphics import (RenderContext, PushMatrix, PopMatrix,
                           UpdateNormalMatrix, Mesh)
from kivy.graphics.transformation import Matrix

from itertools import product
from math import cos, sin


class TileMap(Widget):
    scale = NumericProperty(1.)
    center = ListProperty([.5, .5])

    def __init__(self, **kwargs):
        self.canvas = RenderContext()
        self.canvas.shader.source = "assets/shaders/simple.glsl"
        self.atlas = Atlas("assets/textures/bg_atlas.atlas")
        self.tex = CoreImage("assets/textures/bg_atlas-0.png").texture
        self.t = 0

        super().__init__(**kwargs)
        with self.canvas:
            PushMatrix()
            self.setup_scene()
            PopMatrix()
        Clock.schedule_interval(self.update_glsl, 1/60)

    def update_glsl(self, *args):
        self.t += 1/60
        d = self.t
        proj = Matrix().view_clip(-1, 2, -1, 2, 0, 1, 0)

        self.canvas['projection_mat'] = proj
        self.canvas['offset'] = [.05*sin(d*cos(d)**2), .05*cos(d*cos(d)**2), 0]

    def get_mesh(self):
        vertices = []
        indices = []
        rows = 10
        cols = 10
        blk_h = 1 / rows
        blk_w = 1 / cols
        n = 0
        tex_names = list(self.atlas.textures.keys())
        for i, j in product(range(cols), range(rows)):
            tex_name = tex_names[(i+j) % len(tex_names)]
            win_x1, win_y1 = blk_w*i,     blk_h*j
            win_x2, win_y2 = blk_w*(i+1), blk_h*(j+1)
            tex_pos = self.atlas[tex_name].uvpos
            tex_size = self.atlas[tex_name].uvsize
            tex_x1, tex_y1 = tex_pos
            tex_x2, tex_y2 = tex_pos[0]+tex_size[0], tex_pos[1]+tex_size[1]
            vertices.extend([win_x1, win_y1, 1, tex_x1, tex_y1,
                             win_x2, win_y1, 1, tex_x2, tex_y1,
                             win_x1, win_y2, 1, tex_x1, tex_y2,
                             win_x2, win_y2, 1, tex_x2, tex_y2])

            indices.extend([n, n+1, n+2, n+2, n+1, n+3])
            n += 4

        fmt = [(b'v_pos',   3, 'float'),
               (b'v_uv',    2, 'float')]
        return Mesh(texture=self.tex, vertices=vertices,
                    indices=indices, fmt=fmt, mode="triangles")

    def setup_scene(self, *args):
        PushMatrix()
        UpdateNormalMatrix()
        self.mesh = self.get_mesh()
        PopMatrix()


class SqatchApp(App):
    def build(self):
        return TileMap()


if __name__ == "__main__":
    SqatchApp().run()
