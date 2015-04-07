#!/usr/bin/env python3
from kivy.app import App
from kivy.uix.widget import Widget
from kivy.graphics import Color, Rectangle


class SqatchApp(App):
    def build(self):
        root = Widget()
        with root.canvas.before:
            Color(1., 0., 1.)
            Rectangle(height=100)
        return root


if __name__ == "__main__":
    app = SqatchApp()
    app.run()
