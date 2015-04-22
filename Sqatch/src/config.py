#!/usr/bin/env python3
"""
Module Docs!
"""
import os
from os.path import join
BASEDIR = os.path.abspath(os.path.dirname(__file__))
TEXDIR = os.path.join(BASEDIR, "../assets/textures/")
SHADERDIR = os.path.join(BASEDIR, "../assets/shaders/")


BG_ATLAS_ATLAS = join(TEXDIR, "bg_atlas.atlas")
BG_ATLAS_IMAGE = join(TEXDIR, "bg_atlas-0.png")
FG_ATLAS_ATLAS = join(TEXDIR, "fg_atlas.atlas")
FG_ATLAS_IMAGE = join(TEXDIR, "fg_atlas-0.png")


SIMPLE_SHADER = join(SHADERDIR, "simple.glsl")
