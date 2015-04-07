#!/usr/bin/env python3
import os
BASEDIR = os.path.abspath(os.path.dirname(__file__))

TEX_MAP = {"grass": (0, 0),
           "stone": (0, 1)}
TEX_ATLAS_FILE = os.path.join(BASEDIR, "../assets/textures/atlas.png")
