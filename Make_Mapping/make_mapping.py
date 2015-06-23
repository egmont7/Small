#!/usr/bin/env python3

import sys
import os
import argparse
from collections import namedtuple


class Passthrough(dict):
    def __getitem__(self, key):
        return key

    def __setitem__(self, key, value):
        pass

Mapping = namedtuple("Mapping", ("from_", "to", "map_"))
ARGS = None


def parse_file():
    mappings = {}
    with open(ARGS.input, 'r') as infile:
        curr_map = None
        for line in infile.readlines():
            line = line.split('#')[0].strip()
            if not line:
                continue
            if line[:8].lower() == "mapping:":
                if curr_map is not None:
                    mappings[(curr_map.from_,curr_map.to)] = curr_map
                from_, to = line.split()[1:]
                curr_map = Mapping(from_=from_, to=to, map_={})
            elif line[:6].lower() == "alias:":
                from_, to = line.split()[1:]
                mappings[(from_,to)] = Mapping(from_=from_, to=to,
                                               map_=Passthrough())
            else:
                from_, to = line.split(',')
                curr_map.map_[from_.strip()] = to.strip()
    return mappings


def disp_maps(mappings):
    from graphviz import Digraph
    import subprocess

    dot = Digraph("Mappngs!")
    nodes = set.union(*(set(mapping)
                        for mapping in mappings.keys()))
    print(nodes)
    for node in nodes:
        dot.node(node, node)
    for mapping in mappings.values():
        dot.edge(mapping.from_, mapping.to)

    dot.render('graph_visual', cleanup=True)
    subprocess.Popen(["evince graph_visual.pdf"],shell=True)


def main():
    mappings = parse_file()
    # disp_maps(mappings)

    ports = []
    for from_, to in zip(ARGS.maps[:-1], ARGS.maps[1:]):
        map_ = mappings[(from_, to)].map_
        if not ports:
            for port in map_.keys():
                ports.append([port])
        for port in ports:
            port.append(map_[port[-1]])
        
    for port in ports:
        if ARGS.short:
            print(' '.join([port[0],port[-1]]))
        else:
            print(' => '.join(port))

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input",
                        help="input mapping file")
    parser.add_argument("-s", "--short", action="store_true",
                        help="short output, only in/out ports")
    parser.add_argument("maps", nargs="+",
                        help="mapping path")
    ARGS = parser.parse_args()
    main()
