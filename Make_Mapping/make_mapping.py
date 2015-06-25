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

    def __contains__(self, key):
        return True

Mapping = namedtuple("Mapping", ("from_", "to"))
ARGS = None


def parse_file():
    mappings = {}
    with open(ARGS.input, 'r') as infile:
        curr_from, curr_to, curr_map = None, None, None
        for line in infile.readlines():
            line = line.split('#')[0].strip()
            if not line:
                continue
            if line[:8].lower() == "mapping:":
                if curr_map:
                    mappings[Mapping(curr_from, curr_to)] = curr_map
                curr_from, curr_to = line.split()[1:]
                curr_map = {}
            elif line[:12].lower() == "passthrough:":
                pass_from, pass_to = line.split()[1:]
                mappings[Mapping(pass_from, pass_to)] = Passthrough()
            else:
                map_from, map_to = line.split(',')
                curr_map[map_from.strip()] = map_to.strip()
        if curr_map:
            mappings[Mapping(curr_from,curr_to)] = curr_map
    return mappings

def gen_rev_mappings(mappings):
    rev_mappings = {}
    for mapping, map_ in mappings.items():
        if type(map_) == Passthrough:
            rev_map = map_
        else:
            rev_map = {item:key for key,item in map_.items()}
        rev_mappings[Mapping(from_=mapping.to, to=mapping.from_)] = rev_map
    return rev_mappings


def disp_maps(mappings):
    from graphviz import Digraph
    import webbrowser
    import subprocess

    dot = Digraph("Mappings!")
    nodes = set.union(*(set(mapping)
                        for mapping in mappings.keys()))
    for node in nodes:
        dot.node(node, node)
    for mapping in mappings.keys():
        dot.edge(mapping.from_, mapping.to, _attributes={"dir":"both"})
    print(mappings.keys())

    dot.format = 'png'
    dot.render('graph_visual', cleanup=True)
    webbrowser.open("graph_visual.png")
    # subprocess.Popen(["evince graph_visual.png"],shell=True)


def main():
    mappings = parse_file()
    if ARGS.plot:
        disp_maps(mappings)
    mappings.update(gen_rev_mappings(mappings))

    if len(ARGS.maps) == 0:
        print("No mapping path given, exiting...")
        return
    ports = []
    for from_, to in zip(ARGS.maps[:-1], ARGS.maps[1:]):
        map_ = mappings[Mapping(from_, to)]
        if not ports:
            for port in map_.keys():
                if port.startswith("XNCX"): continue
                ports.append([port])
        for port in ports:
            # print(port[-1], map_.keys())
            if port[-1].startswith("XNCX") or port[-1] not in map_:
                port.append("XNCX")
            else:
                port.append(map_[port[-1]])
            # print(port,"\n")

    print("Mapping for:")
    print(" ".join(ARGS.maps))
    print()
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
    parser.add_argument("-p", "--plot", action="store_true",
                        help="shows graphviz plot of included mappings")
    parser.add_argument("maps", nargs="*", default=[],
                        help="mapping path")
    ARGS = parser.parse_args()
    main()
