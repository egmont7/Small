#!/usr/bin/env python3
"""
    FLASHY - A Terminal Flash Card Program

      **Copyright (c) 2015 Caleb Fangmeier (caleb.fangmeier@gmail.com)

      **Permission is hereby granted, free of charge, to any person obtaining a
      **copy of this software and associated documentation files (the "Software"),
      **to deal in the Software without restriction, including without limitation
      **the rights to use, copy, modify, merge, publish, distribute, sublicense,
      **and/or sell copies of the Software, and to permit persons to whom the
      **Software is furnished to do so, subject to the following conditions:

      **The above copyright notice and this permission notice shall be included
      **in all copies or substantial portions of the Software.

      **THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
      **OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      **FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
      **THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
      **OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
      **ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
      **OTHER DEALINGS IN THE SOFTWARE.
"""
import sys
import curses
import argparse
from string import ascii_letters, punctuation
from random import shuffle
from collections import deque

SCREEN = None
ARGS = None

DIGRAPHS = {('a',':'):'ä',
            ('o',':'):'ö',
            ('u',':'):'ü',
            ('s','s'):'ß'}
USER_CHARS = ascii_letters+punctuation+' '

def line_from_center(d = 0):
    return (curses.LINES//2)+d

class Deck:
    """
    A Deck of flash cards.
    """
    def __init__(self, title):
        self.title = title
        self.cards = []
    def __repr__(self):
        rep = ["\t{}".format(card) for card in self.cards]
        return "\n".join(["==> {}".format(self.title)]+rep)

class Card:
    """
    A flash card.
    """
    def __init__(self, sideA, sideB):
        self.sideA = sideA
        self.sideB = sideB
    def __repr__(self):
        return "{x.sideA} <==> {x.sideB}".format(x=self)

def parse_decks():
    with open(ARGS.flashyfile) as f:
        decks = []
        for line in f.readlines():
            line = line.split("#")[0].strip()
            if not line: continue
            if line[0] == "$":
                decks.append(Deck(line[1:]))
            else:
                sideA, sideB = (lang.strip() for lang in line.split("::"))
                decks[-1].cards.append(Card(sideA, sideB))
    return decks

def display_question(question):
    left_bound = (curses.COLS-len(question)-1)//2
    SCREEN.addstr(line_from_center(-1), left_bound, question)
def display_answer(answer):
    left_bound = (curses.COLS-len(answer)-1)//2
    SCREEN.addstr(line_from_center(0), left_bound, answer)
def display_opts(opts):
    left_bound = (curses.COLS-len(opts)-1)//2
    SCREEN.addstr(curses.LINES-2, left_bound, opts)
def display_stats(remain, total):
    s = "({}/{})".format(remain, total)
    left_bound = curses.COLS-len(s)-1
    SCREEN.addstr(0, left_bound, s)
def display_title(title):
    s = "Deck: {}".format(title)
    left_bound = (curses.COLS-len(s)-1)//2
    SCREEN.addstr(0, left_bound, s)
def display_guess(s):
    s = "=>"+s
    left_bound = (curses.COLS-1)//2 - 5
    row = line_from_center(2)
    SCREEN.move(row,0)
    SCREEN.clrtoeol()
    SCREEN.addstr(row, left_bound, s)
def display_correct(correct):
    s = "Correct!" if correct else "Not Correct"
    left_bound = (curses.COLS-len(s)-1)//2
    SCREEN.addstr(line_from_center(3), left_bound, s)


def select_deck(decks):
    SCREEN.clear()
    if len(decks) > 1:
        SCREEN.addstr(0, 0, "Please Select a Deck to Use")
        for i, deck in enumerate(decks):
            SCREEN.addstr(i+1, 0, "{:2d}: {}".format(i+1, deck.title))
        SCREEN.addstr(i+2, 0, " q: Quit")
        SCREEN.move(i+3, 0)
        curses.echo()
        curses.curs_set(2)
        while True:
            selection = SCREEN.getstr().strip().lower()
            if selection == b"q": sys.exit(0)
            try:
                selection = int(selection)
                deck = decks[selection-1]
                curses.noecho()
                curses.curs_set(0)
                return deck
            except Exception:
                continue
    else:
        return decks[0]


def get_guess(mimick = None):
    s = ""
    while True:
        display_guess(s)
        key = curses.keyname(SCREEN.getch()).decode()
        if key == '^?': #BACKSPACE
            s = s[:-1]
        elif key =='^J' and not mimick: # ENTER
            return s
        elif key =='^W': # CTRL-W
            return False
        ch = ''
        if key == '^K': # CTRL-K
            dig1 = SCREEN.getkey()
            dig2 = SCREEN.getkey()
            dig = (dig1, dig2)
            if dig in DIGRAPHS:
                ch = DIGRAPHS[dig]
        elif key in USER_CHARS:
            ch = key
        if mimick and not mimick.startswith(s+ch):
            continue
        else:
            s += ch
        if s == mimick:
            return s



def run_deck(deck):
    cards = deque(deck.cards)
    shuffle(cards)
    N = len(cards)
    while cards:
        card = cards.popleft()
        if ARGS.reverse:
            show1, show2 = card.sideA, card.sideB
        else:
            show1, show2 = card.sideB, card.sideA
        SCREEN.clear()
        display_stats(len(cards)+1, N)
        display_title(deck.title)
        display_question(show1)
        if ARGS.spelling:
            display_opts('ctrl-W <quit>')
            guess = get_guess()
            if guess is False: return
            display_answer(show2)
            if guess.strip() != show2:
                cards.append(card)
                display_correct(False)
                guess = get_guess(show2)
                if guess is False:
                    return
            else:
                display_correct(True)
                SCREEN.getkey()

        else:
            SCREEN.getkey()
            display_answer(show2)
            display_opts("r <replace>  d <discard>  q <quit>")

            while True:
                c = SCREEN.getkey().lower()
                if c == 'r':
                    cards.append(card)
                    break
                elif c == 'd':
                    break
                elif c == 'q':
                    return

def main(stdscr):
    global SCREEN
    SCREEN = stdscr

    SCREEN.immedok(True)
    curses.noecho()
    curses.curs_set(0)
    decks = parse_decks()

    while True:
        deck = select_deck(decks)
        run_deck(deck)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("flashyfile", help="The card file to use")
    parser.add_argument("-r", "--reverse",
                        help="Reverses which side is shown",
                        action="store_true")
    parser.add_argument("-s", "--spelling",
                        help="Enable spelling mode",
                        action="store_true")
    ARGS = parser.parse_args()

    curses.wrapper(main)
