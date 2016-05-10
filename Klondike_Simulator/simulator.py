#!/usr/bin/env python3
import itertools
import random
import copy
from datetime import datetime



class Suite:
    hearts = '♡'
    diamonds = '♢'
    clubs = '♧'
    spades = '♤'
    void = '~'

    @classmethod
    def suites(cls):
        return [cls.hearts, cls.diamonds, cls.clubs, cls.spades]

    @classmethod
    def suite_idx(cls, suite):
        if suite == cls.hearts:
            return 0
        elif suite == cls.diamonds:
            return 1
        elif suite == cls.clubs:
            return 2
        else:  # suite == cls.spades
            return 3

    @classmethod
    def color(cls, card):
        if card.suite in (cls.hearts, cls.diamonds):
            return 'r'
        else:
            return 'k'


class Face:
    up = '↑'
    down = '↓'


class Card:
    name_map = {0: ' A', 1: ' 2', 2: ' 3', 3: ' 4', 4: ' 5', 5: ' 6',
                6: ' 7', 7: ' 8', 8: ' 9', 9: '10', 10: ' J', 11: ' Q',
                12: ' K'}

    def __init__(self, suite, value, face):
        self.suite = suite
        self.value = value
        self.face = face

    def __copy__(self):
        return Card(self.suite, self.value, self.face)

    @staticmethod
    def column_compatable(cardHigh, cardLow):
        if cardLow.suite is Suite.void:
            return False
        elif cardHigh.suite is Suite.void and cardLow.value == 12:
            return True
        else:
            colorCheck = Suite.color(cardHigh) != Suite.color(cardLow)
            numberCheck = (cardHigh.value - cardLow.value) == 1
            return colorCheck and numberCheck

    @staticmethod
    def foundation_compatable(cardHigh, cardLow):
        if cardLow.suite is Suite.void:
            return False
        elif cardHigh.suite is Suite.void and cardLow.value == 0:
            return True
        else:
            suiteCheck = cardHigh.suite == cardLow.suite
            numberCheck = (cardLow.value - cardHigh.value) == 1
            return suiteCheck and numberCheck

    def __str__(self):
        if self.suite == Suite.void:
            return "~~~"
        elif self.face == Face.up:
            return "{}{}".format(self.name_map[self.value], self.suite)
        else:
            return "XXX"

    def __repr__(self):
        return str(self)

    def __eq__(self, card):
        return self.suite == card.suite and self.value == card.value

    def __ne__(self, card):
        return not (self == card)


class Deck:
    def __init__(self):
        self.cards = [Card(suite, value, Face.down) for suite, value in
                      itertools.product(Suite.suites(), range(13))]
        random.shuffle(self.cards)

    def pull_card(self):
        return self.cards.pop()

    def pull_cards(self, n):
        while n > 0:
            try:
                yield self.pull_card()
                n -= 1
            except IndexError:
                break


class Board():

    def __init__(self, weights=None, populate=False):
        self.found = ([], [], [], [])
        self.cols = ([], [], [], [], [], [], [])
        self.deck = ([], [])
        self.deck_turns_remaining = 3
        self.history = []
        if populate:
            deck = Deck()
            for i, col in enumerate(self.cols):
                cards = list(deck.pull_cards(i+1))
                col.append(Card(Suite.void, 0, Face.up))
                col.extend(cards)
                col[-1].face = Face.up
            self.deck[0].extend(deck.pull_cards(24))
            for card in self.deck[0]:
                card.face = Face.up
            self.deck[1].append(self.deck[0].pop())
            for fnd in self.found:
                fnd.append(Card(Suite.void, 0, Face.up))
        if weights is not None:
            norm = sum(abs(weight) for weight in weights.values())
            self.weight_foundation = weights['foundation']/norm
            self.weight_deck = weights['deck']/norm
            self.weight_uncovered = weights['uncovered']/norm
            self.weight_history = weights['history']/norm
            self.weight_king_in_place = weights['king_in_place']/norm

    def __deepcopy__(self, memodict={}):
        def list_copy(l):
            return [copy.copy(i) for i in l]

        def tuple_of_list_copy(ll):
            return tuple(list_copy(l) for l in ll)
        b = Board()
        b.found = tuple_of_list_copy(self.found)
        b.cols = tuple_of_list_copy(self.cols)
        b.deck = tuple_of_list_copy(self.deck)
        b.deck_turns_remaining = self.deck_turns_remaining

        b.history = self.history[:]
        b.history.append(hash(self))

        b.weight_foundation = self.weight_foundation
        b.weight_deck = self.weight_deck
        b.weight_uncovered = self.weight_uncovered
        b.weight_history = self.weight_history
        b.weight_king_in_place = self.weight_king_in_place

        return b

    def __str__(self):
        sep = '\n'+'*'*39+'\n'
        return sep.join([self.cols_to_str(),
                         self.found_to_str(),
                         self.deck_to_str(),
                         self.turns_to_str()])

    def __eq__(self, board):
        return hash(self) == hash(board)

    def __hash__(self):
        return hash(str(self))

    def merit(self):
        foundation_score = sum(len(l)-1 for l in self.found)
        deck_score = sum(len(l)-1 for l in self.deck)*self.deck_turns_remaining + len(self.deck[0])
        uncovered_score = sum(len([c for c in l if c.face == Face.down])
                              for l in self.cols)
        history_score = len(self.history)
        king_in_place_score = len([col for col in self.cols
                                   if len(col) > 1 and
                                   col[1].value == 12 and
                                   col[1].face == Face.up])
        return self.weight_foundation*foundation_score + \
            self.weight_deck*deck_score + \
            self.weight_uncovered*uncovered_score + \
            self.weight_history*history_score + \
            self.weight_king_in_place*king_in_place_score

    def is_winning(self):
        return sum(len(l)-1 for l in self.found) == 52

    def valid_moves_col_col(self):
        """
        Returns the list of new boards resulting from moving a stack of
        cards form one column to another.
        """
        for f, col_from in enumerate(self.cols):
            for cf, card_from in enumerate(col_from):
                # Cannot move face-down cards
                if card_from.face == Face.down:
                    continue
                # Don't move top level Kings
                if cf == 1 and card_from.value == 12:
                    continue
                for t, col_to in enumerate(self.cols):
                    card_to = col_to[-1]
                    if Card.column_compatable(card_to, card_from):
                        d = copy.deepcopy(self)
                        tmp = []
                        for _ in range(len(col_from)-cf):
                            tmp.append(d.cols[f].pop())
                        for _ in range(len(col_from)-cf):
                            d.cols[t].append(tmp.pop())
                        d.cols[f][-1].face = Face.up
                        yield d

    def valid_moves_col_found(self):
        """
        Returns the list of new boards resulting from moving a single
        card from the columns to the foundation.
        """
        for f, col_from in enumerate(self.cols):
            card_from = col_from[-1]
            t = Suite.suite_idx(card_from.suite)
            fnd_to = self.found[t]
            card_to = fnd_to[-1]
            if Card.foundation_compatable(card_to, card_from):
                d = copy.deepcopy(self)
                card = d.cols[f].pop()
                d.found[t].append(card)
                d.cols[f][-1].face = Face.up
                yield d

    def valid_moves_found_col(self):
        """
        Returns the list of new boards resulting from moving a single
        card from the foundation to a column.
        """
        for t, col_to in enumerate(self.cols):
            card_to = col_to[-1]
            for f, fnd_from in enumerate(self.found):
                card_from = fnd_from[-1]
                if Card.column_compatable(card_to, card_from):
                    d = copy.deepcopy(self)
                    card = d.found[f].pop()
                    d.cols[t].append(card)
                    yield d

    def valid_moves_deck_col(self):
        """
        Returns the list of new boards resulting from moving a single
        card from the deck to a column.
        """
        if len(self.deck[1]) > 0:
            card_from = self.deck[1][-1]
        else:
            raise StopIteration

        for t, col_to in enumerate(self.cols):
            card_to = col_to[-1]
            if Card.column_compatable(card_to, card_from):
                d = copy.deepcopy(self)
                card = d.deck[1].pop()
                d.cols[t].append(card)
                yield d

    def valid_moves_deck_found(self):
        """
        Returns the list of new boards resulting from moving a single
        card from the deck to the foundation.
        """
        if len(self.deck[1]) > 0:
            card_from = self.deck[1][-1]
        else:
            raise StopIteration

        for t, fnd_to in enumerate(self.found):
            card_to = fnd_to[-1]
            if Card.foundation_compatable(card_to, card_from):
                d = copy.deepcopy(self)
                card = d.deck[1].pop()
                d.found[t].append(card)
                yield d

    def valid_moves_flip_deck(self):
        """
        Returns a new board resulting from flipping a card in the deck.
        """
        d = copy.deepcopy(self)
        if len(d.deck[0]) > 0:
            card = d.deck[0].pop()
            d.deck[1].append(card)
            yield d
        elif d.deck_turns_remaining > 0:
            for _ in range(len(d.deck[1])):
                card = d.deck[1].pop()
                d.deck[0].append(card)
            d.deck_turns_remaining -= 1
            yield d
        else:
            raise StopIteration

    def valid_moves(self):
        # Returns a list of new boards that can be arrived at
        # via a single move from the current board
        # The categories of possible moves are:
        #  1. moves within cols
        #  2. moves cols -> found
        #  3. moves found -> cols
        #  4. moves deck -> cols
        #  5. moves deck -> found
        #  6. flip deck(possibly a full flip)
        return list(itertools.chain(self.valid_moves_col_col(),
                                    self.valid_moves_col_found(),
                                    self.valid_moves_found_col(),
                                    self.valid_moves_deck_col(),
                                    self.valid_moves_deck_found(),
                                    self.valid_moves_flip_deck()))

    def cols_to_str(self):
        # rev_cols = [col[::-1] for col in self.cols]
        z = itertools.zip_longest(*self.cols, fillvalue='   ')
        s_rows = []
        for row in z:
            s_rows.append(" | ".join(map(str, row)))
        return '\n'.join(s_rows)

    def found_to_str(self):
        # rev_found = [col[::-1] for col in self.found]
        z = itertools.zip_longest(*self.found, fillvalue='   ')
        s_rows = []
        for row in z:
            s_rows.append(" | ".join(map(str, row)))
        return '\n'.join(s_rows)

    def deck_to_str(self):
        def to_string(cards):
            return ','.join(map(str, reversed(cards)))
        s = map(to_string, self.deck)
        return '\n'.join(list(s))

    def turns_to_str(self):
        return "Turns remaining: {}".format(self.deck_turns_remaining)


def solve_lookahead(weights, foresight=1):
    b_start = Board(weights, populate=True)
    prev = {hash(b_start): str(b_start)}

    def lookahead(board, foresight):
        if board.is_winning():
            return (1000, [board])
        elif foresight == 0:
            return (board.merit(), [board])
        else:
            moves = [move for move in board.valid_moves()
                     if hash(move) not in prev]
            paths = []
            for move in moves:
                score, path = lookahead(move, foresight-1)
                if path is not None:
                    path.insert(0, board)
                    paths.append((score, path))
            if len(paths) == 0:
                return (0, None)
            best_path = max(paths, key=lambda x: x[0])
            return best_path

    b = b_start
    while True:
        score, chain = lookahead(b, foresight)
        if score == 1000:
            return 1
        elif chain is None:
            return 0
        else:
            print(b)
            print(chain[1])
            print('*'*80)
            print('*'*80)
            # input()
            b = chain[1]
            prev[hash(b)] = str(b)

weights = {"foundation": 20,
           "deck": -5,
           "uncovered": -6,
           "history": -5,
           "king_in_place": 10}
N = 10
results = []
random.seed(datetime.now().microsecond)
for i in range(10):
    result = solve_lookahead(weights, foresight=3)
    print("{:02d}:{}".format(i, bool(result)))
    results.append(result)
print("{}/{}".format(sum(results), N))
