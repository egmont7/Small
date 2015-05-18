#!/usr/bin/python3

import sys
import os
import time
import logging
import random
from collections import deque
import matplotlib
matplotlib.use("Qt5Agg")
import seaborn
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

import PyQt5
from PyQt5.QtWidgets import (QMainWindow, QFrame, QDesktopWidget,
                             QApplication, QWidget, QPushButton,
                             QVBoxLayout, QHBoxLayout, QGridLayout,
                             QLabel, QListWidget, QListWidgetItem, QSplitter)
from PyQt5.QtCore import Qt, QBasicTimer, pyqtSignal


VIRTUAL_HARDWARE = True
SAMPLE_PERIOD_SECONDS = .5 #seconds
SAMPLE_HISTORY_SIZE = 50
TEMP_ERROR_THRESHOLD = 10
random.seed("DEADBEEF")
logging.basicConfig(filename="OvenControlV2.log", filemode='w',
                    format='%(levelname)s::%(asctime)s %(message)s',
                    level=logging.INFO)


class DynamicPlot(FigureCanvas):
    """Ultimately, this is a QWidget (as well as a FigureCanvasAgg, etc.)."""
    def __init__(self, parent=None, width=5, height=4, dpi=100,
                 n_plots=1, n_data = 10, labels=None,
                 x_range=(0,10),y_range=(0,50),
                 title="", xlabel="", ylabel=""):
        self.fig = Figure()
        self.axes = self.fig.add_subplot(111)
        self.title = title
        self.xlabel = xlabel
        self.ylabel = ylabel
        self.x_range = x_range
        self.y_range = y_range
        self.n_plots = n_plots

        FigureCanvas.__init__(self, self.fig)
        self.setParent(parent)

        FigureCanvas.updateGeometry(self)
        self.lines = []
        xs_strt = [x_range[0]+(x_range[1]-x_range[0])*i/n_data for i in range(n_data)]
        ys_strt = [(y_range[0]+y_range[1])/2]*n_data
        for i in range(n_plots):
            line, = self.axes.plot(xs_strt, ys_strt, label=labels[i])
            self.lines.append(line)
        self.axes.set_xlim(x_range)
        self.axes.set_ylim(y_range)
        self.axes.set_title(self.title)
        self.axes.set_xlabel(self.xlabel)
        self.axes.set_ylabel(self.ylabel)
        self.axes.legend()
        self.fig.canvas.draw()

    def set_lims(self, xlim, ylim):
        self.axes.set_xlim(xlim)
        self.axes.set_ylim(ylim)
        self.fig.canvas.draw()

    def update_figure(self, xs, ys):
        for i, (x, y) in enumerate(zip(xs, ys)):
            self.lines[i].set_data(x, y)
        self.figure.canvas.draw()


class OvenController(QMainWindow):

    def __init__(self):
        super().__init__()
        self.gui_init()
        self.temperature_current = 50
        self.temperature_target = 10
        self.history_measured = deque([0]*SAMPLE_HISTORY_SIZE, SAMPLE_HISTORY_SIZE)
        self.history_target = deque([0]*SAMPLE_HISTORY_SIZE, SAMPLE_HISTORY_SIZE)
        self.oven_on = False
        self.cycles = {}
        self.read_cycles()

        self.timer = QBasicTimer()
        self.timer.start(1000*SAMPLE_PERIOD_SECONDS, self)

    def read_cycles(self):
        cycle_files = [filename
                       for filename in os.listdir()
                       if filename.endswith(".cycle")]
        for filename in cycle_files:
            with open(filename) as file_:
                lines = [line.split('#')[0].strip() for line in file_.readlines()]
                cycle_data = [float(line)
                              for line in lines
                              if line != ""]
                cycle_name = filename[:-6]
                self.cycles[cycle_name] = cycle_data
        for cycle_name in self.cycles.keys():
            self.cycle_list.addItem(cycle_name)
        self.cycle_list.sortItems()

    def change_preview(self, curr):
        temps = self.cycles[curr]
        times = list(range(len(temps)))
        self.preview_cycle_graph.set_lims((0,len(temps)), (0,100))
        self.preview_cycle_graph.update_figure([times], [temps])

    def start_cycle(self, *args):
        print("start cycle", args)

    def abort_cycle(self, *args):
        print("abort cycle", args)

    def gui_init(self):
        # Graph Area(Left)
        self.preview_cycle_graph = DynamicPlot(n_plots=1, n_data=SAMPLE_HISTORY_SIZE,
                                               labels=["preview"],
                                               x_range=(0, 10), y_range=(0, 100),
                                               title="Cycle Preview")
        self.monitor_cycle_graph = DynamicPlot(n_plots=2, n_data=SAMPLE_HISTORY_SIZE,
                                               labels=["Measured", "Target"],
                                               x_range=(0, SAMPLE_HISTORY_SIZE*SAMPLE_PERIOD_SECONDS),
                                               y_range=(0, 100),
                                               title="Temperature Monitor")

        ga = QWidget()
        ga_layout = QVBoxLayout()
        ga_layout.addWidget(self.monitor_cycle_graph)
        ga_layout.addWidget(self.preview_cycle_graph)
        ga.setLayout(ga_layout)

        # Top control area
        self.target_temp_label = QLabel("No Data")
        self.current_temp_label = QLabel("No Data")
        temp_inc = QPushButton("+")
        temp_inc.setAutoRepeat(True)
        temp_inc.clicked.connect(self.increment_temperature)
        temp_dec = QPushButton("-")
        temp_dec.setAutoRepeat(True)
        temp_dec.clicked.connect(self.decrement_temperature)

        # Bottom control area
        self.cycle_list = QListWidget()
        self.cycle_list.currentTextChanged.connect(self.change_preview)
        self.start_cycle_button = QPushButton("Start Cycle")
        self.start_cycle_button.clicked.connect(self.start_cycle)
        self.abort_cycle_button = QPushButton("Abort Cycle")
        self.abort_cycle_button.clicked.connect(self.abort_cycle)
        self.quit_button = QPushButton("Quit")

        # Control area layout
        ca = QWidget()
        ca_layout = QGridLayout()
        ca_layout.addWidget(self.target_temp_label, 0, 0, 1, 3)
        ca_layout.addWidget(self.current_temp_label, 1, 0, 1, 3)
        ca_layout.addWidget(temp_dec, 0, 3, 1, 1)
        ca_layout.addWidget(temp_inc, 0, 4, 1, 1)
        ca_layout.addWidget(QLabel("Available Curves"), 2, 0, 1, 3)
        ca_layout.addWidget(self.cycle_list, 3, 0, 8, 3)
        ca_layout.addWidget(self.start_cycle_button, 3, 3, 1, 2)
        ca_layout.addWidget(self.abort_cycle_button, 4, 3, 1, 2)
        ca_layout.addWidget(self.quit_button, 10, 3, 1, 2)
        ca.setLayout(ca_layout)

        # Main Widget Setup
        main = QSplitter()
        main.addWidget(ga)
        main.addWidget(ca)
        self.setCentralWidget(main)

        self.center()
        self.setWindowTitle('Oven Controller')
        self.show()

    def center(self):
        screen = QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.resize(screen.width()//2, screen.height()//2)
        self.move((screen.width()-size.width())//2,
                  (screen.height()-size.height())//2)

    def update_temperature(self):
        if VIRTUAL_HARDWARE:
            self.temperature_current = self.temperature_current + 3*(.5 - random.random())
        else:
            raise NotImplementedError("Serial interface not implemented.")
        self.history_measured.append(self.temperature_current)
        self.history_target.append(self.temperature_target)
        logging.info("Temp Reading: {:.2f}, Temp Target: {:.2f}"
                     .format(self.temperature_current, self.temperature_target))
        t_error = self.temperature_current-self.temperature_target
        if(t_error > TEMP_ERROR_THRESHOLD):
            logging.warning("Temperature too hot!")
        elif(t_error < -TEMP_ERROR_THRESHOLD):
            logging.warning("Temperature too cold!")

    def turn_oven_on(self):
        if VIRTUAL_HARDWARE:
            pass
        else:
            raise NotImplementedError("Serial interface not implemented.")
        self.oven_on = True

    def turn_oven_off(self):
        if VIRTUAL_HARDWARE:
            pass
        else:
            raise NotImplementedError("Serial interface not implemented.")
        self.oven_on = False

    def update_ui(self):
        self.current_temp_label.setText("Current: {:.2f}°C"
                                        .format(self.temperature_current))
        plot_times = [i*SAMPLE_PERIOD_SECONDS for i in range(SAMPLE_HISTORY_SIZE)]
        self.monitor_cycle_graph.update_figure([plot_times, plot_times],
                                               [self.history_measured, self.history_target])

    def update_temperature_target(self):
        self.target_temp_label.setText("Target:   {:.2f}°C"
                                       .format(self.temperature_target))


    def increment_temperature(self, *args):
        self.temperature_target += 1
        self.update_temperature_target()

    def decrement_temperature(self, *args):
        self.temperature_target -= 1
        self.update_temperature_target()

    def timerEvent(self, event):
        if event.timerId() == self.timer.timerId():
            self.update_temperature()
            self.update_ui()
        else:
            super(Board, self).timerEvent(event)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    oc = OvenController()
    sys.exit(app.exec_())
