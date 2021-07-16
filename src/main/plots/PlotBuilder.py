import configparser

import matplotlib.ticker as ticker
import matplotlib.pyplot as plt
import configparser as cp
import numpy as np
import json
import ast

toList = lambda s : ast.literal_eval(s)


class PlotBuilder:

    def __init__(self, pltProfile):
        self.plot_axes = plt.gca()
        self.config = cp.ConfigParser()
        self.config.read("settings.ini")
        plt.tight_layout()
        self.colors = toList(self.config["Plot_Profile"]["color_list"])

        if pltProfile == "fitnesses":
            self.plot_profile = json.loads(self.config["Plot_Profile"]["fitnesses"])
            self.figure, self.axes = plt.subplots(self.plot_profile["rows"],
                                                  self.plot_profile["columns"])
            self.figure.set_figwidth(10)
            self.figure.suptitle('Fitness evolution')

        if pltProfile == "avgratiohisto":
            self.plot_profile = json.loads(self.config["Plot_Profile"]["avgratiohisto"])




    def set_axes_label(self, x_axis_name, y_axis_name):
        self.plot_axes.set_xlabel(x_axis_name, fontsize=14, labelpad=10)
        self.plot_axes.set_ylabel(y_axis_name, fontsize=14, labelpad=10, rotation=90)

    def print_to_screen(self):
        plt.draw()
        plt.show(block=True)

    def add_subplot(self, x_axis_values=None, y_axis_values=None, labels=None, error_bar=None):
        if self.plot_profile["name"] == "FitnessPlot":
            plt.clf()
            for y, ax, lb, color in zip(y_axis_values, self.axes, labels, self.colors):
                ax.plot(x_axis_values, y, color=color, marker=".", markevery=0.1)
                ax.set_title(lb)
                # ll, bb, ww, hh = ax.get_position().bounds
                # ax.set_position([ll, bb , ww*0.5, hh])

        elif self.plot_profile["name"] == "histogramPlot":
            plt.clf()
            self.figure, self.axes = plt.subplots()
            self.figure.suptitle('Average ratio Computing Time / Communication Time')
            self.axes.bar(tuple(x_axis_values), tuple(y_axis_values), 0.15, yerr=tuple(error_bar))
            self.axes.set_xticks(x_axis_values)
            self.axes.set_xticklabels((tuple(labels)))

        elif self.plot_profile == "avgratiopie":
            print("TODO")
        else:
            print("not valid profile")

    def save_to_file(self):
        directory = self.config["General"]["export_directory"]
        export_name = directory + self.plot_profile["name"] + self.plot_profile["Extension"]
        plt.tight_layout()
        plt.savefig(export_name, format="jpg", dpi=300, bbox_inches='tight')

    def draw(self):
        if self.config["General"]["save_to_file"] == "yes":
            self.save_to_file()
        else:
            self.print_to_screen()
