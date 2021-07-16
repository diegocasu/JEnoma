from PlotBuilder import PlotBuilder
import numpy as np
import configparser as cp
import pandas as pd
import ast

toList = lambda s : ast.literal_eval(s)

class Main:

    def main(self):
        self.config = cp.ConfigParser()
        self.config.read("settings.ini")
        self.types = {
            "Id"                 : "str",
            "ComputationTime"    : "list",
            "CommunicationTime"  : "list",
            "fitnesses"   		 : "list",
        }
        self.jenomaStatdf = pd.read_csv(self.config["General"]["working_csv"])
        self.drawFitnessesPlot()


    def drawFitnessesPlot(self):
        self.scatterBuilder = PlotBuilder("fitnesses")
        fitnesses = self.jenomaStatdf["fitnesses"]
        y_values = [toList(s) for s in self.jenomaStatdf["fitnesses"]]
        idLabels = self.jenomaStatdf["Id"]
        self.scatterBuilder.add_subplot(x_axis_value=np.arange(start=0, step=1, stop=len(y_values[0])),
                                        y_axis_values=y_values,
                                        labels=idLabels)
        self.scatterBuilder.draw()

    def drawPiePlot(self):
        self.pieBuilder = PlotBuilder("fitnesses")

    def drawHistoPlot(self):
        self.histoBuilder = PlotBuilder("fitnesses")


if __name__ == '__main__':
    Main().main()
