from PlotBuilder import PlotBuilder
import numpy as np
import configparser as cp
import pandas as pd
import ast
import scipy.stats
import json
import math

toList = lambda s : ast.literal_eval(s)

class Main:

    def main(self):
        self.config = cp.ConfigParser()
        self.config.read("settings.ini")
        self.jenomaStatdf = pd.read_csv(self.config["General"]["working_csv"])
       # self.drawFitnessesPlot()
        self.drawHistoPlot()


    def drawFitnessesPlot(self):
        self.scatterBuilder = PlotBuilder("fitnesses")
        fitnesses = self.jenomaStatdf["fitnesses"]
        y_values = [toList(s) for s in self.jenomaStatdf["fitnesses"]]
        idLabels = self.jenomaStatdf["Id"]
        self.scatterBuilder.add_subplot(x_axis_values=np.arange(start=0, step=1, stop=len(y_values[0])),
                                        y_axis_values=y_values,
                                        labels=idLabels)
        self.scatterBuilder.draw()

    def drawHistoPlot(self):
        self.histoBuilder = PlotBuilder("avgratiohisto")

        computationTime   = [toList(cmpt) for cmpt in self.jenomaStatdf["ComputationTime"]]
        communicationTime = [toList(commt) for commt in self.jenomaStatdf["CommunicationTime"]]
        ci_level = float(json.loads(self.config["Plot_Profile"]["avgratiohisto"])["confidence_level"])
        avgratio = []
        workerCI = []

        for comp, comm in zip(computationTime, communicationTime):
            timingRatio = [compElem/commElem for compElem, commElem in zip(comp, comm)]
            avg = float(np.average(timingRatio))
            avgratio.append(avg)
            ci = math.sqrt(np.var(timingRatio, ddof=1) / ((len(timingRatio)-1) * len(timingRatio))) * math.fabs(scipy.stats.norm.ppf((1-ci_level)/2))
            workerCI.append(ci)

        idLabels = self.jenomaStatdf["Id"].tolist()

        self.histoBuilder.add_subplot(x_axis_values=np.arange(start=0, step=1, stop=len(avgratio)),
                                      y_axis_values=avgratio,
                                      labels=idLabels,
                                      error_bar=workerCI)
        self.histoBuilder.draw()


    def drawPiePlot(self):
        self.pieBuilder = PlotBuilder("fitnesses")


if __name__ == '__main__':
    Main().main()
