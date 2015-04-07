#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       project.py
#
#       Copyright 2011 Caleb Fangmeier

import numpy as np
import matplotlib.pyplot as plt
import integrator
from pylab import figure, cm
from matplotlib.colors import LogNorm

def main():
    #pathPlots(0.6,0.0,1.0,1.0)
    #pathPlots(1.2,0.0,1.5,1.0)
    #pathPlots(1.5,0.0,1.5,1.0)


    #bifurcationPlot(0.2, 1.2, 200, 0, 1., 1., 2*np.pi)
    #attractorPlot(0.6,0.0,1.0,1.0, 2*np.pi)
    attractorPlot(1.2,2.0,1.5,1.0, 4*np.pi)



    #makeMoneyPlots()


def attractorPlot(f0, f1, w0, w1, period, name = '', show = True, save = False):
    consts = {}
    consts["F0"] = f0
    consts["F1"] = f1
    consts["w0"] = w0
    consts["w1"] = w1
    consts["c"] = 1.0
    consts["k"] = -1.0
    consts["beta"] = 1.0
    consts["phi"] = 0.0

    x0 = 1.0
    v0 = 0.0
    plt.figure()
    plt.title("Poincare Plot - 2 Forcing Terms")
    plt.xlabel("x")
    plt.ylabel("v")

    x,v,t,f = integrator.integrate(consts, x0, v0, 10000, 5E-5, True, 2000000)
    N = len(x)//2
    plt.plot(x[N:],v[N:],'-r')
    strb_x = integrator.strobe_points(x[N:],t[N:],period)
    strb_v = integrator.strobe_points(v[N:],t[N:],period)
    lab = "$\omega_0={},F_0={},\omega_1={},F_1={}$".format(w0,f0,w1,f1)
    l = plt.plot(strb_x, strb_v,'.', label = lab)
    plt.setp(l, 'markersize', 2)
    plt.legend(loc = "best")
    plt.xlabel("$x$")
    plt.ylabel("$\dot{x}$")

    #plt.savefig("Poincare4.pdf")
    plt.show()


def pathPlots(f0, f1, w0, w1, name = '', show = True, save = False):

    consts = {}
    consts["F0"] = f0
    consts["F1"] = f1
    consts["w0"] = w0
    consts["w1"] = w1
    consts["c"] = 1.0
    consts["k"] = -1.0
    consts["beta"] = 1.0
    consts["phi"] = 0

    x0 = 1.0
    v0 = 0.0

    x,v,t,f = integrator.integrate(consts,x0,v0,500,5E-5, True, 20000)
    if not show and not save: return
    N = len(x)//2
    plt.figure()
    plt.plot(x[N:],v[N:])
    plt.axis([-2,2,-2,2])
    plt.title('Phase Space Trajectory')
    plt.xlabel("x")
    plt.ylabel("$\dot{x}$")
    if save:
        plt.savefig("pathPlots_phase_" + name + ".pdf")

    plt.figure()
    plt.plot(t,x)
    plt.title('x vs t')
    plt.xlabel("t")
    plt.ylabel("x")
    if save:
        plt.savefig("pathPlots_xt_" + name + ".pdf")

    if show:
        plt.show()

def bifurcationPlot(f0_min, f0_max, num_pts, f1, w0, w1,
                    period, name = '', show = True, save = False):

    consts = {}
    consts["F0"] = 0
    forces = list(np.linspace(f0_min,f0_max,num_pts))
    consts["F1"] = f1
    consts["c"] = 1.0
    consts["k"] = -1.0
    consts["beta"] = 1.0
    consts["w0"] = w0
    consts["w1"] = w1
    consts["phi"] = 0

    x0 = 1.0
    y0 = 0.0
    plt.figure()
    plt.title('Bifurcation - f1={}'.format(f1))
    plt.xlabel("$f_0$")
    plt.ylabel("$x$")
    for force in forces:
        consts["F0"] = force
        #print("integrating for F={}".format(force))
        X, V, T, F  = integrator.integrate(consts,x0,y0,400,.5*10**-5, True, 20000)
        cut = len(X)//2
        strobe_x = integrator.strobe_points(X[cut:],T[cut:],period)

        l = plt.plot([force]*len(strobe_x), strobe_x, 'b.')
        plt.setp(l, 'markersize', 2)
    #plt.savefig("bifurcation_f1_" + str(round(f,3))  + "w1_" + str(w)+ ".pdf")
    plt.show()


def makeMoneyPlots():
    T0 = np.pi*2.0
    #~ moneyPlot(1.5,T0*2)
    #~ moneyPlot(2.0,T0)
    #~ moneyPlot(2.5,T0*2)
    #~ moneyPlot(3.0,T0)
    #~ moneyPlot(3.5,T0*2)
    moneyPlot(4.0,T0)
    #~ moneyPlot(4.5,T0*2)


def moneyPlot(w, strobePeriod):

    ptsf1 = 150
    ptsf2 = 100

    f1Min = 0.01
    f1Max = 2.0
    f2Min = 0.01
    f2Max = 2.0

    f1_array = np.linspace(f1Min,f1Max,ptsf1)
    f2_array = np.linspace(f2Min,f2Max,ptsf2)

    Allresults = np.empty([ptsf2,ptsf1])

    consts = np.empty(8)
    consts[0] = 0 #F0---taken care of in loops
    consts[1] = 0 #F1---|
    consts[2] = 1.0 #delta
    consts[3] = -1.0 #beta
    consts[4] = 1.0 #alpha | omega0^2
    consts[5] = 1.0 #w0
    consts[6] = w #w1
    consts[7] = 0.0 #phi
    x0 = 1.0
    y0 = 0.0

    for i in range(ptsf2):

        for j in range(ptsf1):

            consts[0] = f1_array[j]
            consts[1] = f2_array[i]

            print("integrating for F1= " , consts[0], " F2= ", consts[1])
            results = integrator.integrate(consts,x0,y0,400,.5*10**-3, True, 40000)
            points = len(results[0])/2
            [maximas,times,maximasv] = integrator.strobePoints(results[0][points:],results[2][points:],results[1][points:],strobePeriod)
            numMax = len(maximas)
            if numMax > 10:

                Allresults[i,j] = 10
            else:
                Allresults[i,j] = numMax


    plt.figure()
    plt.title('Number of Unique Strobe Points')
    plt.imshow(np.flipud(Allresults), cmap=plt.matplotlib.cm.gist_rainbow, extent=( f1Min, f1Max,f2Min, f2Max), aspect = (f1Max - f1Min)/(f2Max - f2Min))
    plt.cb=plt.colorbar(pad=.04, aspect = 35)

    plt.xlabel("$F_1$")
    plt.ylabel("$F_2$")

    #~ plt.savefig("w_" + str(w) + "_flat.pdf")
    plt.show()

    print(10*"*", "Plot For w=", w, " Finished!! ", 10*"*")



if __name__ == '__main__':
    #import cProfile
    #cProfile.run('main()',sort='time')
    main()

