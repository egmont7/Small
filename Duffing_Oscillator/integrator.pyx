#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       integrator.py
#
#       Copyright 2015 Caleb Fangmeier
# cython: profile=True
from cpython.array cimport array as c_array
from cpython cimport array
from libc cimport math
from array import array as pyarray
from collections import deque

cdef struct integration_params:
    float F0, F1, c, k, beta, w0, w1, phi

cdef float A21 =  1.0/5
cdef float A31 =  3.0/40
cdef float A32 =  9.0/40
cdef float A41 =  3.0/10
cdef float A42 = -9.0/10
cdef float A43 =  6.0/5
cdef float A51 = -11.0/55
cdef float A52 =  5.0/2
cdef float A53 = -70.0/27
cdef float A54 =  35.0/27
cdef float A61 = 1631.0/55296
cdef float A62 = 172.0/512
cdef float A63 = 575.0/13824
cdef float A64 = 44275.0/110592
cdef float A65 = 253.0/4096

#used in error calculation
cdef float B1S = 37.0/378.0 - 2825.0/27648.0
cdef float B2S = 0.0
cdef float B3S = 250.0/621.0 - 18575.0/48384.0
cdef float B4S = 125.0/594.0 - 13525.0/55296.0
cdef float B5S = 0.0 - 277.0/14336.0
cdef float B6S = 512.0/1771.0 - 1.0/4.0

#used in error calculation
cdef float B1 = 37.0/378
cdef float B2 = 0.0
cdef float B3 = 250.0/621
cdef float B4 = 125.0/594
cdef float B5 = 0.0
cdef float B6 = 512.0/1771

cdef float C2 = 1.0/5
cdef float C3 = 3.0/10
cdef float C4 = 3.0/5
cdef float C5 = 1.0
cdef float C6 = 7.0/8

def integrate(params , x0, v0, tEnd,
              errorTolerance, condenseData = False,  points = 5000):

    cdef integration_params p
    p.F0 = params["F0"]
    p.F1 = params["F1"]
    p.c = params["c"]
    p.k = params["k"]
    p.beta = params["beta"]
    p.w0 = params["w0"]
    p.w1 = params["w1"]
    p.phi = params["phi"]

    X,V,T,F = RK6(&p, x0, v0, tEnd, 1E-3, errorTolerance)
    if condenseData:
        return condense(X, V, T, F, points, ign_start=2, ign_end=2)
    else:
        return X,V,T,F

cdef RK6(integration_params* p, float x0, float v0, float tEnd,
         float dt0, float errorTolerance):

    cdef float stepMax = 1E-1
    cdef float dt = dt0
    cdef float x = x0
    cdef float v = v0
    cdef float t = 0.
    cdef list x_arr = [x0]
    cdef list v_arr = [v0]
    cdef list t_arr = [0.]
    cdef list f_arr = [forcingTerms(t,p)]
    cdef float[6] Kx
    cdef float[6] Kv
    cdef float error_x, error_v, error
    while t < tEnd:
        rhs(x,
            v,
            t, &Kx[0], &Kv[0], p)
        rhs(x + dt*(A21*Kx[0]),
            v + dt*(A21*Kv[0]),
            t + C2*dt, &Kx[1], &Kv[1] ,p)
        rhs(x + dt*(A31*Kx[0] + A32*Kx[1]),
            v + dt*(A31*Kv[0] + A32*Kv[1]),
            t + C3*dt, &Kx[2], &Kv[2], p)
        rhs(x + dt*(A41*Kx[0] + A42*Kx[1] + A43*Kx[2]),
            v + dt*(A41*Kv[0] + A42*Kv[1] + A43*Kv[2]),
            t + C4*dt, &Kx[3], &Kv[3], p)
        rhs(x + dt*(A51*Kx[0] + A52*Kx[1] + A53*Kx[2] + A54*Kx[3]),
            v + dt*(A51*Kv[0] + A52*Kv[1] + A53*Kv[2] + A54*Kv[3]),
            t + C5*dt, &Kx[4], &Kv[4], p)
        rhs(x + dt*(A61*Kx[0] + A62*Kx[1] + A63*Kx[2] + A64*Kx[3] + A65*Kx[4]),
            v + dt*(A61*Kv[0] + A62*Kv[1] + A63*Kv[2] + A64*Kv[3] + A65*Kv[4]),
            t + C6*dt, &Kx[5], &Kv[5], p)

        error_x = RK6_Error(Kx[0], Kx[1], Kx[2], Kx[3], Kx[4], Kx[5])
        error_v = RK6_Error(Kv[0], Kv[1], Kv[2], Kv[3], Kv[4], Kv[5])


        if error_x == 0. and error_v == 0.:
            error = 5.0
        else:
            error = errorTolerance/max(error_x,error_v)

        if error > 1.:
            #good step
            x += dt*(B1*Kx[0] + B2*Kx[1] + B3*Kx[2] + B4*Kx[3] + B5*Kx[4] + B6*Kx[5])
            v += dt*(B1*Kv[0] + B2*Kv[1] + B3*Kv[2] + B4*Kv[3] + B5*Kv[4] + B6*Kv[5])

            t_arr.append(t)
            f_arr.append(forcingTerms(t,p))
            x_arr.append(x)
            v_arr.append(v)
            t = t + dt
            if error > 2 and dt < stepMax:
                #step too good, increase dt
                dt *= (error**0.2)

        else:
            #redo step with smaller interval
            dt *= 0.9*(error**0.2)

    return (x_arr, v_arr, t_arr, f_arr)


cdef inline float RK6_Error(float k1, float k2, float k3, float k4, float k5, float k6):
    return abs(B1S*k1 + B2S*k2 + B3S*k3 + B4S*k4 + B5S*k5 + B6S*k6)


cdef inline  void rhs(float x, float v, float t,
         float* xp, float* vp, integration_params* p):
    xp[0] = v
    vp[0] = p.F0*math.cos(p.w0*t) + p.F1*math.cos(p.w1*t + p.phi) -\
            p.c*v - p.k*x - p.beta*x**3

cdef inline float forcingTerms(float t, integration_params* p):
    return p.F0*math.cos(p.w0*t) + p.F1*math.cos(p.w1*t + p.phi)



def condense(list x_dat, list v_dat, list t_dat, list f_dat,
              int steps, ign_start = 0, ign_end = 0):

    if len(x_dat) < 5: raise ValueError("Insufficent data length.")
    cdef list t_cond = []

    cdef float t_start = t_dat[0] + ign_start
    cdef float t_end = t_dat[-1] - ign_end
    cdef float dt = (t_end-t_start)/steps

    cdef int i
    for i in range(steps):
        t_cond.append(t_start + dt*i)

    cdef list x_cond = resample(x_dat, t_dat, t_cond)
    cdef list v_cond = resample(v_dat, t_dat, t_cond)
    cdef list f_cond = resample(f_dat, t_dat, t_cond)

    return x_cond, v_cond, t_cond, f_cond


cdef list resample(list dat, list dat_times, list smpl_times):
    if len(dat) < 5:
        raise ValueError("Insufficient dat for interpolation")
    if smpl_times[-1] > dat_times[-1]:
        raise ValueError("Cannot sample at times after data")
    cdef list dat_smpl = []
    dat_iter = zip(dat, dat_times)
    dat_buf = deque(maxlen=5)
    for _ in range(5):
        dat_buf.append(next(dat_iter))
    cdef float t_nxt
    for t_nxt in smpl_times:
        while dat_buf[2][1] < t_nxt:
            dat_buf.append(next(dat_iter))
        dat_smpl.append(interp(dat_buf, t_nxt))
    return dat_smpl

cdef inline float interp(buf_dat, float time):
    cdef int offset
    if abs(buf_dat[2][1] - time) < abs(buf_dat[1][1] - time):
        offset = 1
    else:
        offset = 0
    cdef float x, x0, x1, x2, x3
    x = time
    x0 = buf_dat[0+offset][1]
    x1 = buf_dat[1+offset][1]
    x2 = buf_dat[2+offset][1]
    x3 = buf_dat[3+offset][1]

    cdef float c1, c2, c3, c4
    c1 = ((x - x1)*(x - x2)*(x - x3))/((x0 - x1)*(x0 - x2)*(x0 - x3))
    c2 = ((x - x0)*(x - x2)*(x - x3))/((x1 - x0)*(x1 - x2)*(x1 - x3))
    c3 = ((x - x0)*(x - x1)*(x - x3))/((x2 - x0)*(x2 - x1)*(x2 - x3))
    c4 = ((x - x0)*(x - x1)*(x - x2))/((x3 - x0)*(x3 - x1)*(x3 - x2))

    return  c1*buf_dat[0+offset][0] + c2*buf_dat[1+offset][0] +\
            c3*buf_dat[2+offset][0] + c4*buf_dat[3+offset][0]


cdef class StrobePoint:
    cdef float mean_x, mean_v
    cdef int n
    cdef list xs
    cdef list vs

    def __init__(self):
        self.xs = pyarray('f')
        self.vs = pyarray('f')
        self.mean_x = 0
        self.mean_v = 0
        self.n = 0

    def add_pt(self, x, v):
        self.mean_x = (self.mean_x*self.n + x)/(self.n+1)
        self.mean_v = (self.mean_v*self.n + x)/(self.v+1)
        self.xs.append(x)
        self.vs.append(v)


def strobe_points(list dat, list dat_t, float period, reduce_ = False):
    cdef float t_start = dat_t[0], t_end = dat_t[-1]
    cdef int num_strobes = int((t_end - t_start)//period)
    cdef list t_strb = []
    cdef int i
    for i in range(num_strobes+1):
        t_strb.append(t_start + i*period)

    cdef list dat_strb = resample(dat, dat_t, t_strb)

    if reduce_:
            raise NotImplementedError()
            #reduce_strobe_pts(strobe_x, strobe_v, strobe_reduce_x, strobe_reduce_v)
            #return strobe_reduce_x, strobe_reduce_v
    else:
        return dat_strb

