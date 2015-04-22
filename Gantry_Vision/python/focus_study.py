#!/usr/bin/python2
from __future__ import print_function, division
import cv2
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit

DATA_DIR = "/home/caleb/Sources/Small/Gantry_Vision/data/focus_images/"

CROP_LEFT = 61
CROP_RIGHT = 588
CROP_TOP = 10
CROP_BOTTOM = 406


def crop_borders(img, x1=CROP_LEFT, x2=CROP_RIGHT,
                 y1=CROP_TOP, y2=CROP_BOTTOM):
    img = img[y1:y2, x1:x2]
    return img


def compute_focus(img):
    lap = cv2.Laplacian(img, cv2.CV_64F)
    _, sigma = cv2.meanStdDev(lap)
    focus_measure = sigma[0][0]**2
    return focus_measure


def read_focus_log(fid_num=0, run_num=0):
    log_path = DATA_DIR+"fid{:1d}/{:1d}.txt".format(fid_num, run_num)
    with open(log_path, 'r') as log:
        data = [map(float, line.split()) for line in log.readlines()]
    data.sort(key=lambda x: x[1])
    foci, heights = zip(*data)
    return np.array(foci), np.array(heights)


def compute_curve_single(fid_num=0, run_num=0):
    foci = []
    for i in range(50):
        img_path = DATA_DIR+"fid{:1d}/{:1d}_{:1d}.png".format(fid_num, run_num, i)
        img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
        img = crop_borders(img)
        focus = compute_focus(img)
        foci.append(focus)
    return foci


def gaussian(x, H, A, mu, sigma):
    return H + A*np.exp(-((x-mu)/sigma)**2)


def super_gaussian(x, H, A, mu, sigma):
    return np.exp(gaussian(x, H, A, mu, sigma))


def get_best_height_max(heights, foci):
    best_focus, best_height = max(zip(foci, heights), key=lambda x: x[0])
    return best_focus, best_height


def get_best_height_fit(heights, foci):
    max_foc, max_h = get_best_height_max(heights, foci)
    heights = np.array(heights)
    foci = np.array(foci)
    foci = np.log(foci)
    p0 = [min(foci), np.log(max_foc)-min(foci), max_h, .1]

    fit_result, cov = curve_fit(gaussian, heights, foci, p0=p0)
    if fit_result[1] < 0:
        raise ValueError("Fit terminated with negative amplitude")
    return fit_result


def main():
    for fid_num in range(4):
        centers_fit = []
        centers_max = []
        for i in range(49):
            # foci_comp = compute_curve_single(fid_num=fid_num, run_num=i)
            foci_read, heights = read_focus_log(fid_num=fid_num, run_num=i)
            centers_max.append(get_best_height_max(heights, foci_read)[1])
            fit_result = get_best_height_fit(heights, foci_read)
            centers_fit.append(fit_result[2])
            # plt_heights = np.linspace(min(heights), max(heights), 100)
            # plt.plot(plt_heights, super_gaussian(plt_heights, *fit_result), 'r')
            # plt.plot(heights, foci_read, 'b.')
            # plt.yscale('log')
            # plt.show()

        max_mean = np.mean(centers_max)
        max_std = np.std(centers_max)
        print("Fiducial {}".format(fid_num))
        print("Method max: mean={}, std={}".format(max_mean, max_std))
        print("Worst outlier: {}\n".format(max([abs(x-max_mean) for x in centers_max])))

        fit_mean = np.mean(centers_fit)
        fit_std = np.std(centers_fit)
        print("method fit: mean={}, std={}".format(fit_mean, fit_std))
        print("worst outlier: {}".format(max([abs(x-fit_mean) for x in centers_fit])))
        print('-'*80)


if __name__ == "__main__":
    main()
