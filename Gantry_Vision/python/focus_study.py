#!/usr/bin/python2
# -*- coding: utf-8 -*-

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

NUM_SAMPLES = 49

FIT_ERROR_LIMIT = .22

def crop_borders(img, x1=CROP_LEFT, x2=CROP_RIGHT,
                 y1=CROP_TOP, y2=CROP_BOTTOM):
    img = img[y1:y2, x1:x2]
    return img


def compute_focus(img):
    lap = cv2.Laplacian(img, cv2.CV_64F)
    _, sigma = cv2.meanStdDev(lap)
    focus_measure = sigma[0][0]**2
    return focus_measure


LOG_CACHE = {}
def read_focus_log(fid_num=0, run_num=0):
    if (fid_num, run_num) not in LOG_CACHE:
        log_path = DATA_DIR+"fid{:1d}/{:1d}.txt".format(fid_num, run_num)
        with open(log_path, 'r') as log:
            data = [map(float, line.split()) for line in log.readlines()]
        data.sort(key=lambda x: x[1])
        foci, heights = [np.array(d) for d in zip(*data)]
        heights *= 1E3  # convert to μm from cm
        LOG_CACHE[(fid_num, run_num)] = (foci, heights)
    return LOG_CACHE[(fid_num, run_num)]


def compute_curve_single(fid_num=0, run_num=0):
    foci = []
    for i in range(50):
        img_path = DATA_DIR+"fid{:1d}/{:1d}_{:1d}.png".format(fid_num,
                                                              run_num, i)
        img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
        img = crop_borders(img)
        focus = compute_focus(img)
        foci.append(focus)
    return foci


def gaussian(x, H, A, mu, sigma):
    G = H + A*np.exp(-((x-mu)/sigma)**2)
    return G


def super_gaussian(x, H, A, mu, sigma):
    return np.exp(gaussian(x, H, A, mu, sigma))


def get_best_height_max(heights, foci):
    best_focus, best_height = max(zip(foci, heights), key=lambda x: x[0])
    return best_focus, best_height


show_fit = False
def get_best_height_fit(heights, foci):
    max_foc, max_h = get_best_height_max(heights, foci)
    heights = np.array(heights)
    foci = np.array(foci)
    foci = np.log(foci)
    p0 = [min(foci), max(foci)-min(foci), max_h, 50.]

    try:
        fit_result, cov = curve_fit(gaussian, heights, foci, p0=p0)
        err = 0
        for height, focus in zip(heights, foci):
            err += (gaussian(height, *fit_result)-focus)**2
        # err = np.sqrt(np.sum(np.diag(cov)))
        err = np.sqrt(err/len(heights))
        # print(err)
    except RuntimeError as err:
        # plt.figure()
        # xs = np.linspace(min(heights), max(heights), 200)
        # ys = gaussian(xs, *p0)
        # plt.plot(xs, ys, 'b-')
        # plt.plot(heights, foci, 'r.')
        # plt.show()
        return None
    if show_fit:
        plt.figure()
        xs = np.linspace(min(heights), max(heights), 200)
        ys = gaussian(xs, *fit_result)
        plt.plot(xs, ys, 'b-')
        plt.plot(heights, foci, 'r.')
        plt.show()
    if fit_result[1] < 0 or err > FIT_ERROR_LIMIT:
        return None
    return fit_result


def fit_sparcity(fid_num=0, sample_rate=1):  # 1=all, 2=even ...
    centers = []
    for i in range(NUM_SAMPLES):
        foci, heights = read_focus_log(fid_num=fid_num, run_num=i)
        foci = [x for i, x in enumerate(foci)
                if i % sample_rate == 0]
        heights = [x for i, x in enumerate(heights)
                   if i % sample_rate == 0]
        fit_result = get_best_height_fit(heights, foci)
        if fit_result is not None:
            centers.append(fit_result[2])  # append mu of gaussian
    return len(centers), np.mean(centers), np.std(centers)


def test_sparcity(fid_num=0):
    for sample_rate in [1, 2, 4, 8]:
        N, mean, std = fit_sparcity(fid_num=fid_num, sample_rate=sample_rate)
        print("="*40)
        print("Fiducial:{}, N:{}, sample_rate:1/{}\n \
mean={:.1f}μm  std={:.1f}μm".format(fid_num, N, sample_rate, mean, std))
    print("="*40, '\n')


def plot_histogram(data):
    hist, bins = np.histogram(data, bins=10)
    width = 0.7*(bins[1] - bins[0])
    center = (bins[:-1] + bins[1:])/2.
    plt.bar(center, hist, align='center', width=width)
    plt.show()


def fit_cutoff(fid_num=0, side_ignore=0):  # 0=all, 1=ignore first/last ...
    centers = []
    for i in range(NUM_SAMPLES):
        foci, heights = read_focus_log(fid_num=fid_num, run_num=i)
        if side_ignore != 0:
            foci = foci[side_ignore:-side_ignore]
            heights = heights[side_ignore:-side_ignore]
        fit_result = get_best_height_fit(heights, foci)
        if fit_result is not None:
            centers.append(fit_result[2])  # append mu of gaussian
    # plot_histogram(centers)
    return len(centers), np.mean(centers), np.std(centers)


def test_cutoff(fid_num=0):
    for cutoff in [0, 5, 10, 15, 20]:
        N, mean, std = fit_cutoff(fid_num=fid_num, side_ignore=cutoff)
        print("="*40)
        print("Fiducial:{}, N:{}, cutoff:{}\n \
mean={:.1f}μm  std={:.1f}μm".format(fid_num, N, cutoff, mean, std))
    print("="*40, '\n')


def old_func():
    for fid_num in range(4):
        centers_fit = []
        centers_max = []
        for i in range(NUM_SAMPLES):
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
        print("worst outlier: {}".format(max([abs(x-fit_mean)
                                              for x in centers_fit])))
        print('-'*80)


def main():
    print("{decor} BEGIN TEST SPARCITY {decor}".format(decor="="*10))
    for fid_num in range(4):
        test_sparcity(fid_num)
    print("{decor} BEGIN TEST CUTOFF {decor}".format(decor="="*10))
    for fid_num in range(4):
        test_cutoff(fid_num)


if __name__ == "__main__":
    main()
