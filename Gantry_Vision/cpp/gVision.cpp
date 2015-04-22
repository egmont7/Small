#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <string>
#include <set>

#include "gVision.hpp"

using namespace cv;
using namespace std;

void setTo(Mat &img, char from, char to){
    int n = img.rows * img.cols;
    for(int i = 0; i < n; i++){
        char &curr = img.at<char>(i);
        if(curr == from) curr = to;
    }
}

void setForeground(Mat &img, char fgId, char fgVal, char bgVal){
    int n = img.rows * img.cols;
    for(int i = 0; i < n; i++){
        char &curr = img.at<char>(i);
        curr = (curr == fgId) ? fgVal : bgVal;
    }
}

void doKMeans(Mat &img, int k){    
    TermCriteria tc(TermCriteria::COUNT + TermCriteria::EPS, 10, 1.0);
    int flags = KMEANS_PP_CENTERS;
    Mat kmeansIn = img.reshape(1, img.rows*img.cols);
    Mat colVecD, bestLabels, centers, clustered;
    kmeansIn.convertTo(colVecD, CV_32FC3, 1.0/255.0);
    double compactness = kmeans(colVecD, k, bestLabels, tc, 1, flags, centers);

    bestLabels = bestLabels.reshape(1, img.rows);
    bestLabels.convertTo(bestLabels, CV_8U);
    img = bestLabels;

    float maxVal = -1; int foreground = -1;
    for(int i = 0; i < centers.rows; i++){
        float center = centers.at<float>(i);
        if(center > maxVal){
            maxVal = center;
            foreground = i;
        }
    }
    setForeground(img, foreground, 1, 0);
}

void doDilate(Mat &img, int size){
    Size s(size, size);
    Mat element = getStructuringElement(MORPH_ELLIPSE, s);
    dilate(img, img,element);
}

vector<vector<Point>> getContours(Mat &img, float sizeMin, float sizeMax){
    float pixels = img.rows * img.cols;
    vector<vector<Point>> contours;
    vector<Vec4i> hierarchy;
    findContours(img.clone(), contours, hierarchy, CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE );
    
    vector<vector<Point>> passContours;
    for(int i = 0; i < contours.size(); i++){
        float size = contourArea(contours[i]) / pixels;
        RotatedRect rr = minAreaRect(contours[i]);
        float ar = float(rr.size.width) / rr.size.height;
        if( (size > sizeMin && size < sizeMax) && (ar > 0.9 && ar < 1.1) ){
             passContours.push_back(contours[i]);
        }
    }
    return passContours;
}

Point2f getFiducial(Mat &img, int dilateSize, float sizeMin, float sizeMax, 
                    float shrinkFactor, int colorGroups){
    
    int rows = img.rows / shrinkFactor;
    int cols = img.cols / shrinkFactor;
    int pixels = rows * cols;
    Size s(cols, rows);
    resize(img, img, s);

    doKMeans(img, colorGroups);

    doDilate(img, dilateSize);

    vector<vector<Point>> contours  = getContours(img, sizeMin, sizeMax);

    setTo(img, 1, 128);

    for (int i = 0; i < contours.size(); i++){
        drawContours(img, contours, i, Scalar(255) );
    }

    if (contours.size() != 1) {
        return Point(-1,-1); //ERROR!!!! Too many or too few fiducials identified
    }
    vector<Point> fidContour = contours[0];

    Moments mu = moments(fidContour, false);
    Point2f centroid( mu.m10/mu.m00 , mu.m01/mu.m00 );
    circle(img, centroid, 3, Scalar(255), -1);

    centroid.x /= cols;
    centroid.y /= rows;
    return centroid;
}


/** getFocus
  * Uses 'LAPV' algorithm (Pech2000)
  * As found here: http://stackoverflow.com/questions/7765810/is-there-a-way-to-detect-if-an-image-is-blurry
  */
double getFocus(Mat &img){
    int kernelSize = 5;
    Mat lap;
    Laplacian(img, lap, CV_64F);

    Scalar mu, sigma;
    meanStdDev(lap, mu, sigma);

    double focusMeasure = sigma.val[0]*sigma.val[0];
    return focusMeasure; 
}





