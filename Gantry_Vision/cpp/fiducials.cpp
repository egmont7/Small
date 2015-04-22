#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <string>
#include <set>

using namespace cv;
using namespace std;

#define INTERACTIVE


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

    cout << "Found " << contours.size() << " Contours of proper size." << endl;
    setTo(img, 1, 128);
#ifdef INTERACTIVE
    namedWindow("MyWindow", WINDOW_AUTOSIZE); //create a window with the name "MyWindow"
    imshow("MyWindow", img); //display the image which is stored in the 'img' in the "MyWindow" window
    waitKey(0); //wait infinite time for a keypress
    destroyWindow("MyWindow"); //destroy the window with the name, "MyWindow"
#endif
    for (int i = 0; i < contours.size(); i++){
        drawContours(img, contours, i, Scalar(255) );
    }

    if (contours.size() != 1) {
        cout << "ERROR: ERROR: ERROR" << endl;
        return Point(-1,-1);
    }
    vector<Point> fidContour = contours[0];

    Moments mu = moments(fidContour, false);
    Point2f centroid( mu.m10/mu.m00 , mu.m01/mu.m00 );
    circle(img, centroid, 3, Scalar(255), -1);

#ifdef INTERACTIVE
    namedWindow("MyWindow", WINDOW_AUTOSIZE); //create a window with the name "MyWindow"
    imshow("MyWindow", img); //display the image which is stored in the 'img' in the "MyWindow" window
    waitKey(0); //wait infinite time for a keypress
    destroyWindow("MyWindow"); //destroy the window with the name, "MyWindow"
#endif
    centroid.x /= cols;
    centroid.y /= rows;
    return centroid;
}

int main(int argc, const char** argv)
{

    if( argc != 3) return 0;

    string fileName(argv[1]);
    cout << "Finding Fiducial in image: "  << fileName << endl;
    Mat img = imread(fileName, IMREAD_GRAYSCALE);
    if (img.empty()){
         cout << "Error : Image cannot be loaded..!!" << endl;
         return -1;
    }

    int dilateSize;
    float shrinkFactor;
    float sizeMin, sizeMax;
    int colorGroups;

    string fidType(argv[2]);
    if(fidType == "HDI"){
        sizeMin = 0.23;
        sizeMax = 0.27;
        colorGroups = 2;
        shrinkFactor = 1.;
        dilateSize = 24/shrinkFactor;
    } else if(fidType == "BBM"){
        sizeMin = 0.01;
        sizeMax = 0.02;
        colorGroups = 3;
        shrinkFactor = 1.;
        dilateSize = 16/shrinkFactor;
    } else {
        cout << "Unrecognized Fiducial Type \"" << fidType << "\" Quitting" << endl;
        return 0;
    }


    Point2f fid = getFiducial(img,dilateSize,sizeMin,sizeMax,shrinkFactor,colorGroups);
    cout << "Found fiducial at (" << fid.x << ", " << fid.y << ")" << endl;
    return 0;
}
