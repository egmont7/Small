#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <cstdio>
#include <string>
#include <set>

using namespace cv;
using namespace std;

//#define INTERACTIVE

void show(Mat &img){
    namedWindow("MyWindow", WINDOW_AUTOSIZE); //create a window with the name "MyWindow"
    imshow("MyWindow", img); //display the image which is stored in the 'img' in the "MyWindow" window
    waitKey(0); //wait infinite time for a keypress
    destroyWindow("MyWindow"); //destroy the window with the name, "MyWindow"
}

/** getFocus
  * Uses 'LAPV' algorithm (Pech2000)
  * As found here: http://stackoverflow.com/questions/7765810/is-there-a-way-to-detect-if-an-image-is-blurry
  */
double getFocus(Mat &img){
    int kernelSize = 5;

#ifdef INTERACTIVE
    show(img);
#endif

    Mat lap;
    Laplacian(img, lap, CV_64F);

#ifdef INTERACTIVE
    show(img);
#endif
    Scalar mu, sigma;
    meanStdDev(lap, mu, sigma);

    double focusMeasure = sigma.val[0]*sigma.val[0];
    return focusMeasure;
}



void do_all(){

    for(int i = 1; i <= 20; i++){
        string fileName;
        fileName += "./data/Focus/";
        char buf[10];
        sprintf(buf,"%02d",i);
        fileName += buf;
        fileName += ".png";
        Mat img = imread(fileName, IMREAD_GRAYSCALE);
        double focus = getFocus(img);
        cout << i << ": " << focus << endl;
    } 
}


int main(int argc, const char** argv)
{

    if( argc != 2) {
        do_all();
        return 0;
    }
    string fileName(argv[1]);
    cout << "Finding Fiducial in image: "  << fileName << endl;
    Mat img = imread(fileName, IMREAD_GRAYSCALE);
    if (img.empty()){
         cout << "Error : Image cannot be loaded..!!" << endl;
         return -1;
    }
    
    double focus = getFocus(img);
    cout << "The image focus is: " << focus << endl;
    
    
}
