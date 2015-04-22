
#include <NIVision.h>
#include <CV.h>


extern C
{

void getFiducialLV(char* LVImagePtr, int LVLineWidth, 
                   int LVWidth, int LVHeight,
                   const char* fidType); //One of "HDI" or "BBM"


void getFocusLV(char* LVImagePtr, int LVLineWidth,
                int LVWidth, int LVHeight);


}
