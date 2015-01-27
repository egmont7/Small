#ifndef __LowROCOccupancies_H
#define __LowROCOccupancies_H

/*  LowROCOccupancies.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 21, 2010
 * 		
 * 		This tab finds the ratio of a single ROC's occupancy over the average
 * 		occupancy for the Module that it is on. It then prints information
 * 		about any outliers to the console. This is a good tab to check for
 * 		dead ROCs and modules, and also for ROCs that have unusually high
 * 		or low occupancy. 
 * 
 */

#include "anaPixelTab.h"
using namespace std;

class LowROCOccupancies : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:

	LowROCOccupancies();
	LowROCOccupancies(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void LoopSlot();
	void Loop(int nevt = -1);
	void paintCanvas();
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~LowROCOccupancies();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F *ROCOccupancyDistribution;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	int ROCHitsTotal[3][44][8][16];
	float moduleAverages[3][44][8];
	
	int ladderToIndex(int layer, int ladder);
	int moduleToIndex(int module);
	bool isHalfModule(int layer, int ladder);

	ClassDef(LowROCOccupancies,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
