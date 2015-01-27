#ifndef __OccTab_H
#define __OccTab_H

/*  OccTab.h
 *  	Author: Austin Deck
 * 		contact: austin@huskers.unl.edu
 */
#include "PixelTab.h"
using namespace std;

class OccTab : public PixelTab // <-- ":" indicates inheritance relationship
{


public:

	OccTab();
	OccTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void executeAnalysis();
	
	TCanvas* getCanvas();
	void remoteExecute();
	void activateCanvas();
	
	virtual ~OccTab();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F     *LadderMinusZ1, *LadderMinusZ2, 
		     *LadderMinusZ3, *LadderMinusZ4, 
	         *LadderMinusZ5, *LadderMinusZ6, 
		     *LadderMinusZ7, *LadderMinusZ8, 

		     *LadderPlusZ1, *LadderPlusZ2,
		     *LadderPlusZ3, *LadderPlusZ4,
		     *LadderPlusZ5, *LadderPlusZ6,
		     *LadderPlusZ7, *LadderPlusZ8,
		     
		     *RatioMinus1, *RatioMinus2,
		     *RatioMinus3, *RatioMinus4,
		     
		     *RatioPlus1, *RatioPlus2,
		     *RatioPlus3, *RatioPlus4;
		     
	THStack *OccupancyStackLayer2;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	ClassDef(OccTab,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
