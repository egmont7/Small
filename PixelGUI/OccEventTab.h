#ifndef __OccEventTab_H
#define __OccEventTab_H

/*  OccEventTab.h
 *  	Author: Austin Deck
 * 		contact: austin@huskers.unl.edu
 */
#include "PixelTab.h"
using namespace std;

class OccEventTab : public PixelTab // <-- ":" indicates inheritance relationship
{


public:

	OccEventTab();
	OccEventTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void executeAnalysis();
	
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~OccEventTab();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F     *LadderMinusz1, *LadderMinusz2, 
		     *LadderMinusz3, *LadderMinusz4, 
		     *LadderPlusz1, *LadderPlusz2,
		     *LadderPlusz3, *LadderPlusz4,
		     *LadderOccMinusz1, *LadderOccMinusz2, 
		     *LadderOccMinusz3, *LadderOccMinusz4, 
		     *LadderOccPlusz1, *LadderOccPlusz2,
		     *LadderOccPlusz3, *LadderOccPlusz4,
		     *Total;
		     
	THStack *OccupancyStackLayer1;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	ClassDef(OccEventTab,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
