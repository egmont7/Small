#ifndef __DigiROCMap_H
#define __DigiROCMap_H

/*  DigiROCMap.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This class allows for the examining of hit maps for a single ROC.
 * 		It also allows stacking multiple ROCs on top of each other via a
 * 		check box.
 * 
 */

#include "anaPixelTab.h"
using namespace std;

class DigiROCMap : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:

	DigiROCMap();
	DigiROCMap(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void LoopSlot();
	void Loop(int nevt = -1);
	void paintCanvas();
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~DigiROCMap();
	
private:

	
	TGTextButton *calcButton;
	TGTextEntry *layerEntry, *ladderEntry, *moduleEntry, *ROCEntry;
	TGCheckButton *overlay;
	
	
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TH2D *moduleMaps[3][44][8][16];
	
	int ladderToIndex(int layer, int ladder);
	int moduleToIndex(int module);

	ClassDef(DigiROCMap,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
