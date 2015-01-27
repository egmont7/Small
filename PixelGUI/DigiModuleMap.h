#ifndef __DigiModuleMap_H
#define __DigiModuleMap_H

/*  DigiModuleMap.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This class allows for the examining of hit maps for a single module.
 * 		It also allows stacking multiple modules on top of each other via a
 * 		check box.
 * 
 */

#include "anaPixelTab.h"
using namespace std;

class DigiModuleMap : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:

	DigiModuleMap();
	DigiModuleMap(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void LoopSlot();
	void Loop(int nevt = -1);
	void paintCanvas();
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~DigiModuleMap();
	
private:

	
	TGTextButton *calcButton;
	TGTextEntry *layerEntry, *ladderEntry, *moduleEntry;
	TGCheckButton *overlay;
	
	TH1F *badEdgeDistribution;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TH2D *moduleMaps[3][44][8];
	
	int ladderToIndex(int layer, int ladder);
	int moduleToIndex(int module);

	ClassDef(DigiModuleMap,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
