#ifndef __LumiblockTab_H
#define __LumiblockTab_H

/*  LumiblockTab.h
 *  	Author: Austin Deck
 * 		contact: austin@huskers.unl.edu
 * 		
 * 		This Class is to be used with PixelGUI. It fills a histogram 
 * 		of the ClusterCharge for each event. It includes
 * 		radio buttons to examine the dependence for each layer.
 * 		NOTE:these histograms can take some time to fill.(~45secs)
 * 
 * 		Modified July 13 by Caleb Fangmeier to fill histogram by manually
 * 		looping over all events in the manor of anaPixelTree;
 */
#include "PixelTab.h"
using namespace std;

class LumiblockTab : public PixelTab // <-- ":" indicates inheritance relationship
{


public:

	LumiblockTab();
	LumiblockTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void executeAnalysis();
	//void LoopSlot();
	//void Loop(int nevt = -1);
	void paintCanvas(int layerNum);
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~LumiblockTab();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F     *lumi;
		     
	
	//TGButtonGroup *LayerSelection;
	//TGRadioButton *Layer1, *Layer2, *Layer3;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	ClassDef(LumiblockTab,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
