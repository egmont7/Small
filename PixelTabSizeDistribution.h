#ifndef __PixelTabSizeDistribution_H
#define __PixelTabSizeDistribution_H

/*  PixelTabSizeDistribution.h
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

class PixelTabSizeDistribution : public PixelTab // <-- ":" indicates inheritance relationship
{


public:

	PixelTabSizeDistribution();
	PixelTabSizeDistribution(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void executeAnalysis();
	//void LoopSlot();
	//void Loop(int nevt = -1);
	void paintCanvas(int layerNum);
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~PixelTabSizeDistribution();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F     *SizeTracksL1, *SizeNoTracksL1,
				*SizeTracksL2, *SizeNoTracksL2,
				*SizeTracksL3, *SizeNoTracksL3;
		     
	
	TGButtonGroup *LayerSelection;
	TGRadioButton *Layer1, *Layer2, *Layer3;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	ClassDef(PixelTabSizeDistribution,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
