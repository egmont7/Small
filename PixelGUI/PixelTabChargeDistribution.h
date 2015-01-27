#ifndef __PixelTabChargeDistribution_H
#define __PixelTabChargeDistribution_H

/*  PixelTabChargeDistribution.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This Class is to be used with PixelGUI. It fills a histogram 
 * 		of the ClusterCharge for each event. It includes
 * 		radio buttons to examine the dependence for each layer.
 * 		NOTE:these histograms can take some time to fill.(~45secs)
 * 
 * 		Modified July 13 by Caleb Fangmeier to fill histogram by manually
 * 		looping over all events in the manor of anaPixelTree;
 */
#include "anaPixelTab.h"
using namespace std;

class PixelTabChargeDistribution : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:

	PixelTabChargeDistribution();
	PixelTabChargeDistribution(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	//void executeAnalysis();
	void LoopSlot();
	void Loop(int nevt = -1);
	void paintCanvas(int layerNum);
	TCanvas* getCanvas();
	
	void activateCanvas();
	
	void remoteExecute();
	
	virtual ~PixelTabChargeDistribution();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F     *ChargeTracksL1, *ChargeNoTracksL1, 
		     *ChargeTracksL2, *ChargeNoTracksL2, 
		     *ChargeTracksL3, *ChargeNoTracksL3;
	THStack *stack1, *stack2, *stack3;
	TGButtonGroup *layerSelection, *bunchCutSelection;
	TGRadioButton *Layer1, *Layer2, *Layer3;
	TGRadioButton *noCut, *goodBunches, *noGoodBunches;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	ClassDef(PixelTabChargeDistribution,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
