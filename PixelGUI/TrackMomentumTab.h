#ifndef __TrackMomentumTab_H
#define __TrackMomentumTab_H

#include "anaPixelTab.h"

#include <stdio.h>
using namespace std;

class TrackMomentumTab : public anaPixelTab
{

public:
	
	TrackMomentumTab();
	TrackMomentumTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	void paintCanvas();
	void paintCanvasSlot();
	
	virtual ~TrackMomentumTab();
	
	
private:
	
	TGTextButton *execute;
	
	TCanvas *fCanvas;
	string *thisTabNumber;

	TH1F *Momentum;
	
	ClassDef(TrackMomentumTab,0);
};

#endif
