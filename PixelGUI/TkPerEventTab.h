#ifndef __TkPerEventTab_H
#define __TkPerEventTab_H

#include "anaPixelTab.h"
#include "TGMsgBox.h"
#include "TProfile.h"
#include <stdio.h>
using namespace std;

class TkPerEventTab : public anaPixelTab
{

public:
	
	TkPerEventTab();
	TkPerEventTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	void paintCanvas();
	void paintCanvasSlot();
	
	virtual ~TkPerEventTab();
	
	
private:
	
	TGTextButton *execute;
	TGTextEntry *max, *min;
	TGRadioButton *automatic, *fixed, *findRange;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TProfile *OnTrackCl;
	TProfile *NumTk;
	TProfile *Ratio;
	THStack *stack1;
	
	TGButtonGroup *PlotSelection;
	TGRadioButton *Plot1, *Ratio1;
	
	ClassDef(TkPerEventTab,0);
};

#endif
