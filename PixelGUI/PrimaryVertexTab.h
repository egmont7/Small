#ifndef __PrimaryVertexTab_H
#define __PrimaryVertexTab_H

#include "anaPixelTab.h"
#include "TGMsgBox.h"
#include "TProfile.h"
#include <stdio.h>
using namespace std;

class PrimaryVertexTab : public anaPixelTab
{

public:
	
	PrimaryVertexTab();
	PrimaryVertexTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	
	virtual ~PrimaryVertexTab();
	
	
private:
	
	TGTextButton *execute;
	TGTextEntry *max, *min;
	TGRadioButton *automatic, *fixed, *findRange;
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TProfile *PV;
	
	bool calculated;
	
	//TGButtonGroup *PlotSelection;
	//TGRadioButton *Plot1, *Ratio1;
	
	ClassDef(PrimaryVertexTab,0);
};

#endif
