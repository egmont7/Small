#ifndef __kShortTab_H
#define __kShortTab_H

#include "anaPixelTab.h"

using namespace std;

class kShortTab : public anaPixelTab
{

public:
	
	kShortTab();
	kShortTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	
	virtual ~kShortTab();
	
	
private:
	
	TGTextButton *execute;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TH1F *thetaDistribution;
	bool calculated;
	
	ClassDef(kShortTab,0);
};

#endif
