#ifndef __TabX_H
#define __TabX_H

#include "anaPixelTab.h"

using namespace std;

class TabX : public anaPixelTab
{

public:
	
	TabX();
	TabX(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	
	virtual ~TabX();
	
	
private:
	
	TGTextButton *execute;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TH1F *thetaDistribution;
	bool calculated;
	
	ClassDef(TabX,0);
};

#endif
