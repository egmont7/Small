#ifndef __PChemTab_H
#define __PChemTab_H

/*  PChemTab.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier74@gmail.com
 * 		date: May 24, 2011
 * 		
 * 		
 * 
 */

#include "anaPixelTab.h"
#include <ctime>

using namespace std;

class PChemTab : public anaPixelTab
{

public:
	
	PChemTab();
	PChemTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void Loop2();
	void executeAnalysis();
	void saveAllPlots();
	
	virtual ~PChemTab();
	
	
private:
	
	TGTextButton *execute, *execute2, *saveAll;
	TGRadioButton *evenPixB,
	              *oddPixB,
	              *evenPixTksB, 
	              *oddPixTksB, 
	              *ClXB, 
	              *ClYB, 
	              *TkXResB, 
	              *TkYResB,
	              *overLocB,
	              *colRowOverlayEB,
	              *colRowOverlayETkB,
	              *colRowOverlayOB,
	              *colRowOverlayOTkB;
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TGTextEntry *prefixField;
	
	TH2D *evenPixP, 
	     *oddPixP,
	     *evenPixTksP,
	     *oddPixTksP,
	     *overLocP,
	     *colRowOverlayEP,
	     *colRowOverlayETkP,
	     *colRowOverlayOP,
	     *colRowOverlayOTkP;
	
	TH1D *ClXP, 
	     *ClYP, 
	     *TkXResP,
	     *TkYResP;
	
	

	
	TH2F *singlePixO[10];
	TH2F *singlePixE[10];
	
	TH2F *singlePixOTks[10];
	TH2F *singlePixETks[10];
	
	TH2F *singlePixOAll[10];
	TH2F *singlePixEAll[10];
	
	TH2F *singlePixOTksAll[10];
	TH2F *singlePixETksAll[10];
	
	
	vector<int> rowsE;
	vector<int> colsE;
	vector<int> rowsO;
	vector<int> colsO;
	
	
	bool calculated;
	
	ClassDef(PChemTab,0);
};

#endif
