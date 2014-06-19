#ifndef __ROCEffTab_H
#define __ROCEffTab_H

/*  ROCEffTab.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier74@gmail.com
 * 		date: May 17, 2011
 * 		
 * 		This class is part of a ROC relative efficiency study wherein 
 * 		hits that are part of clusters associated with a track are 
 * 		plotted over the surface of a ROC and then the results of many
 * 		ROC's are summed together to determine if , on the average, 
 * 		there exist parts of a ROC that are more efficient than others.
 * 
 */

#include "anaPixelTab.h"

using namespace std;

class ROCEffTab : public anaPixelTab
{

public:
	
	ROCEffTab();
	ROCEffTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	bool useROC(int Layer, int Ladder, int Module, int ROC);
	
	void paintCanvas(int plot);
	virtual ~ROCEffTab();
	
	
private:
	
	TGTextButton *execute;
	TGRadioButton *avgHits, *stdHits;
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	TH2D *ROCHits, *ROCHitsDev;
	bool calculated;
	
	ClassDef(ROCEffTab,0);
};

#endif
