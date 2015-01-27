#ifndef __anaPixelTabBadEdges_H
#define __anaPixelTabBadEdges_H

/*  anaPixelTabBadEdges.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This class examines the distribution of the ratio of hits on edges.
 * 		It finds, for each ROC, the ratio of hits on edge pixels to total
 * 		hits and plots a histogram of those ratios. It also prints off
 * 		any ROC with unusually high edge pixel occupancies to the console.
 * 
 */

#include "anaPixelTab.h"
using namespace std;

class anaPixelTabBadEdges : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:

	anaPixelTabBadEdges();
	anaPixelTabBadEdges(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	void LoopSlot();
	void Loop(int nevt = -1);
	void paintCanvas();
	TCanvas* getCanvas();
	
	void activateCanvas();
	void remoteExecute();
	
	virtual ~anaPixelTabBadEdges();
	
private:

	
	TGTextButton *calcButton;
	
	TH1F *badEdgeDistribution;
	bool isCalculated;
	
	TCanvas *fCanvas;
	string *thisTabNumber;
	
	int ROCHitsOnEdge[3][44][8][16];
	int ROCHitsTotal[3][44][8][16];
	
	int ladderToIndex(int layer, int ladder);
	int moduleToIndex(int module);

	ClassDef(anaPixelTabBadEdges,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
