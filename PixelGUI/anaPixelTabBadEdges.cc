#ifndef __anaPixelTabBadEdges_cc
#define __anaPixelTabBadEdges_cc

/*  anaPixelTabBadEdges.cc
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

#include "anaPixelTabBadEdges.h"    //include the header file for this class.

using namespace std;

ClassImp(anaPixelTabBadEdges);

anaPixelTabBadEdges::anaPixelTabBadEdges() : anaPixelTab(){}

anaPixelTabBadEdges::anaPixelTabBadEdges(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram With fraction of hits on edges");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","anaPixelTabBadEdges",this,"LoopSlot()");
	

	
	AddFrame(description);
	AddFrame(calcButton,    new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	badEdgeDistribution = new TH1F(("badEdgeDistribution" + *thisTabNumber).c_str(),
	                                "Fraction of hits on edge",100,0,.25);
	                                
	//initialize the array to zero
	for (int layer = 0; layer < 3; layer++)
	{
		for (int ladder = -22; ladder < 22; ladder++)
		{
			for (int module = -4; module < 4; module++)
			{
				for (int ROC = 0; ROC < 16; ROC++)
				{
					
					ROCHitsOnEdge[layer][ladderToIndex(layer + 1,ladder)][moduleToIndex(module)][ROC] = 0;
					ROCHitsTotal[layer][ladderToIndex(layer + 1,ladder)][moduleToIndex(module)][ROC] = 0;
				}
				if (module == -1) module++;
			}
			if (ladder == -1) ladder++;
		}
		
	}
	
			
} 

anaPixelTabBadEdges::~anaPixelTabBadEdges(){
	delete badEdgeDistribution;
	
	}

void anaPixelTabBadEdges::LoopSlot(){
	Loop(-1);
}

void anaPixelTabBadEdges::remoteExecute(){
	Loop(-1);
}

void anaPixelTabBadEdges::Loop(int nevt){
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("DgN",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClModule",1);
	fChain->SetBranchStatus("DgRoc",1);
	fChain->SetBranchStatus("DgRocC",1);
	fChain->SetBranchStatus("DgRocR",1);
	fChain->SetBranchStatus("DgClI",1);

	if(!isCalculated){
		if (fChain == 0) return;
		
		Long64_t nentries = fChain->GetEntries();
		
		int step(100000), maxEvents(nentries); 
		if (nevt > 0 && nevt < nentries) maxEvents = nevt; 
		if (maxEvents < 1000000) step = 50000; 
		if (maxEvents < 100000)  step = 5000; 
		if (maxEvents < 10000)   step = 500; 
		if (maxEvents < 1000)    step = 100; 
		
		Long64_t nbytes = 0, nb = 0;
		
		for (Long64_t jentry=0; jentry<nentries;jentry++) {
			
			if ((nevt > -1) && (jentry > nevt)) break;
			Long64_t ientry = LoadTree(jentry);
			if (ientry < 0) break;
			nb = fChain->GetEntry(jentry);   nbytes += nb;
			if (Cut(ientry) < 0) continue;
			
			
			for (int ic = 0; ic < DgN; ++ic) {
				int layerNum  = ClLayer [ DgClI[ic] ];
				int ladderNum = ClLadder[ DgClI[ic] ];
				int moduleNum = ClModule[ DgClI[ic] ];
				int ROCNum    = DgRoc   [ ic ];
				
				if(layerNum > 0){
					if (DgRocR[ic] == 0 || DgRocR[ic] == 79 || DgRocC[ic] == 0 || DgRocC[ic] == 51)
					{
						ROCHitsOnEdge[layerNum - 1][ladderToIndex(layerNum,ladderNum)][moduleToIndex(moduleNum)][ROCNum]++;
					}
					ROCHitsTotal[layerNum - 1][ladderToIndex(layerNum,ladderNum)][moduleToIndex(moduleNum)][ROCNum]++;
				}
			}
			
		}
		int numHotEdges = 0;
		for (int layer = 0; layer < 3; layer++)
		{
			int ladderMin, ladderMax;
			if (layer == 0)
			{
				ladderMin = -10; ladderMax = 10;
			}
			if (layer == 1)
			{
				ladderMin = -16; ladderMax = 16;
			}
			if (layer == 2)
			{
				ladderMin = -22; ladderMax = 22;
			}
			
			for (int ladder = ladderMin; ladder < ladderMax; ladder++)
			{
				for (int module = -4; module < 4; module++)
				{
					for (int ROC = 0; ROC < 16; ROC++)
					{
						if(ROCHitsTotal[layer][ladderToIndex(layer + 1,ladder)][moduleToIndex(module)][ROC] != 0 &&
						   ROCHitsOnEdge[layer][ladderToIndex(layer + 1,ladder)][moduleToIndex(module)][ROC] != 0){
							
							float ratio = ROCHitsOnEdge[layer][ladderToIndex(layer + 1,ladder)]
													   [moduleToIndex(module)][ROC] / 
													   ((float)ROCHitsTotal[layer][ladderToIndex(layer + 1,ladder)]
																		          [moduleToIndex(module)][ROC]);
							if (ratio > .2)
							{
								cout << "Hot Edges at Layer: " << (layer + 1)
								     << ", Ladder: " << ladder 
								     << ", Module: " << module
								     << ", ROC  #: " << (ROC) 
								     << ", fraction on edge: " << ratio
								     << endl;
								numHotEdges++;
							}
							badEdgeDistribution->Fill(ratio);
						}
					}
					if (module == -1) module++;
				}
				if (ladder == -1) ladder++;
			}
			
		}
		isCalculated = true;
		cout << "There are " << numHotEdges << " ROC's with hot edges." << endl; 
	}
	
	paintCanvas();
	
}

int anaPixelTabBadEdges::ladderToIndex(int layer, int ladder){
	
	if(layer == 1){//20 ladders
		if(ladder <= -1){
			ladder = ladder +10;
		}
		else if(ladder >= 1){
			ladder = ladder + 9;
		}
	}
	else if(layer == 2){//32 ladders
		if(ladder <= -1){
			ladder = ladder +16;
		}
		else if(ladder >= 1){
			ladder = ladder + 15;
		}
	}
	else if(layer == 3){//44 ladders
		if(ladder <= -1){
			ladder = ladder +22;
		}
		else if(ladder >= 1){
			ladder = ladder + 21;
		}
	}
	return ladder;
}

int anaPixelTabBadEdges::moduleToIndex(int module){
	
	if(module <= -1){
		module = module + 4;
	}
	else if(module >= 1){
		module = module + 3;
	}
	return module;
}

void anaPixelTabBadEdges::paintCanvas(){
	
	badEdgeDistribution->Draw();
	fCanvas->Update();
}

TCanvas* anaPixelTabBadEdges::getCanvas(){
	return fCanvas;
}

void anaPixelTabBadEdges::activateCanvas(){
	
	fCanvas->cd();
}

#endif
