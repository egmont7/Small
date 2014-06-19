#ifndef __LowROCOccupancies_cc
#define __LowROCOccupancies_cc

/*  LowROCOccupancies.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 21, 2010
 * 		
 * 		This tab finds the ratio of a single ROC's occupancy over the average
 * 		occupancy for the Module that it is on. It then prints information
 * 		about any outliers to the console. This is a good tab to check for
 * 		dead ROCs and modules, and also for ROCs that have unusually high
 * 		or low occupancy. 
 * 
 */

#include "LowROCOccupancies.h"    //include the header file for this class.

using namespace std;

ClassImp(LowROCOccupancies);

LowROCOccupancies::LowROCOccupancies() : anaPixelTab(){}

LowROCOccupancies::LowROCOccupancies(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram With ratio of hits on a ROC over the average for its module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","LowROCOccupancies",this,"LoopSlot()");
	

	
	AddFrame(description);
	AddFrame(calcButton,    new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	ROCOccupancyDistribution = new TH1F(("ROCOccupancyDistribution" + *thisTabNumber).c_str(),
	                                "Ratio of ROC Occupancy to Module Average",100,0,3);
	                                
	//initialize the array to zero
	for (int layer = 0; layer < 3; layer++)
	{
		for (int ladder = -22; ladder < 22; ladder++)
		{
			for (int module = -4; module < 4; module++)
			{
				for (int ROC = 0; ROC < 16; ROC++)
				{
					ROCHitsTotal[layer][ladderToIndex(layer,ladder)][moduleToIndex(module)][ROC] = 0;
				}
				moduleAverages[layer][ladderToIndex(layer,ladder)][moduleToIndex(module)] = 0;
				if (module == -1) module++;
			}
			if (ladder == -1) ladder++;
		}
		
	}
	
			
} 

LowROCOccupancies::~LowROCOccupancies(){
	
	delete ROCOccupancyDistribution;
	}

void LowROCOccupancies::LoopSlot(){
	Loop(-1);
}

void LowROCOccupancies::remoteExecute(){
	Loop(-1);
}

void LowROCOccupancies::Loop(int nevt){
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
				
				if(layerNum > 0){// Filter FPIX
					ROCHitsTotal[layerNum - 1][ladderToIndex(layerNum,ladderNum)][moduleToIndex(moduleNum)][ROCNum]++;
				}
			}
			
		}
		
		for (int layer = 0; layer < 3; layer++)//This block finds Module Averages
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
					int ROCMax;
					if(isHalfModule(layer+1,ladder)) ROCMax = 8;
					else ROCMax = 16;
					
					float occAvg = 0;
					
					for (int ROC = 0; ROC < ROCMax; ROC++)
					{
						occAvg += ROCHitsTotal[layer][ladderToIndex(layer + 1,ladder)]
												     [moduleToIndex(module)][ROC];
					}
					
					moduleAverages[layer]
					              [ladderToIndex(layer + 1,ladder)]
					              [moduleToIndex(module)] = occAvg / ROCMax;
					
					if (module == -1) module++;
				}
				if (ladder == -1) ladder++;
			}
			
		}
		
		int numLowOcc = 0; int numHighOcc = 0;
		for (int layer = 0; layer < 3; layer++)//This block compares ROC's to Module Averages
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
					int ROCMax;
					if(isHalfModule(layer+1,ladder)) ROCMax = 8;
					else ROCMax = 16;
					
					
					for (int ROC = 0; ROC < ROCMax; ROC++)
					{
						float occRatio = ROCHitsTotal[layer][ladderToIndex(layer + 1,ladder)]
												     [moduleToIndex(module)][ROC] / 
												     moduleAverages[layer]
												                   [ladderToIndex(layer + 1,ladder)]
												                   [moduleToIndex(module)];
						ROCOccupancyDistribution->Fill(occRatio);
						
						if(occRatio < .5){
							cout << "Unusually Low occupancy at "
							     << "Layer: " << (layer + 1)
							     << ", Ladder: " << ladder
							     << ", Module: " << module
							     << ", ROC #: "  << ROC
							     << " Ratio: " << occRatio << endl;
							     numLowOcc++;
						}
						
						if(occRatio > 2){
							cout << "Unusually High occupancy at "
							     << "Layer: " << (layer + 1)
							     << ", Ladder: " << ladder
							     << ", Module: " << module
							     << ", ROC #: "  << ROC
							     << " Ratio: " << occRatio << endl;
							     numHighOcc++;
						}
					}
					if (module == -1) module++;
				}
				if (ladder == -1) ladder++;
			}
			
		}
		cout << "ROC's with high Occupancy: " << numHighOcc << endl;
		cout << "ROC's with low Occupancy: " << numLowOcc << endl;  
		isCalculated = true;
	}
	
	paintCanvas();
	
}

int LowROCOccupancies::ladderToIndex(int layer, int ladder){
	
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

int LowROCOccupancies::moduleToIndex(int module){
	
	if(module <= -1){
		module = module + 4;
	}
	else if(module >= 1){
		module = module + 3;
	}
	return module;
}

bool LowROCOccupancies::isHalfModule(int layer, int ladder){
	bool isHalf = false;
	if(layer == 1 && (ladder == 1 || ladder == -1 || ladder == -10 || ladder == 10)){
		isHalf = true;
	}
	else if(layer == 2 && (ladder == 1 || ladder == -1 || ladder == -16 || ladder == 16)){
		isHalf = true;
	}
	else if(layer == 3 && (ladder == 1 || ladder == -1 ||  ladder == -22 || ladder == 22)){
		isHalf = true;
	}
	
	return isHalf;
}

void LowROCOccupancies::paintCanvas(){
	
	ROCOccupancyDistribution->Draw();
	fCanvas->Update();
}

TCanvas* LowROCOccupancies::getCanvas(){
	return fCanvas;
}

void LowROCOccupancies::activateCanvas(){
	
	fCanvas->cd();
}

#endif
