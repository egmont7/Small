#ifndef __DigiROCMap_cc
#define __DigiROCMap_cc

/*  DigiROCMap.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This class allows for the examining of hit maps for a single ROC.
 * 		It also allows stacking multiple ROCs on top of each other via a
 * 		check box.
 * 
 */

#include "DigiROCMap.h"    //include the header file for this class.

using namespace std;

ClassImp(DigiROCMap);

DigiROCMap::DigiROCMap() : anaPixelTab(){}

DigiROCMap::DigiROCMap(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	gStyle->SetHistFillStyle(1);
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Show Digis(Hits) for a Specific ROC.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","DigiROCMap",this,"LoopSlot()");
	
	
	TGHorizontalFrame *bottomFrame = new TGHorizontalFrame(this,kHorizontalFrame);
	TGCompositeFrame *entryFrame = new TGCompositeFrame(bottomFrame);
	TGMatrixLayout *m = new TGMatrixLayout(entryFrame,2,4,1,1);
	entryFrame->SetLayoutManager(m);
	
	layerEntry  = new TGTextEntry(entryFrame,"");
	ladderEntry = new TGTextEntry(entryFrame,"");
	moduleEntry = new TGTextEntry(entryFrame,"");
	ROCEntry    = new TGTextEntry(entryFrame,"");
	TGLabel *layerLabel  = new TGLabel(entryFrame,"enter Layer");
	TGLabel *ladderLabel = new TGLabel(entryFrame,"enter Ladder");
	TGLabel *moduleLabel = new TGLabel(entryFrame,"enter Module");
	TGLabel *ROCLabel    = new TGLabel(entryFrame,"enter ROC #");
	
	entryFrame->AddFrame(layerLabel);
	entryFrame->AddFrame(ladderLabel);
	entryFrame->AddFrame(moduleLabel);
	entryFrame->AddFrame(ROCLabel);
	entryFrame->AddFrame(layerEntry);
	entryFrame->AddFrame(ladderEntry);
	entryFrame->AddFrame(moduleEntry);
	entryFrame->AddFrame(ROCEntry);
	
	overlay = new TGCheckButton(bottomFrame,"Overlay Subsequent Modules");
	bottomFrame->AddFrame(entryFrame);
	bottomFrame->AddFrame(overlay, new TGLayoutHints(kLHintsCenterY));
	
	AddFrame(description);
	AddFrame(calcButton,    new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	AddFrame(bottomFrame, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	                                
	//initialize the array
	stringstream ss (stringstream::in | stringstream::out);
	stringstream ss2(stringstream::in | stringstream::out);
	for (int layer = 1; layer <= 3; layer++)
	{
		int ladderMin, ladderMax;
		if (layer == 1)
		{
			ladderMin = -10; ladderMax = 10;
		}
		if (layer == 2)
		{
			ladderMin = -16; ladderMax = 16;
		}
		if (layer == 3)
		{
			ladderMin = -22; ladderMax = 22;
		}
		
		for (int ladder = ladderMin; ladder <= ladderMax; ladder++)
		{
			for (int module = -4; module <= 4; module++)
			{
				for(int ROC = 0; ROC <= 15; ROC++){
					
					ss  << "moduleMaps[" << layer - 1 << "][" << ladderToIndex(layer,ladder) << "]["
						<< moduleToIndex(module) << "][" << ROC << "]" << thisTabNumber->c_str();
					
					ss2 << "Layer: " << layer << " Ladder: " << ladder << " Module: "
						<< module << " ROC: " << ROC;
					
					moduleMaps[layer-1][ladderToIndex(layer,ladder)][moduleToIndex(module)][ROC] = 
									  new TH2D(ss.str().c_str(),ss2.str().c_str(),80,0,80,52,0,52);
									  
					
					ss.str("");
					ss2.str("");
					
				}
				if (module == -1) module++;
			}
			if(ladder == -1) ladder++;
		}
		
	}
	
			
}

DigiROCMap::~DigiROCMap(){
	for (int layer = 1; layer <= 3; layer++)
	{
		int ladderMin, ladderMax;
		if (layer == 1)
		{
			ladderMin = -10; ladderMax = 10;
		}
		if (layer == 2)
		{
			ladderMin = -16; ladderMax = 16;
		}
		if (layer == 3)
		{
			ladderMin = -22; ladderMax = 22;
		}
		
		for (int ladder = ladderMin; ladder <= ladderMax; ladder++)
		{
			for (int module = -4; module <= 4; module++)
			{
				for(int ROC = 0; ROC <= 15; ROC++){
					
					delete moduleMaps[layer-1][ladderToIndex(layer,ladder)][moduleToIndex(module)][ROC];
				}
				if (module == -1) module++;
			}
			if(ladder == -1) ladder++;
		}
		
	}
	}

void DigiROCMap::LoopSlot(){
	

	Loop(-1);
}

void DigiROCMap::remoteExecute(){
	layerEntry->SetText("1");
	ladderEntry->SetText("1");
	moduleEntry->SetText("1");
	ROCEntry->SetText("1");
	Loop(-1);
}

void DigiROCMap::Loop(int nevt){
	
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
			
				if(layerNum != -99)//filter out FPIX
				{
					moduleMaps[layerNum - 1][ladderToIndex(layerNum,ladderNum)]
					          [moduleToIndex(moduleNum)][ROCNum]->Fill(DgRocR[ic],DgRocC[ic]);
				}
			}
		}
		isCalculated = true;
	}
	paintCanvas();
	
}

int DigiROCMap::ladderToIndex(int layer, int ladder){
	
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

int DigiROCMap::moduleToIndex(int module){
	
	if(module <= -1){
		module = module + 4;
	}
	else if(module >= 1){
		module = module + 3;
	}
	return module;
}

void DigiROCMap::paintCanvas(){
	
	int LayerNum = atoi(layerEntry->GetBuffer()->GetString());
	int LadderNum = atoi(ladderEntry->GetBuffer()->GetString());
	int ModuleNum = atoi(moduleEntry->GetBuffer()->GetString());
	int ROCNum    = atoi(ROCEntry->GetBuffer()->GetString());
	
	cout << "layer: " << LayerNum << " Ladder: " << LadderNum << " Module: " << ModuleNum << endl;
	
	if (overlay->IsDown())
	{
		moduleMaps[LayerNum - 1][ladderToIndex(LayerNum,LadderNum)][moduleToIndex(ModuleNum)][ROCNum]->Draw("colzsame");
	}
	else{
		moduleMaps[LayerNum - 1][ladderToIndex(LayerNum,LadderNum)][moduleToIndex(ModuleNum)][ROCNum]->Draw("colz");
	}
	fCanvas->Update();
}

TCanvas* DigiROCMap::getCanvas(){
	return fCanvas;
}

void DigiROCMap::activateCanvas(){
	fCanvas->cd();
}

#endif
