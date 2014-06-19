#ifndef __DigiModuleMap_cc
#define __DigiModuleMap_cc

/*  DigiModuleMap.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 * 		
 * 		This class allows for the examining of hit maps for a single module.
 * 		It also allows stacking multiple modules on top of each other via a
 * 		check box.
 * 
 */

#include "DigiModuleMap.h"    //include the header file for this class.

using namespace std;

ClassImp(DigiModuleMap);

DigiModuleMap::DigiModuleMap() : anaPixelTab(){}

DigiModuleMap::DigiModuleMap(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	gStyle->SetHistFillStyle(1);
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Show Digis(Hits) for a Specific Module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","DigiModuleMap",this,"LoopSlot()");
	
	
	TGHorizontalFrame *bottomFrame = new TGHorizontalFrame(this,kHorizontalFrame);
	TGCompositeFrame *entryFrame = new TGCompositeFrame(bottomFrame);
	TGMatrixLayout *m = new TGMatrixLayout(entryFrame,2,3,1,1);
	entryFrame->SetLayoutManager(m);
	
	layerEntry = new TGTextEntry(entryFrame,"");
	ladderEntry = new TGTextEntry(entryFrame,"");
	moduleEntry = new TGTextEntry(entryFrame,"");
	TGLabel *layerLabel = new TGLabel(entryFrame,"enter Layer");
	TGLabel *ladderLabel = new TGLabel(entryFrame,"enter Ladder");
	TGLabel *moduleLabel = new TGLabel(entryFrame,"enter Module");
	
	entryFrame->AddFrame(layerLabel);
	entryFrame->AddFrame(ladderLabel);
	entryFrame->AddFrame(moduleLabel);
	entryFrame->AddFrame(layerEntry);
	entryFrame->AddFrame(ladderEntry);
	entryFrame->AddFrame(moduleEntry);
	
	overlay = new TGCheckButton(bottomFrame,"Overlay Subsequent Modules");
	bottomFrame->AddFrame(entryFrame);
	bottomFrame->AddFrame(overlay, new TGLayoutHints(kLHintsCenterY));
	
	AddFrame(ECanvas,     new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(description, new TGLayoutHints(kLHintsTop));
	AddFrame(calcButton,  new TGLayoutHints(kLHintsCenterX | kLHintsTop));
	AddFrame(bottomFrame, new TGLayoutHints(kLHintsCenterX | kLHintsTop));

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
				
				ss  << "moduleMaps[" << layer - 1 << "][" << ladderToIndex(layer,ladder) << "]["
				    << moduleToIndex(module) << "]" << thisTabNumber->c_str();
				
				ss2 << "Layer: " << layer << " Ladder: " << ladder << " Module: "
				    << module;
				
				moduleMaps[layer-1][ladderToIndex(layer,ladder)][moduleToIndex(module)] = 
				                  new TH2D(ss.str().c_str(),ss2.str().c_str(),160,0,160,416,0,416);
				                  
				
				ss.str("");
				ss2.str("");
				if (module == -1) module++;
			}
			if(ladder == -1) ladder++;
		}
		
	}
	
			
}

DigiModuleMap::~DigiModuleMap(){
	
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
				delete moduleMaps[layer-1][ladderToIndex(layer,ladder)][moduleToIndex(module)];
				if (module == -1) module++;
			}
			if(ladder == -1) ladder++;
		}
	}
	
	}

void DigiModuleMap::LoopSlot(){
	
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("DgN",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClModule",1);
	fChain->SetBranchStatus("DgRow",1);
	fChain->SetBranchStatus("DgCol",1);
	fChain->SetBranchStatus("DgClI",1);
	
	Loop(-1);
}

void DigiModuleMap::remoteExecute(){
	layerEntry->SetText("1");
	ladderEntry->SetText("1");
	moduleEntry->SetText("1");
	Loop(-1);
}

void DigiModuleMap::Loop(int nevt){

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
			
				if(layerNum > 0)//filter out FPIX
				{
					moduleMaps[layerNum - 1][ladderToIndex(layerNum,ladderNum)][moduleToIndex(moduleNum)]->Fill(DgRow[ic],DgCol[ic]);
				}
			}
		}
		isCalculated = true;
	}
	paintCanvas();
	
}

int DigiModuleMap::ladderToIndex(int layer, int ladder){
	
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

int DigiModuleMap::moduleToIndex(int module){
	
	if(module <= -1){
		module = module + 4;
	}
	else if(module >= 1){
		module = module + 3;
	}
	return module;
}

void DigiModuleMap::paintCanvas(){
	
	int LayerNum = atoi(layerEntry->GetBuffer()->GetString());
	int LadderNum = atoi(ladderEntry->GetBuffer()->GetString());
	int ModuleNum = atoi(moduleEntry->GetBuffer()->GetString());
	
	cout << "layer: " << LayerNum << " Ladder: " << LadderNum << " Module: " << ModuleNum << endl;
	
	if (overlay->IsDown())
	{
		moduleMaps[LayerNum - 1][ladderToIndex(LayerNum,LadderNum)][moduleToIndex(ModuleNum)]->Draw("colzsame");
	}
	else{
		moduleMaps[LayerNum - 1][ladderToIndex(LayerNum,LadderNum)][moduleToIndex(ModuleNum)]->Draw("colz");
	}
	fCanvas->Update();
}

TCanvas* DigiModuleMap::getCanvas(){
	return fCanvas;
}

void DigiModuleMap::activateCanvas(){
	
	fCanvas->cd();
}

#endif
