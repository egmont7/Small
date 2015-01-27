#ifndef __TrackMomentumTab_cc
#define __TrackMomentumTab_cc

#include "TrackMomentumTab.h"

using namespace std;

ClassImp(TrackMomentumTab); 

TrackMomentumTab::TrackMomentumTab() : anaPixelTab(){} 

TrackMomentumTab::TrackMomentumTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str()
	                                                       ,this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	execute = new TGTextButton(this,"Execute");
	execute->Connect("Clicked()","TrackMomentumTab",this,"executeAnalysis()");
	AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
	Momentum = new TH1F("Momentum","True Momentum of Tracks",200,0,20);
	
} 

TrackMomentumTab::~TrackMomentumTab(){
	
	if(Momentum   != 0) delete Momentum;
	}

void TrackMomentumTab::executeAnalysis(){
		
Loop();
paintCanvas();
}
void TrackMomentumTab::paintCanvasSlot(){
	paintCanvas();
}

void TrackMomentumTab::paintCanvas(){
	
		Momentum->Draw("");
		fCanvas->Update();
	}


void TrackMomentumTab::Loop(int nevt){
	
	fChain->SetBranchStatus("*",0);

	fChain->SetBranchStatus("event",1);
	fChain->SetBranchStatus("TkN",1);
	fChain->SetBranchStatus("TkPt",1);
	fChain->SetBranchStatus("TkTheta",1);
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
////////////////////////////////////////////////////////////////////////
		double Mo;
		for (int ic = 0; ic < TkN; ++ic) {
			Mo = TkPt[ic]/sin(TkTheta[ic]);
			Momentum->Fill(Mo);
		}
////////////////////////////////////////////////////////////////////////
	
}
}
TCanvas* TrackMomentumTab::getCanvas(){
	return fCanvas;
}

void TrackMomentumTab::activateCanvas(){
	fCanvas->cd();
}

void TrackMomentumTab::remoteExecute(){
	execute->Clicked();
}

#endif
