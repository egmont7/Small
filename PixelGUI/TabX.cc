#ifndef __TabX_cc
#define __TabX_cc

#include "TabX.h"

using namespace std;

ClassImp(TabX); 

TabX::TabX() : anaPixelTab(){} 

TabX::TabX(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str()
	                                                       ,this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	execute = new TGTextButton(this,"Execute");
	execute->Connect("Clicked()","TabX",this,"executeAnalysis()");
	AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
	thetaDistribution = new TH1F(("thetaDistribution" + *thisTabNumber).c_str(),"Theta Distribution",
	                             50,0,3.15);
	calculated = false;
} 

TabX::~TabX(){
	delete thetaDistribution; 
}

void TabX::executeAnalysis(){
	
	if(!calculated){
		Loop();
		calculated = true;
	}
	thetaDistribution->Draw();
	fCanvas->Update();
	
}


void TabX::Loop(int nevt){
	
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
		for (int ic = 0; ic < TkN; ++ic) {
			
			thetaDistribution->Fill(TkTheta[ic]);
			
		}
////////////////////////////////////////////////////////////////////////
	}

}

TCanvas* TabX::getCanvas(){
	return fCanvas;
}

void TabX::activateCanvas(){
	fCanvas->cd();
}

void TabX::remoteExecute(){
	execute->Clicked();
}

#endif
