#ifndef __PixelTabSizeDistribution_cc
#define __PixelTabSizeDistribution_cc

/*  PixelTabSizeDistribution.cc
 *  	Author: Austin Deck
 * 		contact: austin@huskers.unl.edu
 * 		date: July 28, 2010
 * 		
 * 		This Class is to be used with PixelGUI. It fills a histogram 
 * 		of Occupancy per Event for each module
 * 		radio buttons to examine the dependence for each layer.
 * 		NOTE:these histograms can take some time to fill.
 * 
 */
#include "PixelTabSizeDistribution.h"    //include the header file for this class.

using namespace std;

ClassImp(PixelTabSizeDistribution);

PixelTabSizeDistribution::PixelTabSizeDistribution() : PixelTab(){}

PixelTabSizeDistribution::PixelTabSizeDistribution(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :PixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram with occupancy per event for each module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","PixelTabSizeDistribution",this,"executeAnalysis()");
	
	LayerSelection = new TGButtonGroup(this,3,1);
	Layer1 = new TGRadioButton(LayerSelection,"Layer 1");
	Layer1->SetState(EButtonState(kButtonDown));
	Layer2 = new TGRadioButton(LayerSelection,"Layer 2");
	Layer3 = new TGRadioButton(LayerSelection,"Layer 3");
	
	AddFrame(description);
	AddFrame(LayerSelection,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(calcButton, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	//lumi = new TH1F(("lumi" + *thisTabNumber).c_str(),"Lumiblock Ratio",5000,0,5000);
	SizeTracksL1 = new TH1F("SizeTracksL1","Cluster Size", 30,0,30);
	SizeNoTracksL1 = new TH1F("SizeNoTracksL1","Cluster Size",30,0,30);
	SizeTracksL2 = new TH1F("SizeTracksL2","Cluster Size", 30,0,30);
	SizeNoTracksL2 = new TH1F("SizeNoTracksL2","Cluster Size",30,0,30);
	SizeTracksL3 = new TH1F("SizeTracksL3","Cluster Size", 30,0,30);
	SizeNoTracksL3 = new TH1F("SizeNoTracksL3","Cluster Size",30,0,30);
} 

PixelTabSizeDistribution::~PixelTabSizeDistribution(){

	if(SizeTracksL1 != 0) delete SizeTracksL1;
	if(SizeNoTracksL1 != 0) delete SizeNoTracksL1;
	if(SizeTracksL2 != 0) delete SizeTracksL2;
	if(SizeNoTracksL2 != 0) delete SizeNoTracksL2;
	if(SizeTracksL3 != 0) delete SizeTracksL3;
	if(SizeNoTracksL3 != 0) delete SizeNoTracksL3;
	}

//void PixelTabSizeDistribution::LoopSlot(){
	//Loop(-1);
//}

void PixelTabSizeDistribution::remoteExecute(){
	calcButton->Clicked();
}

void PixelTabSizeDistribution::executeAnalysis(){

	if(!isCalculated){
		
		
pixelTree->Project("SizeTracksL1","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 1 && ClType == 1");
pixelTree->Project("SizeNoTracksL1","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 1 && ClType == 2");
SizeNoTracksL1->SetLineColor(2);
SizeNoTracksL1->SetLineStyle(2);

SizeTracksL1->SetTitle("Cluster Size - Layer 1 - Red is Type 2 (w/o track)");
SizeTracksL1->GetXaxis()->SetTitle("Cluster Size");
SizeTracksL1->GetYaxis()->SetTitle("Number of Events");





pixelTree->Project("SizeTracksL2","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 2 && ClType == 1");
pixelTree->Project("SizeNoTracksL2","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 2 && ClType == 2");
SizeNoTracksL2->SetLineColor(2);
SizeNoTracksL2->SetLineStyle(2);

SizeTracksL2->SetTitle("Cluster Size - Layer 2 - Red is Type 2 (w/o track)");
SizeTracksL2->GetXaxis()->SetTitle("Cluster Size");
SizeTracksL2->GetYaxis()->SetTitle("Number of Events");






pixelTree->Project("SizeTracksL3","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 3 && ClType == 1");
pixelTree->Project("SizeNoTracksL3","ClSize","ClSize < 80 && ClLadder > -30 && ClLadder < 30 && ClLayer == 3 && ClType == 2");
SizeNoTracksL3->SetLineColor(2);
SizeNoTracksL3->SetLineStyle(2);

SizeTracksL3->SetTitle("Cluster Size - Layer 3 - Red is Type 2 (w/o track)");
SizeTracksL3->GetXaxis()->SetTitle("Cluster Size");
SizeTracksL3->GetYaxis()->SetTitle("Number of Events");		
	}
	
	isCalculated = true;
		


	//LadderMinusz1->SetTitle("Cluster Occupancy - Layer 1 Ratio of on Track/All");
	//OccupancyStackLayer2->GetXaxis()->SetTitle("Ladder Number");
	//OccupancyStackLayer2->GetYaxis()->SetTitle("Number of Events");
	
	
	
	
	if(Layer1->IsDown()){
		paintCanvas(1);
	}
	
	if(Layer2->IsDown()){
		paintCanvas(2);
	}
	
	if(Layer3->IsDown()){
		paintCanvas(3);
	}
}

void PixelTabSizeDistribution::paintCanvas(int layerNum){
	
	if(layerNum == 1){
		SizeTracksL1->Draw();
		SizeNoTracksL1->Draw("same");
	}
	else if(layerNum == 2){
		SizeTracksL2->Draw();
		SizeNoTracksL2->Draw("same");
	}
	else if(layerNum == 3){
		SizeTracksL3->Draw();
		SizeNoTracksL3->Draw("same");
	}
	//fCanvas->BuildLegend(0.5,0.67,0.88,0.88,"OccPerEvent");
	fCanvas->Update();
}

TCanvas* PixelTabSizeDistribution::getCanvas(){
	return fCanvas;
}

void PixelTabSizeDistribution::activateCanvas(){
	
	fCanvas->cd();
}

#endif
