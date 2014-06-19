#ifndef __LumiblockTab_cc
#define __LumiblockTab_cc

/*  LumiblockTab.cc
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
#include "LumiblockTab.h"    //include the header file for this class.

using namespace std;

ClassImp(LumiblockTab);

LumiblockTab::LumiblockTab() : PixelTab(){}

LumiblockTab::LumiblockTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :PixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram with occupancy per event for each module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","LumiblockTab",this,"executeAnalysis()");
	
	//LayerSelection = new TGButtonGroup(this,3,1);
	//Layer1 = new TGRadioButton(LayerSelection,"Layer 1");
	//Layer1->SetState(EButtonState(kButtonDown));
	//Layer2 = new TGRadioButton(LayerSelection,"Layer 2");
	//Layer3 = new TGRadioButton(LayerSelection,"Layer 3");
	
	AddFrame(description);
	//AddFrame(LayerSelection,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(calcButton, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	lumi = new TH1F(("lumi" + *thisTabNumber).c_str(),"Lumiblock Ratio",5000,0,5000);
	
} 

LumiblockTab::~LumiblockTab(){

	if (lumi != 0) delete lumi;

	}

//void LumiblockTab::LoopSlot(){
	//Loop(-1);
//}

void LumiblockTab::remoteExecute(){
	calcButton->Clicked();
}

void LumiblockTab::executeAnalysis(){

	if(!isCalculated){
		
		pixelTree->Project(("lumi" + *thisTabNumber).c_str(),"lumiblock");
		
		double Num = lumi->GetEntries();
		
		lumi->Scale(1/Num);
		
	}
	
	isCalculated = true;
		


	//LadderMinusz1->SetTitle("Cluster Occupancy - Layer 1 Ratio of on Track/All");
	//OccupancyStackLayer2->GetXaxis()->SetTitle("Ladder Number");
	//OccupancyStackLayer2->GetYaxis()->SetTitle("Number of Events");
	
	
	
	
	//if(Layer1->IsDown()){
		paintCanvas(1);
	//}
	
	//if(Layer2->IsDown()){
		//paintCanvas(2);
	//}
	
	//if(Layer3->IsDown()){
		//paintCanvas(3);
	//}
}

void LumiblockTab::paintCanvas(int layerNum){
	
	if(layerNum == 1){
		lumi->Draw();
	}
	//else if(layerNum == 2){
		//stack2->Draw("nostack");
	//}
	//else if(layerNum == 3){
		//stack3->Draw("nostack");
	//}
	//fCanvas->BuildLegend(0.5,0.67,0.88,0.88,"OccPerEvent");
	fCanvas->Update();
}

TCanvas* LumiblockTab::getCanvas(){
	return fCanvas;
}

void LumiblockTab::activateCanvas(){
	
	fCanvas->cd();
}

#endif
