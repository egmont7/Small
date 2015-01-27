#ifndef __OccTab_cc
#define __OccTab_cc

/*  OccTab.cc
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

#include "OccTab.h"    //include the header file for this class.

using namespace std;

ClassImp(OccTab);

OccTab::OccTab() : PixelTab(){}

OccTab::OccTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :PixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram with occupancy per event for each module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","OccTab",this,"executeAnalysis()");
	
	//LayerSelection = new TGButtonGroup(this,3,1);
	//Layer1 = new TGRadioButton(LayerSelection,"Layer 1");
	//Layer1->SetState(EButtonState(kButtonDown));
	//Layer2 = new TGRadioButton(LayerSelection,"Layer 2");
	//Layer3 = new TGRadioButton(LayerSelection,"Layer 3");
	
	AddFrame(description);
	//AddFrame(LayerSelection,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(calcButton, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	LadderMinusZ1 = new TH1F(("LadderMinusZ1" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	LadderMinusZ2 = new TH1F(("LadderMinusZ2" + *thisTabNumber).c_str(),"Ring 2, Minus Z",23,-11.5,11.5);
	LadderMinusZ3 = new TH1F(("LadderMinusZ3" + *thisTabNumber).c_str(),"Ring 3, Minus Z",23,-11.5,11.5);
	LadderMinusZ4 = new TH1F(("LadderMinusZ4" + *thisTabNumber).c_str(),"Ring 4, Minus Z",23,-11.5,11.5);
	LadderMinusZ5 = new TH1F(("LadderMinusZ5" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	LadderMinusZ6 = new TH1F(("LadderMinusZ6" + *thisTabNumber).c_str(),"Ring 2, Minus Z",23,-11.5,11.5);
	LadderMinusZ7 = new TH1F(("LadderMinusZ7" + *thisTabNumber).c_str(),"Ring 3, Minus Z",23,-11.5,11.5);
	LadderMinusZ8 = new TH1F(("LadderMinusZ8" + *thisTabNumber).c_str(),"Ring 4, Minus Z",23,-11.5,11.5);

	LadderPlusZ1 = new TH1F(("LadderPlusZ1" + *thisTabNumber).c_str(),"Ring 1, Plus Z",23,-11.5,11.5);
	LadderPlusZ2 = new TH1F(("LadderPlusZ2" + *thisTabNumber).c_str(),"Ring 2, Plus Z",23,-11.5,11.5);
	LadderPlusZ3 = new TH1F(("LadderPlusZ3" + *thisTabNumber).c_str(),"Ring 3, Plus Z",23,-11.5,11.5);
	LadderPlusZ4 = new TH1F(("LadderPlusZ4" + *thisTabNumber).c_str(),"Ring 4, Plus Z",23,-11.5,11.5);
	LadderPlusZ5 = new TH1F(("LadderPlusZ5" + *thisTabNumber).c_str(),"Ring 1, Plus Z",23,-11.5,11.5);
	LadderPlusZ6 = new TH1F(("LadderPlusZ6" + *thisTabNumber).c_str(),"Ring 2, Plus Z",23,-11.5,11.5);
	LadderPlusZ7 = new TH1F(("LadderPlusZ7" + *thisTabNumber).c_str(),"Ring 3, Plus Z",23,-11.5,11.5);
	LadderPlusZ8 = new TH1F(("LadderPlusZ8" + *thisTabNumber).c_str(),"Ring 4, Plus Z",23,-11.5,11.5);

	RatioMinus1 = new TH1F(("RatioMinus1" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioMinus2 = new TH1F(("RatioMinus2" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioMinus3 = new TH1F(("RatioMinus3" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioMinus4 = new TH1F(("RatioMinus4" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);

	RatioPlus1 = new TH1F(("RatioPlus1" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioPlus2 = new TH1F(("RatioPlus2" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioPlus3 = new TH1F(("RatioPlus3" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	RatioPlus4 = new TH1F(("RatioPlus4" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	
	
} 

OccTab::~OccTab(){

	if(LadderMinusZ1 != 0) delete LadderMinusZ1;
	if(LadderMinusZ2 != 0) delete LadderMinusZ2;
	if(LadderMinusZ3 != 0) delete LadderMinusZ3;
	if(LadderMinusZ4 != 0) delete LadderMinusZ4;
	if(LadderMinusZ5 != 0) delete LadderMinusZ5;
	if(LadderMinusZ6 != 0) delete LadderMinusZ6;
	if(LadderMinusZ7 != 0) delete LadderMinusZ7;
	if(LadderMinusZ8 != 0) delete LadderMinusZ8;
	
	if(LadderPlusZ1 != 0) delete LadderPlusZ1;
	if(LadderPlusZ2 != 0) delete LadderPlusZ2;
	if(LadderPlusZ3 != 0) delete LadderPlusZ3;
	if(LadderPlusZ4 != 0) delete LadderPlusZ4;
	if(LadderPlusZ5 != 0) delete LadderPlusZ5;
	if(LadderPlusZ6 != 0) delete LadderPlusZ6;
	if(LadderPlusZ7 != 0) delete LadderPlusZ7;
	if(LadderPlusZ8 != 0) delete LadderPlusZ8;
	
	if(RatioMinus1 != 0) delete RatioMinus1;
	if(RatioMinus2 != 0) delete RatioMinus2;
	if(RatioMinus3 != 0) delete RatioMinus3;
	if(RatioMinus4 != 0) delete RatioMinus4;
	
	if(RatioPlus1 != 0) delete RatioPlus1;
	if(RatioPlus2 != 0) delete RatioPlus2;
	if(RatioPlus3 != 0) delete RatioPlus3;
	if(RatioPlus4 != 0) delete RatioPlus4;
	
	if(OccupancyStackLayer2 != 0) delete OccupancyStackLayer2;
	}

void OccTab::remoteExecute(){
	executeAnalysis();	
}

void OccTab::executeAnalysis(){

	if(!isCalculated){

	pixelTree->Project(("LadderMinusZ1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-1 && ClType==1");
	pixelTree->Project(("LadderMinusZ2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-2 && ClType==1");
	pixelTree->Project(("LadderMinusZ3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-3 && ClType==1");
	pixelTree->Project(("LadderMinusZ4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-4 && ClType==1");
	
	pixelTree->Project(("LadderPlusZ1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==1 && ClType==1");
	pixelTree->Project(("LadderPlusZ2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==2 && ClType==1");
	pixelTree->Project(("LadderPlusZ3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==3 && ClType==1");
	pixelTree->Project(("LadderPlusZ4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==4 && ClType==1");
	
	pixelTree->Project(("LadderMinusZ5" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-1 ");
	pixelTree->Project(("LadderMinusZ6" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-2 ");
	pixelTree->Project(("LadderMinusZ7" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-3 ");
	pixelTree->Project(("LadderMinusZ8" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-4");
	
	pixelTree->Project(("LadderPlusZ5" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==1 ");
	pixelTree->Project(("LadderPlusZ6" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==2 ");
	pixelTree->Project(("LadderPlusZ7" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==3 ");
	pixelTree->Project(("LadderPlusZ8" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==4 ");
	
	pixelTree->Project(("RatioMinus1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-1 && ClType==1");
	pixelTree->Project(("RatioMinus2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-2 && ClType==1");
	pixelTree->Project(("RatioMinus3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-3 && ClType==1");
	pixelTree->Project(("RatioMinus4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==-4 && ClType==1");
	
	pixelTree->Project(("RatioPlus1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==1 && ClType==1");
	pixelTree->Project(("RatioPlus2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==2 && ClType==1");
	pixelTree->Project(("RatioPlus3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==3 && ClType==1");
	pixelTree->Project(("RatioPlus4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer ==2 && ClModule==4 && ClType==1");
	
	for (int i = 0; i < 37; ++i)
	{
		if (LadderMinusZ5->GetBinContent(i) > 0)	RatioMinus1->SetBinContent(i,LadderMinusZ1->GetBinContent(i)/LadderMinusZ5->GetBinContent(i));
		if (LadderMinusZ6->GetBinContent(i) > 0)	RatioMinus2->SetBinContent(i,LadderMinusZ2->GetBinContent(i)/LadderMinusZ6->GetBinContent(i));
		if (LadderMinusZ7->GetBinContent(i) > 0)	RatioMinus3->SetBinContent(i,LadderMinusZ3->GetBinContent(i)/LadderMinusZ7->GetBinContent(i));
		if (LadderMinusZ8->GetBinContent(i) > 0)	RatioMinus4->SetBinContent(i,LadderMinusZ4->GetBinContent(i)/LadderMinusZ8->GetBinContent(i));
		if (LadderPlusZ5->GetBinContent(i) > 0)		RatioPlus1->SetBinContent(i,LadderPlusZ1->GetBinContent(i)/LadderPlusZ5->GetBinContent(i));
		if (LadderPlusZ6->GetBinContent(i) > 0)		RatioPlus2->SetBinContent(i,LadderPlusZ2->GetBinContent(i)/LadderPlusZ6->GetBinContent(i));
		if (LadderPlusZ7->GetBinContent(i) > 0)		RatioPlus3->SetBinContent(i,LadderPlusZ3->GetBinContent(i)/LadderPlusZ7->GetBinContent(i));
		if (LadderPlusZ8->GetBinContent(i) > 0)		RatioPlus4->SetBinContent(i,LadderPlusZ4->GetBinContent(i)/LadderPlusZ8->GetBinContent(i));
    }
	


	RatioMinus1->SetLineColor(2);
	RatioMinus2->SetLineColor(3);
	RatioMinus3->SetLineColor(4);
	RatioMinus4->SetLineColor(6);
	RatioPlus1->SetLineColor(2);
	RatioPlus2->SetLineColor(3);
	RatioPlus3->SetLineColor(4);
	RatioPlus4->SetLineColor(6);
	
	RatioPlus1->SetLineStyle(2);
	RatioPlus2->SetLineStyle(2);
	RatioPlus3->SetLineStyle(2);
	RatioPlus4->SetLineStyle(2);
	
	
}
		isCalculated = true;
		
		OccupancyStackLayer2 = new THStack(("OccupancyStackLayer2"+ *thisTabNumber).c_str(),"Cluster Occupancy - Layer 2 Ratio of Tracked/All");
	
	OccupancyStackLayer2->Add(RatioMinus1);
	OccupancyStackLayer2->Add(RatioMinus2);
	OccupancyStackLayer2->Add(RatioMinus3);
	OccupancyStackLayer2->Add(RatioMinus4);
	OccupancyStackLayer2->Add(RatioPlus1);
	OccupancyStackLayer2->Add(RatioPlus2);
	OccupancyStackLayer2->Add(RatioPlus3);
	OccupancyStackLayer2->Add(RatioPlus4);
	
	OccupancyStackLayer2->Draw("nostack");
	fCanvas->Update();
	
}

TCanvas* OccTab::getCanvas(){
	return fCanvas;
}

void OccTab::activateCanvas(){
	
	fCanvas->cd();
}

#endif
