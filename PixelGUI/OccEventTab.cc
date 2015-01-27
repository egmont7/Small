#ifndef __OccEventTab_cc
#define __OccEventTab_cc

/*  OccEventTab.cc
 *  	Author: Austin Deck
 * 		contact: austin@huskers.unl.edu
 * 		date: July 28, 2010
 */

#include "OccEventTab.h"    //include the header file for this class.

using namespace std;

ClassImp(OccEventTab);

OccEventTab::OccEventTab() : PixelTab(){}

OccEventTab::OccEventTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :PixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);

	TGLabel  *description = new TGLabel(this,"Fill histogram with occupancy per event for each module.");
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	calcButton = new TGTextButton(this,"Execute Analysis");
	calcButton->Connect("Clicked()","OccEventTab",this,"executeAnalysis()");
	
	AddFrame(description);
	AddFrame(calcButton, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));

	isCalculated = false;
	
	LadderMinusz1 = new TH1F(("LadderMinusz1" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	LadderMinusz2 = new TH1F(("LadderMinusz2" + *thisTabNumber).c_str(),"Ring 2, Minus Z",23,-11.5,11.5);
	LadderMinusz3 = new TH1F(("LadderMinusz3" + *thisTabNumber).c_str(),"Ring 3, Minus Z",23,-11.5,11.5);
	LadderMinusz4 = new TH1F(("LadderMinusz4" + *thisTabNumber).c_str(),"Ring 4, Minus Z",23,-11.5,11.5);

	LadderPlusz1 = new TH1F(("LadderPlusz1" + *thisTabNumber).c_str(),"Ring 1, Plus Z",23,-11.5,11.5);
	LadderPlusz2 = new TH1F(("LadderPlusz2" + *thisTabNumber).c_str(),"Ring 2, Plus Z",23,-11.5,11.5);
	LadderPlusz3 = new TH1F(("LadderPlusz3" + *thisTabNumber).c_str(),"Ring 3, Plus Z",23,-11.5,11.5);
	LadderPlusz4 = new TH1F(("LadderPlusz4" + *thisTabNumber).c_str(),"Ring 4, Plus Z",23,-11.5,11.5);

	LadderOccMinusz1 = new TH1F(("LadderOccMinusz1" + *thisTabNumber).c_str(),"Ring 1, Minus Z",23,-11.5,11.5);
	LadderOccMinusz2 = new TH1F(("LadderOccMinusz2" + *thisTabNumber).c_str(),"Ring 2, Minus Z",23,-11.5,11.5);
	LadderOccMinusz3 = new TH1F(("LadderOccMinusz3" + *thisTabNumber).c_str(),"Ring 3, Minus Z",23,-11.5,11.5);
	LadderOccMinusz4 = new TH1F(("LadderOccMinusz4" + *thisTabNumber).c_str(),"Ring 4, Minus Z",23,-11.5,11.5);

	LadderOccPlusz1 = new TH1F(("LadderOccPlusz1" + *thisTabNumber).c_str(),"Ring 1, Plus Z",23,-11.5,11.5);
	LadderOccPlusz2 = new TH1F(("LadderOccPlusz2" + *thisTabNumber).c_str(),"Ring 2, Plus Z",23,-11.5,11.5);
	LadderOccPlusz3 = new TH1F(("LadderOccPlusz3" + *thisTabNumber).c_str(),"Ring 3, Plus Z",23,-11.5,11.5);
	LadderOccPlusz4 = new TH1F(("LadderOccPlusz4" + *thisTabNumber).c_str(),"Ring 4, Plus Z",23,-11.5,11.5);
	
	Total = new TH1F(("Total" + *thisTabNumber).c_str(),"Total Entries",23,-11.5,11.5);
	
} 

OccEventTab::~OccEventTab(){

	if(LadderMinusz1 != 0) delete LadderMinusz1;
	if(LadderMinusz2 != 0) delete LadderMinusz2;
	if(LadderMinusz3 != 0) delete LadderMinusz3;
	if(LadderMinusz4 != 0) delete LadderMinusz4;
	
	if(LadderPlusz1 != 0) delete LadderPlusz1;
	if(LadderPlusz2 != 0) delete LadderPlusz2;
	if(LadderPlusz3 != 0) delete LadderPlusz3;
	if(LadderPlusz4 != 0) delete LadderPlusz4;
	
	if(LadderOccMinusz1 != 0) delete LadderOccMinusz1;
	if(LadderOccMinusz2 != 0) delete LadderOccMinusz2;
	if(LadderOccMinusz3 != 0) delete LadderOccMinusz3;
	if(LadderOccMinusz4 != 0) delete LadderOccMinusz4;

	if(LadderOccPlusz1 != 0) delete LadderOccPlusz1;
	if(LadderOccPlusz1 != 0) delete LadderOccPlusz2;
	if(LadderOccPlusz1 != 0) delete LadderOccPlusz3;
	if(LadderOccPlusz1 != 0) delete LadderOccPlusz4;
	
	if(OccupancyStackLayer1 != 0) delete OccupancyStackLayer1;
	if(Total != 0) delete Total;
}

void OccEventTab::remoteExecute(){
	calcButton->Clicked();
}

void OccEventTab::executeAnalysis(){

	if(!isCalculated){
		
		pixelTree->Project(("LadderMinusz1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == -1 ");
		pixelTree->Project(("LadderMinusz2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == -2 ");
		pixelTree->Project(("LadderMinusz3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == -3 ");
		pixelTree->Project(("LadderMinusz4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == -4 ");
		pixelTree->Project(("LadderPlusz1" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == 1 ");
		pixelTree->Project(("LadderPlusz2" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == 2 ");
		pixelTree->Project(("LadderPlusz3" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == 3 ");
		pixelTree->Project(("LadderPlusz4" + *thisTabNumber).c_str(),"ClLadder","ClLadder > -30 && ClLayer == 1 && ClModule == 4 ");
		pixelTree->Project(("Total" + *thisTabNumber).c_str(),"ClLadder" ,"ClLadder > -30");

		double Num = Total->GetEntries();

		for (int i = 2; i < 23; i++)
		{
			LadderOccMinusz1->SetBinContent(i,LadderMinusz1->GetBinContent(i)/Num);
			LadderOccMinusz2->SetBinContent(i,LadderMinusz2->GetBinContent(i)/Num);
			LadderOccMinusz3->SetBinContent(i,LadderMinusz3->GetBinContent(i)/Num);
			LadderOccMinusz4->SetBinContent(i,LadderMinusz4->GetBinContent(i)/Num);
			LadderOccPlusz1->SetBinContent(i,LadderPlusz1->GetBinContent(i)/Num);
			LadderOccPlusz2->SetBinContent(i,LadderPlusz2->GetBinContent(i)/Num);
			LadderOccPlusz3->SetBinContent(i,LadderPlusz3->GetBinContent(i)/Num);
			LadderOccPlusz4->SetBinContent(i,LadderPlusz4->GetBinContent(i)/Num);
		}


		LadderOccMinusz1->SetLineColor(2);
		LadderOccMinusz2->SetLineColor(3);
		LadderOccMinusz3->SetLineColor(4);
		LadderOccMinusz4->SetLineColor(6);
		LadderOccPlusz1->SetLineColor(2);
		LadderOccPlusz2->SetLineColor(3);
		LadderOccPlusz3->SetLineColor(4);
		LadderOccPlusz4->SetLineColor(6);

		LadderOccPlusz1->SetLineStyle(2);
		LadderOccPlusz2->SetLineStyle(2);
		LadderOccPlusz3->SetLineStyle(2);
		LadderOccPlusz4->SetLineStyle(2);
	}
	isCalculated = true;
	
	OccupancyStackLayer1 = new THStack(("OccupancyStackLayer1" + *thisTabNumber).c_str(),"Cluster Occupancy - Layer 1 Occupancy");
	
	OccupancyStackLayer1->Add(LadderOccMinusz1);
	OccupancyStackLayer1->Add(LadderOccMinusz2);
	OccupancyStackLayer1->Add(LadderOccMinusz3);
	OccupancyStackLayer1->Add(LadderOccMinusz4);
	OccupancyStackLayer1->Add(LadderOccPlusz1);
	OccupancyStackLayer1->Add(LadderOccPlusz2);
	OccupancyStackLayer1->Add(LadderOccPlusz3);
	OccupancyStackLayer1->Add(LadderOccPlusz4);
	
	OccupancyStackLayer1->Draw("nostack");
	fCanvas->Update();
	

}

TCanvas* OccEventTab::getCanvas(){
	return fCanvas;
}

void OccEventTab::activateCanvas(){
	
	fCanvas->cd();
}

#endif
