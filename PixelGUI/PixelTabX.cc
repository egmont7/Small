#ifndef __PixelTabX_cc
#define __PixelTabX_cc

/*  PixelTabX.h
 *	  Author: Caleb Fangmeier
 *	  contact: cfangmeier@hotmail.com
 *	  date: June 24, 2010
 *	  
 *	  This is a generic class outline for a potential tab to be used
 *	  with PixelGUI. It inherites from PixelTab which gives it the 
 *	  basic functionality it needs to interface with the main GUI.
 *	  To use this class you need simply rename it
 *	  to something appropriate and fill in the necessary methods with the 
 *	  requisite functionality outlined in the method implementations.
 *	  You need to make the following changes to PixelGUI
 *	  
 *		  1)modify numTabs in PixelGUI.h to the correct number of tabs
 *			you want in the GUI.
 *		  2)Include PixelTabX.h(or whatever your header file is called)
 *			in PixelGUI.h
 *		  3)in PixelGUI.cc you need to create in instance of your class
 *			following the pattern seen there in the buildTabs() method.
 *	  
 *	  Finally you will need to add your new class to the Makefile and
 *	  to PixelGUI_Linkdef.h following the pattern set up there. 
 * 
 *	  To compile your code in BASH simply type
 *	  $ make
 * 
 *	  To run the GUI type
 *	  $ ./PixelGUI
 * 
 */

#include "PixelTabX.h"	//include the header file for this class.

using namespace std;

ClassImp(PixelTabX); // < ---- Necessary Detail for Class Inheritance;

PixelTabX::PixelTabX() : anaPixelTab(){} // <--Default constructor
									  //   (Do not change!)

PixelTabX::PixelTabX(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
		 :anaPixelTab(p,tree, kVerticalFrame){
	//Build the GUI here and initialize any class Data members that must
	//be initialized prior to analysis execution. This includes booking 
	//any histograms 
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str(),this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	execute = new TGTextButton(this,"Execute Analysis");
	execute->Connect("Clicked()","PixelTabX",this,"executeAnalysis()");
	
	AddFrame(execute,new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
	
	ChargeTracksL1 = new TH1F(("ChargeTracksL1" + *thisTabNumber).c_str(),"Charge deposited on tracks",280,0,280);
	ChargeNoTracksL1 = new TH1F(("ChargeNoTracksL1" + *thisTabNumber).c_str(),"Charge deposited off tracks",280,0,280);
	
	calculated = false;
} 

PixelTabX::~PixelTabX(){}//dtor


void PixelTabX::executeAnalysis(){
//Here is where you actually run your analysis routines. 
//Normal practice here is to connect a button in your GUI to this method.
//It is reccomended that you implement a control structure that will have
//each analysis run only once and upon further button presses it simply
//draw the Graph. It would probably be good practice to call Loop from here
	if(!calculated){
		Loop();
		calculated = true;
	}
	paintCanvas();
}

void PixelTabX::Loop(int nevt){
	
	
	if (fChain == 0) return;
	
	Long64_t nentries = fChain->GetEntries();
	
	int step(100000), maxEvents(nentries); 
	if (nevt > 0 && nevt < nentries) maxEvents = nevt; 
	if (maxEvents < 1000000) step = 50000; 
	if (maxEvents < 100000)  step = 5000; 
	if (maxEvents < 10000)   step = 500; 
	if (maxEvents < 1000)	step = 100; 
	
	Long64_t nbytes = 0, nb = 0;
	
	for (Long64_t jentry=0; jentry<nentries;jentry++) {
		
		if ((nevt > -1) && (jentry > nevt)) break;
		Long64_t ientry = LoadTree(jentry);
		if (ientry < 0) break;
		nb = fChain->GetEntry(jentry);   nbytes += nb;
		if (Cut(ientry) < 0) continue;
////////////////////////////////////////////////////////////////////////
		//Put analysis for each event here. This example fills two 
		//histograms with Cluster Charge for tracked and untracked hits.
		//(this is very similar to what is done in PixelTabChargeDistribution)
		
		for (int ic = 0; ic < ClN; ++ic) {//each event has ClN number of Clusters
										  //This loops over each one
			
			if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 1){
				ChargeTracksL1->Fill(ClCharge[ic]);
			}
			
			if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 2){
				ChargeNoTracksL1->Fill(ClCharge[ic]);
			}
		}
		
////////////////////////////////////////////////////////////////////////
	}
	
	//format histograms
	ChargeNoTracksL1->SetLineColor(2);
	ChargeNoTracksL1->SetLineStyle(2);
	ChargeTracksL1->SetTitle("Cluster Charge - Layer 1 - Red is Type 2 (w/o track)");
	ChargeTracksL1->GetXaxis()->SetTitle("Cluster Charge (keV)");
	ChargeTracksL1->GetYaxis()->SetTitle("Number of Events");
	stack1 = new THStack();
	stack1->Add(ChargeTracksL1);
	stack1->Add(ChargeNoTracksL1);

}

void PixelTabX::paintCanvas(){
	stack1->Draw("nostack");
	fCanvas->Update();
}

TCanvas* PixelTabX::getCanvas(){
	//here you want to return the Canvas that you would like to be 
	//saved via the file menu. You can have multiple canvases but you
	//need to allow the user to somehow pick which one they want saved.
	return fCanvas;
}

void PixelTabX::activateCanvas(){
	//This is called whenever the user switches tabs to this one. Again,
	//if you have multiple canvases you will need to add some way to switch
	//to the canvas that you want.
	fCanvas->cd();
}
#endif
