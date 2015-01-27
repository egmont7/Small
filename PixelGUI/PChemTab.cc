#ifndef __PChemTab_cc
#define __PChemTab_cc

/*  PChemTab.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier74@gmail.com
 * 		date: May 24, 2011
 * 		
 * 		
 * 
 */

#include "PChemTab.h"

using namespace std;

ClassImp(PChemTab); 

PChemTab::PChemTab() : anaPixelTab(){} 

PChemTab::PChemTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	
	
	thisTabNumber = new string(tabNumber);
	gStyle->SetOptStat("iouen");
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str()
	                                                       ,this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	
	TGGroupFrame *macroFrame = new TGGroupFrame(this,"Pixel Efficiency Analysis",kHorizontalFrame);
	execute = new TGTextButton(macroFrame,"Execute");
	execute->Connect("Clicked()","PChemTab",this,"executeAnalysis()");
	execute2= new TGTextButton(macroFrame,"Execute 2");
	execute2->Connect("Clicked()","PChemTab",this,"Loop2()");
	saveAll = new TGTextButton(macroFrame,"Save All Plots");
	saveAll->Connect("Clicked()","PChemTab",this,"saveAllPlots()");
	prefixField = new TGTextEntry(macroFrame,"enter Prefix");
	
	TGCanvas *fCan = new TGCanvas(macroFrame, 100, 100, kFixedSize);
	macroFrame->AddFrame(fCan, new TGLayoutHints(kLHintsExpandY | kLHintsExpandX));
	
	TGVButtonGroup *bGroup = new TGVButtonGroup(fCan->GetViewPort(),"Plot Type");
	fCan->SetContainer(bGroup);
	evenPixB    = new TGRadioButton(bGroup,"Even Columns - Hits");
	oddPixB     = new TGRadioButton(bGroup,"Odd Columns - Hits");
	evenPixTksB = new TGRadioButton(bGroup,"Even Columns - Tracks");
	oddPixTksB  = new TGRadioButton(bGroup,"Odd Columns - Tracks");
	
	ClXB             = new TGRadioButton(bGroup,"ClRhX");
	ClYB             = new TGRadioButton(bGroup,"ClRhY");
	TkXResB          = new TGRadioButton(bGroup,"TkXResidual");
	TkYResB          = new TGRadioButton(bGroup,"TkYResidual");
	overLocB         = new TGRadioButton(bGroup,"over/under Locs");
	colRowOverlayEB   = new TGRadioButton(bGroup,"Even Columns - col/row");
	colRowOverlayETkB = new TGRadioButton(bGroup,"Even Columns - col/row Tks");
	colRowOverlayOB   = new TGRadioButton(bGroup,"Odd Columns - col/row");
	colRowOverlayOTkB = new TGRadioButton(bGroup,"Odd Columns - col/row Tks");
	evenPixB->SetState(kButtonDown);
	
	macroFrame->AddFrame(execute2, new TGLayoutHints(kLHintsLeft | kLHintsCenterY));
	macroFrame->AddFrame(execute, new TGLayoutHints(kLHintsLeft | kLHintsCenterY));
	macroFrame->AddFrame(saveAll, new TGLayoutHints(kLHintsLeft | kLHintsCenterY));
	macroFrame->AddFrame(prefixField, new TGLayoutHints(kLHintsLeft | kLHintsCenterY));
	macroFrame->AddFrame(fCan , new TGLayoutHints(kLHintsCenterY | kLHintsExpandX));
	
	AddFrame(macroFrame,new TGLayoutHints(kLHintsExpandX | kLHintsBottom));
	
	evenPixP = new TH2D(("evenPixP" + *thisTabNumber).c_str(),"evenPixP",
	                             20,0,0.01,30,0,0.015);
	
	oddPixP = new TH2D(("oddPixP" + *thisTabNumber).c_str(),"oddPixP",
	                             20,0,0.01,30,0,0.015);
	
	evenPixTksP = new TH2D(("evenPixTksP" + *thisTabNumber).c_str(),"evenPixTksP",
	                             20,0,0.01,30,0,0.015);
	
	oddPixTksP = new TH2D(("oddPixTksP" + *thisTabNumber).c_str(),"oddPixTksP",
	                             20,0,0.01,30,0,0.015);
	
	ClXP = new TH1D(("ClXP" + *thisTabNumber).c_str(),"ClXP",
	                             100,-0.85,0.85);
	
	ClYP = new TH1D(("ClYP" + *thisTabNumber).c_str(),"ClYP",
	                             100,-3.5,3.5);
	
	TkXResP = new TH1D(("TkXResP" + *thisTabNumber).c_str(),"TkXResP",
	                             100,-0.05,0.05);
	
	TkYResP = new TH1D(("TkYResP" + *thisTabNumber).c_str(),"TkYResP",
	                             100,-0.05,0.05);
	
	overLocP = new TH2D(("overLocP" + *thisTabNumber).c_str(),"overLocP",
	                             161,0,160,416,0,417);
	
	colRowOverlayEP = new TH2D(("colRowOverlayEP" + *thisTabNumber).c_str(),"colRowOverlayEP",
	                             20,0,0.01,30,0,0.015);
	
	colRowOverlayETkP = new TH2D(("colRowOverlayETkP" + *thisTabNumber).c_str(),"colRowOverlayETkP",
	                             20,0,0.01,30,0,0.015);
	
	colRowOverlayOP = new TH2D(("colRowOverlayOP" + *thisTabNumber).c_str(),"colRowOverlayOP",
	                             20,0,0.01,30,0,0.015);
	
	colRowOverlayOTkP = new TH2D(("colRowOverlayOTkP" + *thisTabNumber).c_str(),"colRowOverlayOTkP",
	                             20,0,0.01,30,0,0.015);
	
	
	rowsE.resize(10);
	colsE.resize(10);
	rowsO.resize(10);
	colsO.resize(10);
	
	for (int i = 0; i < 10; i++)
	{
		rowsE[i] = 4 + 2*i;
		colsE[i] = 4 + 2*i;
		
		rowsO[i] = 3 + 2*i;
		colsO[i] = 3 + 2*i;
	}
	
	
	for (int i = 0; i < 10; i++)
	{
		cout << "booking singlepix Histograms" << endl;
		
		stringstream ss(stringstream::in | stringstream::out);
		ss << "singlePixRow" << rowsO[i] << "Col" << colsO[i];
		singlePixO[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixRow" << rowsE[i] << "Col" << colsE[i];
		singlePixE[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixTksRow" << rowsO[i] << "Col" << colsO[i];
		singlePixOTks[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixTksRow" << rowsE[i] << "Col" << colsE[i];
		singlePixETks[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		
		ss.str("");
		ss << "singlePixAllRow" << rowsO[i] << "Col" << colsO[i];
		singlePixOAll[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixAllRow" << rowsE[i] << "Col" << colsE[i];
		singlePixEAll[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixTksAllRow" << rowsO[i] << "Col" << colsO[i];
		singlePixOTksAll[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
		
		ss.str("");
		ss << "singlePixTksAllRow" << rowsE[i] << "Col" << colsE[i];
		singlePixETksAll[i] = new TH2F(ss.str().c_str(),ss.str().c_str(),
		                         500,-0.85,0.85,500,-3.5,3.5);
	}
	
	
	
	calculated = false;
	
} 

PChemTab::~PChemTab(){
	delete evenPixP; 
	delete oddPixP;
	delete evenPixTksP;
	delete oddPixTksP;
	delete ClXP; 
	delete ClYP;
	delete TkXResP; 
	delete TkYResP;
	delete overLocP;
	delete colRowOverlayEP;
	delete colRowOverlayETkP;
	delete colRowOverlayOP;
	delete colRowOverlayOTkP;
	
	for (int i = 0; i < 10; i++)
	{
		delete singlePixO[i];
		delete singlePixE[i];
		
		delete singlePixOTks[i];
		delete singlePixETks[i];
		
		delete singlePixOAll[i];
		delete singlePixEAll[i];
		
		delete singlePixOTksAll[i];
		delete singlePixETksAll[i];
	}
	
	
}

void PChemTab::executeAnalysis(){
	
	if(!calculated){
		Loop(-1);
		calculated = true;
	}
	
	if (evenPixB->IsDown()){
		evenPixP->Draw("colz");
	}
	else if(oddPixB->IsDown()){
		oddPixP->Draw("colz");
	}
	else if(evenPixTksB->IsDown()){
		evenPixTksP->Draw("colz");
	}
	else if(oddPixTksB->IsDown()){
		oddPixTksP->Draw("colz");
	}
	else if (ClXB->IsDown()){
		ClXP->Draw();
	}
	else if (ClYB->IsDown()){
		ClYP->Draw();
	}
	else if (TkXResB->IsDown()){
		TkXResP->Draw();
	}
	else if (TkYResB->IsDown()){
		TkYResP->Draw();
	}
	else if (overLocB->IsDown())
	{
		overLocP->Draw("colz");
	}
	else if (colRowOverlayEB->IsDown())
	{
		colRowOverlayEP->Draw("colz");
	}
	else if (colRowOverlayETkB->IsDown())
	{
		colRowOverlayETkP->Draw("colz");
	}
	else if (colRowOverlayOB->IsDown())
	{
		colRowOverlayOP->Draw("colz");
	}
	else if (colRowOverlayOTkB->IsDown())
	{
		colRowOverlayOTkP->Draw("colz");
	}
	fCanvas->Update();
	
}

void PChemTab::saveAllPlots(){
	
	stringstream filename (stringstream::in | stringstream::out);
	filename << prefixField->GetText() << ".root" ;
	TFile f(filename.str().c_str(),"recreate");
	
	evenPixP->Write(); 
	oddPixP->Write();
	evenPixTksP->Write();
	oddPixTksP->Write();
	ClXP->Write(); 
	ClYP->Write();
	TkXResP->Write(); 
	TkYResP->Write();
	overLocP->Write();
	colRowOverlayEP->Write();
	colRowOverlayETkP->Write();
	colRowOverlayOP->Write();
	colRowOverlayOTkP->Write();
}

void PChemTab::Loop2(){
	int nevt = -1;
	time_t timeStart = time(0);
	
	int hitN = 0;
	
	//Enable only branches used for faster operation
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("ClN",1);
	//fChain->SetBranchStatus("ClDgN",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClModule",1);
	fChain->SetBranchStatus("ClTkN",1);
	fChain->SetBranchStatus("ClTkI",1);
	fChain->SetBranchStatus("ClRhLx",1);
	fChain->SetBranchStatus("ClRhLy",1);
	fChain->SetBranchStatus("ClCol",1);
	fChain->SetBranchStatus("ClRow",1);
	fChain->SetBranchStatus("ClSizeX",1);
	fChain->SetBranchStatus("ClSizeY",1);
	
	
	//~ fChain->SetBranchStatus("TkEta",1);
	fChain->SetBranchStatus("TkResX",1);
	fChain->SetBranchStatus("TkResY",1);
	fChain->SetBranchStatus("TkClN",1);
	fChain->SetBranchStatus("TkClI",1);
	fChain->SetBranchStatus("TkPt" ,1);
	
	
	
	if (fChain == 0) return;
	
	Long64_t nentries = fChain->GetEntries();
	
	int step(100000), maxEvents(nentries); 
	if (nevt > 0 && nevt < nentries) maxEvents = nevt; 
	if (maxEvents < 1000000) step = 50000; 
	if (maxEvents < 100000)  step = 5000; 
	if (maxEvents < 10000)   step = 500; 
	if (maxEvents < 1000)    step = 100; 
	
	Long64_t nbytes = 0, nb = 0;
	
	cout << "Will process " << nentries << " Events" << endl;
	
	for (Long64_t jentry=0; jentry<nentries;jentry++) {
		
		if ((nevt > -1) && (jentry > nevt)) break;
		Long64_t ientry = LoadTree(jentry);
		if (ientry < 0) break;
		nb = fChain->GetEntry(jentry);   nbytes += nb;
		if (Cut(ientry) < 0) continue;
////////////////////////////////////////////////////////////////////////
		
		if (!(jentry % 2000))
		{
			cout << "finished: " << (jentry/float(nentries)) * 100 << "%            ";
			
			time_t timeCurrent = time(0);
			cout << ((timeCurrent - timeStart) * (nentries-jentry))/(float(60*jentry))  ;
			cout << " minutes left" << endl; 
			
		}
		
		for (int ic = 0; ic < ClN; ++ic){
			for (int tk = 0; tk < 1; ++tk){
				
				//Layer 1
				if(ClLayer[ic] != 1)         continue;
				//Ladder 2
				if(ClLadder[ic] != 2) continue;
				//Module 1
				if(ClModule[ic] != 1)   continue;
				//Track Pt at least 5GeV
				if(TkPt[ClTkI[ic][tk]] < 5)  continue;
				
				
				///////////END CUTS/////////////////////////////////////
				
				++hitN;
				//Find track's index for current cluster
				//This is backwards but must be done.
				int clustI = -1;
				for (int j = 0;(clustI == -1) && j < TkClN[ClTkI[ic][tk]]; j++){
					
					if (ic == TkClI[ClTkI[ic][tk]][j]){
						clustI = j;
					}
				}
				
				int row = floor(ClRow[ic]);
				int col = floor(ClCol[ic]);
				
				double TkRhLx = ClRhLx[ic] + TkResX[ClTkI[ic][tk]][clustI];
				double TkRhLy = ClRhLy[ic] + TkResY[ClTkI[ic][tk]][clustI];
				if (row == col)
				{
					cout << "row " << row << " col " << col << endl;
				}
				
				
				for (int i = 0; i < 10; i++)
				{
					if (row == rowsE[i] && col == colsE[i]){
						cout << "Filling Even " << i;
						singlePixEAll[i]->Fill(ClRhLx[ic],ClRhLy[ic]);
						singlePixETksAll[i]->Fill(TkRhLx,TkRhLy);
						
						if (ClSizeX[ic] == 1 || ClSizeY[ic] == 1)
							break;
						singlePixE[i]->Fill(ClRhLx[ic],ClRhLy[ic]);
						singlePixETks[i]->Fill(TkRhLx,TkRhLy);
						break;
						
					}
					else if (row == rowsO[i] && col == colsO[i]){
						cout << "Filling Odd " << i;
						singlePixOAll[i]->Fill(ClRhLx[ic],ClRhLy[ic]);
						singlePixOTksAll[i]->Fill(TkRhLx,TkRhLy);
						
						if (ClSizeX[ic] == 1 || ClSizeY[ic] == 1)
							break;
						singlePixO[i]->Fill(ClRhLx[ic],ClRhLy[ic]);
						singlePixOTks[i]->Fill(TkRhLx,TkRhLy);
						break;
						
						
					}
				}
				
			}
		}
////////////////////////////////////////////////////////////////////////
	}
	
	stringstream filename (stringstream::in | stringstream::out);
	filename << "singlepix_" << timeStart << ".root" ;
	TFile f(filename.str().c_str(),"recreate");
	
	for (int i = 0; i < 10; i++)
	{
		singlePixOAll[i]->Write();
		singlePixOTksAll[i]->Write();
		
		singlePixO[i]->Write();
		singlePixOTks[i]->Write();
		
		singlePixEAll[i]->Write();
		singlePixETksAll[i]->Write();
		
		singlePixE[i]->Write();
		singlePixETks[i]->Write();
		
	}
	
	cout << "finished!!" << endl;
	time_t timeCurrent = time(0);
	cout << "processed " << nentries << " events in " ;
	cout << (timeCurrent - timeStart)/ 60.0 << " minutes." << endl;
	cout << hitN << " Tracks passed cuts" << endl;
}


void PChemTab::Loop(int nevt){
	
	time_t timeStart = time(0);
	
	int hitN = 0;
	double pixSizeX = 100*pow(10,-4);//cm
	double pixSizeY = 150*pow(10,-4);//cm
	double pixSizeXE = 0; //cm
	double pixSizeYE = 0; //cm
	
	
	//Enable only branches used for faster operation
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("ClN",1);
	//fChain->SetBranchStatus("ClDgN",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClModule",1);
	fChain->SetBranchStatus("ClTkN",1);
	fChain->SetBranchStatus("ClTkI",1);
	fChain->SetBranchStatus("ClRhLx",1);
	fChain->SetBranchStatus("ClRhLy",1);
	fChain->SetBranchStatus("ClCol",1);
	fChain->SetBranchStatus("ClRow",1);
	fChain->SetBranchStatus("ClSizeX",1);
	fChain->SetBranchStatus("ClSizeY",1);
	fChain->SetBranchStatus("ClRhSpansTwoROCs",1);
	fChain->SetBranchStatus("ClRhIsOnEdge",1);
	fChain->SetBranchStatus("ClRhHasBadPixels",1);
	
	
	//~ fChain->SetBranchStatus("TkEta",1);
	fChain->SetBranchStatus("TkResX",1);
	fChain->SetBranchStatus("TkResY",1);
	fChain->SetBranchStatus("TkClN",1);
	fChain->SetBranchStatus("TkClI",1);
	fChain->SetBranchStatus("TkPt" ,1);
	
	//~ fChain->SetBranchStatus("ClRhLxE" ,1);
	//~ fChain->SetBranchStatus("ClRhLyE" ,1);
	//~ fChain->SetBranchStatus("TkResXe" ,1);
	//~ fChain->SetBranchStatus("TkResYe" ,1);
	
	
	
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
		
		if (!(jentry % 2000))
		{
			cout << "finished: " << (jentry/float(nentries)) * 100 << "%            ";
			
			time_t timeCurrent = time(0);
			cout << ((timeCurrent - timeStart) * (nentries-jentry))/(float(60*jentry))  ;
			cout << " minutes left" << endl; 
			
		}
		
		for (int ic = 0; ic < ClN; ++ic){
			for (int tk = 0; tk < ClTkN[ic]; ++tk){
				
				//Track Pt at least 5GeV
				if(TkPt[ClTkI[ic][tk]] < 5)  continue;
				//Layer 1
				if(ClLayer[ic] != 1)         continue;
				//Module +/-1
				if(abs(ClModule[ic]) != 1)   continue;
				
				//Non-flipped Layers (+ and odd or - and even)
				//~ if (ClLadder[ic] < 0){
					//~ if ((ClLadder[ic] % 2)) continue;
				//~ }
				//~ else if (ClLadder[ic] > 0){
					//~ if (!(ClLadder[ic] % 2)) continue;
				//~ }
				
				
				
				//exclude half modules
				if (abs(ClLadder[ic]) == 1 || abs(ClLadder[ic]) == 10)
					continue;
				
				
				
				//Quality cuts
				if(ClRhSpansTwoROCs[ic])         continue;
				if(ClRhIsOnEdge[ic])             continue;
				if(ClRhHasBadPixels[ic])         continue;
				
				
				
				//~ if(ClRow[ic] >= 78 && ClRow[ic] <= 1)         continue;
				//~ if(ClCol[ic] >= 50 && ClCol[ic] <= 1)         continue;
				//if (ClDgN[ic] != 1)        continue;
				
				if (ClRow[ic] >= 80)         continue;//Restrict to "left" side of module.
				//if (ClCol[ic] >= 52)         continue;//Restrict to ROC 0
				
				//Ignore 0 or 1 dimensional clusters.
				if (ClSizeX[ic] == 1)        continue;
				if (ClSizeY[ic] == 1)        continue;
				
				//Ignore Edge Pixels because they are oversized
				if ((int(ClRow[ic]) % 80) == 0 || ((int(ClRow[ic]) - 1) % 80) == 0) continue;
				if ((int(ClCol[ic]) % 52) == 0 || ((int(ClCol[ic]) - 1) % 52) == 0) continue;
				
				///////////END CUTS/////////////////////////////////////
				
				++hitN;
				//Find track's index for current cluster
				//This is backwards but must be done.
				int clustI = -1;
				for (int j = 0;(clustI == -1) && j < TkClN[ClTkI[ic][tk]]; j++){
					
					if (ic == TkClI[ClTkI[ic][tk]][j]){
						clustI = j;
					}
				}
				
				ClXP->Fill(ClRhLx[ic]);
				ClYP->Fill(ClRhLy[ic]);
				TkXResP->Fill(TkResX[ClTkI[ic][tk]][clustI]);
				TkYResP->Fill(TkResY[ClTkI[ic][tk]][clustI]);
				
				//compensate for double wide edge pixels between ROCs
				double ROCShiftX = (int(floor(ClRow[ic])) / 80) * 2*pixSizeX;
				double ROCShiftY = (int(floor(ClCol[ic])) / 52) * 2*pixSizeY;
				
				double shiftX = 0.8155 - floor(ClRow[ic]) * pixSizeX - ROCShiftX;//?
				double shiftY = 3.2246 - floor(ClCol[ic]) * pixSizeY - ROCShiftY;//?
				
				double HtX = ClRhLx[ic] + shiftX;
				double HtY = ClRhLy[ic] + shiftY;
				
				double TkX = HtX + TkResX[ClTkI[ic][tk]][clustI];
				double TkY = HtY + TkResY[ClTkI[ic][tk]][clustI];
				
				if (int(ClCol[ic]) % 2 == 0){
					evenPixP->Fill(HtX,HtY);
					evenPixTksP->Fill(TkX,TkY);
					
					double RowX = (ClRow[ic] - floor(ClRow[ic]))*pixSizeX;
					double ColY = (ClCol[ic] - floor(ClCol[ic]))*pixSizeY;
					
					colRowOverlayEP->Fill(RowX,ColY);
					
					RowX += TkResX[ClTkI[ic][tk]][clustI];
					ColY += TkResY[ClTkI[ic][tk]][clustI];
					
					//wrap tracks around
					if (RowX > pixSizeX) RowX -= pixSizeX;
					else if (RowX < 0)   RowX += pixSizeX;
					if (ColY > pixSizeY) ColY -= pixSizeY;
					else if (ColY < 0)   ColY += pixSizeY;
					
					if (RowX > pixSizeX) RowX -= pixSizeX;
					else if (RowX < 0)   RowX += pixSizeX;
					if (ColY > pixSizeY) ColY -= pixSizeY;
					else if (ColY < 0)   ColY += pixSizeY;
					
					colRowOverlayETkP->Fill(RowX,
					                       ColY);
				}
				else{
					oddPixP->Fill(HtX,HtY);
					oddPixTksP->Fill(TkX,TkY);
					
					
					double RowX = (ClRow[ic] - floor(ClRow[ic]))*pixSizeX;
					double ColY = (ClCol[ic] - floor(ClCol[ic]))*pixSizeY;
					
					
					colRowOverlayOP->Fill(RowX,ColY);
					
					RowX += TkResX[ClTkI[ic][tk]][clustI];
					ColY += TkResY[ClTkI[ic][tk]][clustI];
					
					//wrap tracks around
					if (RowX > pixSizeX) RowX -= pixSizeX;
					else if (RowX < 0)   RowX += pixSizeX;
					if (ColY > pixSizeY) ColY -= pixSizeY;
					else if (ColY < 0)   ColY += pixSizeY;
					
					if (RowX > pixSizeX) RowX -= pixSizeX;
					else if (RowX < 0)   RowX += pixSizeX;
					if (ColY > pixSizeY) ColY -= pixSizeY;
					else if (ColY < 0)   ColY += pixSizeY;
					
					colRowOverlayOTkP->Fill(RowX,
					                       ColY);
					
					
					
				}
				
				if (HtX < 0.0 || HtX > 0.01 || HtY < 0.0 || HtY > 0.015 )
				{
					overLocP->Fill(ClRow[ic],ClCol[ic]);
					//cout << "Col: " << ClCol[ic] << " Row: " << ClRow[ic] << endl;
				}
				
				
				
			}
		}
////////////////////////////////////////////////////////////////////////
	}
	cout << "finished!!" << endl;
	time_t timeCurrent = time(0);
	cout << "processed " << nentries << " events in " ;
	cout << (timeCurrent - timeStart)/ 60.0 << " minutes." << endl;
	cout << hitN << " Tracks passed cuts" << endl;
}

TCanvas* PChemTab::getCanvas(){
	return fCanvas;
}

void PChemTab::activateCanvas(){
	fCanvas->cd();
}

void PChemTab::remoteExecute(){
	execute->Clicked();
}

#endif
