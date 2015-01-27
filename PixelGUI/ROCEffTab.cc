#ifndef __ROCEffTab_cc
#define __ROCEffTab_cc

/*  ROCEffTab.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier74@gmail.com
 * 		date: May 17, 2011
 * 		
 * 		This class is part of a ROC relative efficiency study wherein 
 * 		hits that are part of clusters associated with a track are 
 * 		plotted over the surface of a ROC and then the results of many
 * 		ROC's are summed together to determine if , on the average, 
 * 		there exist parts of a ROC that are more efficient than others.
 * 		Specifically, this is looking for an effect due to the 
 * 		p-chem spray.
 * 
 */

#include "ROCEffTab.h"

using namespace std;

ClassImp(ROCEffTab); 

ROCEffTab::ROCEffTab() : anaPixelTab(){} 

ROCEffTab::ROCEffTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str()
	                                                       ,this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	
	TGGroupFrame *macroFrame = new TGGroupFrame(this,"ROC Relative Efficiency Analysis",kHorizontalFrame);
	execute = new TGTextButton(macroFrame,"Execute");
	execute->Connect("Clicked()","ROCEffTab",this,"executeAnalysis()");
	
	TGVButtonGroup *bGroup = new TGVButtonGroup(macroFrame,"Plot Type");
	avgHits = new TGRadioButton(bGroup,"Average Hits");
	avgHits->SetState(kButtonDown);
	stdHits = new TGRadioButton(bGroup,"Deviation");
	
	macroFrame->AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	macroFrame->AddFrame(bGroup , new TGLayoutHints(kLHintsCenterY | kLHintsRight));
	
	AddFrame(macroFrame,new TGLayoutHints(kLHintsExpandX | kLHintsBottom));
	
	ROCHits = new TH2D(("ROCHits" + *thisTabNumber).c_str(),"ROC Hits",
	                             52,0,52,80,0,80);
	ROCHitsDev = new TH2D(("ROCHitsDev" + *thisTabNumber).c_str(),"ROC HitsDev",
	                             52,0,52,80,0,80);
	
	//gStyle->SetOptStat("0");
	calculated = false;
} 

ROCEffTab::~ROCEffTab(){
	delete ROCHits, ROCHitsDev;
}

void ROCEffTab::executeAnalysis(){
	
	if(!calculated){
		Loop(-1);
		calculated = true;
	}
	if (avgHits->IsDown())
	{
		paintCanvas(0);
	}
	else
	{
		paintCanvas(1);
	}
	
	
}


void ROCEffTab::Loop(int nevt){
	
	int eventN = 0;
	vector< vector< vector< int > > > hits;
	
	const int ROCS = 640;
	const int COLS = 52;
	const int ROWS = 80;
	
	hits.resize(ROCS);
	for (int i = 0; i < ROCS; ++i) 
	{
		hits[i].resize(COLS);
		
		for (int j = 0; j < COLS; ++j)
		{
			hits[i][j].resize(ROWS);
		}
	}
	
	
	//Enable only branches used for faster operation
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("DgN",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClModule",1);
	fChain->SetBranchStatus("ClDgN",1);
	fChain->SetBranchStatus("ClTkN",1);
	fChain->SetBranchStatus("DgRoc",1);
	fChain->SetBranchStatus("DgRocC",1);
	fChain->SetBranchStatus("DgRocR",1);
	//fChain->SetBranchStatus("DgRow",1);
	//fChain->SetBranchStatus("DgCol",1);
	fChain->SetBranchStatus("DgClI",1);
	fChain->SetBranchStatus("TkEta",1);
	
	
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
		eventN++;
		if (eventN % 1000 == 0)
		{
			cout << "Event: " << eventN << endl;
		}
		
		
		for (int ic = 0; ic < DgN; ++ic) {
			
			//various cuts...
			if (ClTkN[DgClI[ic]] < 1) continue; // at least one track
			if (ClLayer[DgClI[ic]] != 1) continue; // layer is 1
			if (ClModule[DgClI[ic]] != -1 && ClModule[DgClI[ic]] != 1) continue; // module +-1
			if (ClDgN[DgClI[ic]] != 1) continue; //single pixel cluster
			
			
			bool smallEta = true;
			for (int i = 0; smallEta && i < ClTkN[DgClI[ic]]; i++)
			{
				//cout << "TkEta: " << TkEta[ClTkI[DgClI[ic]][i]] << endl;
				if (pow(float(TkEta[ClTkI[DgClI[ic]][i]]),2) > 1.0){
					smallEta = false;
					///cout << "digi cut out" << endl;
					}
			}
			if (!smallEta) continue;
			
			
			
			
			
			
			//Convert Ladder and module from physical label to array index.
			int LadderI,ModuleI;
			if (ClLadder[DgClI[ic]] < 0)
			{
				LadderI = ClLadder[DgClI[ic]] + 10;
			}
			else
			{
				LadderI = ClLadder[DgClI[ic]] + 9;
			}
			if (ClModule[DgClI[ic]] < 0)
			{
				ModuleI = 0;
			}
			else
			{
				ModuleI = 1;
			}
			
			
			//Add Hit into proper entry
			
			if (DgRoc[ic] > 7)
			{
				hits[LadderI*32 + ModuleI*16 + DgRoc[ic]][51-DgRocC[ic]][DgRocR[ic]]++;
			}
			else{
				hits[LadderI*32 + ModuleI*16 + DgRoc[ic]][DgRocC[ic]][DgRocR[ic]]++;
			}
		}
		
		
		
////////////////////////////////////////////////////////////////////////
	}
	
	//INITIALIZE VECTOR ARRAYS
	vector< vector<float> > avgs;//mean hits per pixel
	vector< vector<float> > devs;//deviation from mean
	avgs.resize(COLS);
	devs.resize(COLS);
	for (int col = 0; col < COLS; col++)
	{
		avgs[col].resize(ROWS);
		devs[col].resize(ROWS);
		for (int row = 0; row < ROWS; row++)
		{
			avgs[col][row] = 0.0;
			devs[col][row] = 0.0;
		}
	}
	
	//CALC MEAN AND DEVIATION
	for (int col = 0; col < COLS; col++)
	{
		for (int row = 0; row < ROWS; row++)
		{
			
			for (int ROC = 0; ROC < ROCS; ROC++)
			{
				avgs[col][row] += hits[ROC][col][row];
			}
			avgs[col][row] /= float(ROCS);
			ROCHits->Fill(col,row,avgs[col][row]);
			
			for (int ROC = 0; ROC < ROCS; ROC++)
			{
				devs[col][row] += pow(hits[ROC][col][row] - avgs[col][row],2);
			}
			ROCHitsDev->Fill(col, row, sqrt(devs[col][row] / float(ROCS)));
			
			
		}
	}
	
}

void ROCEffTab::paintCanvas(int plot){
	
	switch (plot)
	{
		case 0:
			ROCHits->Draw("colz");
			break;
		case 1:
			ROCHitsDev->Draw("colz");
			break;
		default:
			cout << "Invalid Plot" << endl;
	}
	fCanvas->Update();
}


bool ROCEffTab::useROC(int Layer, int Ladder, int Module, int ROC){
	//Populate this with checks that will cause the method to return 
	//false if the ROC specified is known to be "bad"
	
	return true;
}



TCanvas* ROCEffTab::getCanvas(){
	return fCanvas;
}

void ROCEffTab::activateCanvas(){
	fCanvas->cd();
}

void ROCEffTab::remoteExecute(){
	execute->Clicked();
}

#endif
