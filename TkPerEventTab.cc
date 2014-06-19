#ifndef __TkPerEventTab_cc
#define __TkPerEventTab_cc

#include "TkPerEventTab.h"

using namespace std;

ClassImp(TkPerEventTab); 

TkPerEventTab::TkPerEventTab() : anaPixelTab(){} 

TkPerEventTab::TkPerEventTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str()
	                                                       ,this,350,350);
	fCanvas = ECanvas->GetCanvas();
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	TGHorizontalFrame *h = new TGHorizontalFrame(this, kHorizontalFrame); 
	TGCompositeFrame *c = new TGCompositeFrame(h);
	TGMatrixLayout *mLayout = new TGMatrixLayout(c,2,2);
	c->SetLayoutManager(mLayout);
	max = new TGTextEntry(c,"");
	min = new TGTextEntry(c,"");
	
	TGVButtonGroup *bg = new TGVButtonGroup(h,"Select Binning Type");
	automatic = new TGRadioButton(bg,"Automatic");
	findRange = new TGRadioButton(bg, "find Range");
	findRange->SetState(kButtonDown);
	fixed = new TGRadioButton(bg,"Force bin size 1");
	
	TGVButtonGroup *bg2 = new TGVButtonGroup(h,"Select Plot");
	Plot1 = new TGRadioButton(bg2,"Duel Plot");
	Plot1->SetState(EButtonState(kButtonDown));
	Ratio1 = new TGRadioButton(bg2,"Ratio Plot");
	Plot1->Connect("Clicked()","TkPerEventTab",this,"paintCanvasSlot()");
	Ratio1->Connect("Clicked()","TkPerEventTab",this,"paintCanvasSlot()");
	
	c->AddFrame(new TGLabel(c,"Range Minimum"));
	c->AddFrame(new TGLabel(c,"Range Maximum"));
	c->AddFrame(min, new TGLayoutHints(kLHintsExpandX));
	c->AddFrame(max, new TGLayoutHints(kLHintsExpandX));
	
	h->AddFrame(c, new TGLayoutHints(kLHintsCenterX));
	h->AddFrame(bg, new TGLayoutHints(kLHintsCenterX));
	h->AddFrame(bg2, new TGLayoutHints(kLHintsCenterX));
	AddFrame(h, new TGLayoutHints(kLHintsExpandX));
	
	
	execute = new TGTextButton(this,"Execute");
	execute->Connect("Clicked()","TkPerEventTab",this,"executeAnalysis()");
	AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
} 

TkPerEventTab::~TkPerEventTab(){
	if(OnTrackCl != 0) delete OnTrackCl;
	if(NumTk     != 0) delete NumTk; 
	if(Ratio     != 0) delete Ratio;
	if(stack1    != 0) delete stack1;
	}

void TkPerEventTab::executeAnalysis(){
		
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("ClN",1);
	fChain->SetBranchStatus("ClType",1);
	fChain->SetBranchStatus("event",1);
	fChain->SetBranchStatus("TkN",1);
	
	if(OnTrackCl != 0) delete OnTrackCl;
	if(NumTk     != 0) delete NumTk; 
	if(Ratio     != 0) delete Ratio;
	if(stack1    != 0) delete stack1;
	
	if (automatic->IsDown())
	{
		OnTrackCl = new TProfile("OnTrackCl","# of Clusters on Track",1000,atoi(min->GetBuffer()->GetString())
		                                                              ,atoi(max->GetBuffer()->GetString()));
		NumTk = new TProfile("NumTk","# of Tracks",1000,atoi(min->GetBuffer()->GetString())
		                                           ,atoi(max->GetBuffer()->GetString()));
		Ratio = new TProfile("Ratio","Ratio of Clusters on Track vs Tracks",1000,atoi(min->GetBuffer()->GetString())
		                                                                    ,atoi(max->GetBuffer()->GetString()));
		stack1 = new THStack(("stack1" + *thisTabNumber).c_str(),"On Track Clusters and Number of Tracks per Event");
		stack1->Add(OnTrackCl);
		stack1->Add(NumTk);
		
		Loop();
		paintCanvas();
	}
	else if (findRange->IsDown())
	{
		int maximum = 0; int minimum = 100000000;
		Long64_t nbytes = 0, nb = 0;
		Long64_t nentries = fChain->GetEntries();
		for (Long64_t jentry=0; jentry<nentries;jentry++) {

			Long64_t ientry = LoadTree(jentry);
			nb = fChain->GetEntry(jentry);   nbytes += nb;
			if (Cut(ientry) < 0) continue;
////////////////////////////////////////////////////////////////////////
			if(event > maximum) maximum = event;
			if(event < minimum) minimum = event;
////////////////////////////////////////////////////////////////////////
		}
		int edgeBuffer = (int)((maximum - minimum) * .05);
		
		char buffermax [50];
		int n;
		n=sprintf (buffermax, "%d", maximum);
		
		char buffermin [50];
		n=sprintf (buffermin, "%d", minimum);
		
		max->SetText(buffermax);
		min->SetText(buffermin);
		cout << "successfully found range" << endl;
		
		OnTrackCl = new TProfile("OnTrackCl","# of Clusters on Track",1000,minimum - edgeBuffer
																	  ,maximum + edgeBuffer);
		NumTk = new TProfile("NumTk","# of Tracks",1000,minimum - edgeBuffer
												   ,maximum + edgeBuffer);
		Ratio = new TProfile("Ratio","Ratio of Clusters on Track vs Tracks",1000,minimum - edgeBuffer
																			,maximum + edgeBuffer);
		stack1 = new THStack(("stack1" + *thisTabNumber).c_str(),"On Track Clusters and Number of Tracks per Event");
		stack1->Add(OnTrackCl);
		stack1->Add(NumTk);
		Loop();
		paintCanvas();
	}
	else
	{
		int maximum = atoi(max->GetBuffer()->GetString());
		int minimum = atoi(min->GetBuffer()->GetString());
		int range = maximum - minimum;
		if(range < 10000000){
			
			OnTrackCl = new TProfile("OnTrackCl","# of Clusters on Track",range,atoi(min->GetBuffer()->GetString())
																	       ,atoi(max->GetBuffer()->GetString()));
			NumTk = new TProfile("NumTk","# of Tracks",range,atoi(min->GetBuffer()->GetString())
													    ,atoi(max->GetBuffer()->GetString()));
			Ratio = new TProfile("Ratio","Ratio of Clusters on Track vs Tracks",range,atoi(min->GetBuffer()->GetString())
																				 ,atoi(max->GetBuffer()->GetString()));
			stack1 = new THStack(("stack1" + *thisTabNumber).c_str(),"On Track Clusters and Number of Tracks per Event");
			stack1->Add(OnTrackCl);
			stack1->Add(NumTk);
			
			Loop();
			paintCanvas();
		}
		else{
			cout << "specify a smaller range" << endl;
			new TGMsgBox(gClient->GetRoot(), this,"Message","Must specify a range less than 10,000,000.", kMBIconExclamation, kMBDismiss);
			return;
		}
	}
}

void TkPerEventTab::paintCanvasSlot(){
	paintCanvas();
}

void TkPerEventTab::paintCanvas(){
	
	if(OnTrackCl != 0){//histograms exist
		
		if(Plot1->IsDown()){
			stack1->Draw("nostack");
			fCanvas->BuildLegend(0.5,0.67,0.88,0.88,"Plots");
		}
		else{
			Ratio->Draw();
		}
		fCanvas->Update();
	}
}

void TkPerEventTab::Loop(int nevt){
	
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("ClN",1);
	fChain->SetBranchStatus("ClType",1);
	fChain->SetBranchStatus("event",1);
	fChain->SetBranchStatus("TkN",1);
	
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
		int numClusters = 0;
		for (int ic = 0; ic < ClN; ++ic) {
			if (ClType[ic] == 1)
				{
				numClusters++;
				}
		}
		OnTrackCl->Fill(event,numClusters);
		
		NumTk->Fill(event,TkN);
		
		if (TkN != 0){
			Ratio->Fill(event,numClusters/(double)TkN);
		}
////////////////////////////////////////////////////////////////////////
	}
	OnTrackCl->SetLineColor(2);
	Ratio->SetTitle("Ratio of On Track Clusters vs Number of Tracks for Every Event");
	Ratio->GetXaxis()->SetTitle("Event Number");
	Ratio->GetYaxis()->SetTitle("Ratio");
}

TCanvas* TkPerEventTab::getCanvas(){
	return fCanvas;
}

void TkPerEventTab::activateCanvas(){
	fCanvas->cd();
}

void TkPerEventTab::remoteExecute(){
	execute->Clicked();
}

#endif
