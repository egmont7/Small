#ifndef __PrimaryVertexTab_cc
#define __PrimaryVertexTab_cc

#include "PrimaryVertexTab.h"

using namespace std;

ClassImp(PrimaryVertexTab); 

PrimaryVertexTab::PrimaryVertexTab() : anaPixelTab(){} 

PrimaryVertexTab::PrimaryVertexTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
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
	
	c->AddFrame(new TGLabel(c,"Range Minimum"));
	c->AddFrame(new TGLabel(c,"Range Maximum"));
	c->AddFrame(min, new TGLayoutHints(kLHintsExpandX));
	c->AddFrame(max, new TGLayoutHints(kLHintsExpandX));
	
	h->AddFrame(c, new TGLayoutHints(kLHintsCenterX));
	h->AddFrame(bg, new TGLayoutHints(kLHintsCenterX));
	AddFrame(h, new TGLayoutHints(kLHintsExpandX));
	
	execute = new TGTextButton(this,"Execute");
	execute->Connect("Clicked()","PrimaryVertexTab",this,"executeAnalysis()");
	AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
	calculated = false;
} 

PrimaryVertexTab::~PrimaryVertexTab(){
	
	if(PV != 0) delete PV;
	
}

void PrimaryVertexTab::executeAnalysis(){
	
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("event",1);
	fChain->SetBranchStatus("PvN",1);
	fChain->SetBranchStatus("PvIsFake",1);
	if(PV != 0) delete PV;
	
	if(automatic->IsDown()){
		PV = new TProfile("PV","# of Primary Verticies per Event",1000,atoi(min->GetBuffer()->GetString())
																	  ,atoi(max->GetBuffer()->GetString()));
		Loop();
		PV->Draw();
		fCanvas->Update();
	}
	else if(findRange->IsDown()){
		double maximum = 0; double minimum = 1000000000;
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
		double edgeBuffer = ((maximum - minimum) * .05);
		
		stringstream ss(stringstream::in | stringstream::out);
		
		ss << maximum;
		max->SetText(ss.str().c_str());
		ss.str("");
		ss << minimum;
		min->SetText(ss.str().c_str());
		
		//n=sprintf (buffermax, "%d", maximum);
		
		//char buffermin [50];
		//n=sprintf (buffermin, "%d", minimum);
		
		//max->SetText(buffermax);
		//min->SetText(buffermin);
		cout << "successfully found range" << endl;
		
		PV = new TProfile("PV","# of Primary Verticies per Event", 1000000, minimum - edgeBuffer,
		                                                                  maximum + edgeBuffer);
		
		Loop();
		PV->Draw();
		fCanvas->Update();
	}
	else{
		int maximum = atoi(max->GetBuffer()->GetString());
		int minimum = atoi(min->GetBuffer()->GetString());
		int range = maximum - minimum;
		if(range < 10000000){
			PV = new TProfile("PV","# of Primary Verticies per Event",range,atoi(min->GetBuffer()->GetString())
																     ,atoi(max->GetBuffer()->GetString()));
			Loop();
			PV->Draw();
			fCanvas->Update();
		}
		else{
			cout << "specify a smaller range" << endl;
			new TGMsgBox(gClient->GetRoot(), this,"Message","Must specify a range less than 10,000,000.", kMBIconExclamation, kMBDismiss);
			
		}
		
	}
	
	
	
	
	
}


void PrimaryVertexTab::Loop(int nevt){
	
	if (fChain == 0) return;
	
	Long64_t nentries = fChain->GetEntries();
	
	int step(100000), maxEvents(nentries); 
	if (nevt > 0 && nevt < nentries) maxEvents = nevt; 
	if (maxEvents < 1000000) step = 50000; 
	if (maxEvents < 100000)  step = 5000; 
	if (maxEvents < 10000)   step = 500; 
	if (maxEvents < 1000)    step = 100; 
	
	Long64_t nbytes = 0, nb = 0;
	int numPriVertex = 0;
	for (Long64_t jentry=0; jentry<nentries;jentry++) {
		
		if ((nevt > -1) && (jentry > nevt)) break;
		Long64_t ientry = LoadTree(jentry);
		if (ientry < 0) break;
		nb = fChain->GetEntry(jentry);   nbytes += nb;
		if (Cut(ientry) < 0) continue;
////////////////////////////////////////////////////////////////////////
		if (PvN > numPriVertex) numPriVertex = PvN;
		int numNotFake = 0;
		for (int i = 0; i < PvN; i++)
		{
		if (PvIsFake[i] == 0)
		numNotFake = numNotFake +1;
		}
		PV->Fill(event,numNotFake);
		
	
////////////////////////////////////////////////////////////////////////
	}
	cout << "maximum number of primary vertices: " << numPriVertex << endl;
}

TCanvas* PrimaryVertexTab::getCanvas(){
	return fCanvas;
}

void PrimaryVertexTab::activateCanvas(){
	fCanvas->cd();
}

void PrimaryVertexTab::remoteExecute(){
	execute->Clicked();
}

#endif
