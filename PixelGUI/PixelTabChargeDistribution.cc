#ifndef __PixelTabChargeDistribution_cc
#define __PixelTabChargeDistribution_cc

/*  PixelTabChargeDistribution.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 12, 2010
 *
 * 		This Class is to be used with PixelGUI. It fills a histogram
 * 		of the ClusterCharge for each event. It includes
 * 		radio buttons to examine the dependence for each layer.
 * 		NOTE:these histograms can take some time to fill.
 *
 */
#include "PixelTabChargeDistribution.h"    //include the header file for this class.

using namespace std;

ClassImp(PixelTabChargeDistribution);

PixelTabChargeDistribution::PixelTabChargeDistribution() : anaPixelTab(){}

PixelTabChargeDistribution::PixelTabChargeDistribution(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree,kVerticalFrame){
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),this,350,300);
	fCanvas = ECanvas->GetCanvas();
	
	TGHorizontalFrame *h = new TGHorizontalFrame(this,kHorizontalFrame);
	
	layerSelection = new TGButtonGroup(h,3,1);
	Layer1 = new TGRadioButton(layerSelection,"Layer 1");
	Layer1->SetState(EButtonState(kButtonDown));
	Layer2 = new TGRadioButton(layerSelection,"Layer 2");
	Layer3 = new TGRadioButton(layerSelection,"Layer 3");
	
	bunchCutSelection = new TGButtonGroup(h,3,1);
	noCut         = new TGRadioButton(bunchCutSelection,"No cut");
	noCut->SetState(EButtonState(kButtonDown));
	goodBunches   = new TGRadioButton(bunchCutSelection,"Good Bunch Crossings");
	noGoodBunches = new TGRadioButton(bunchCutSelection,"No Bunch Crossings");
	
	calcButton = new TGTextButton(h,"Execute Analysis");
	calcButton->Connect("Clicked()","PixelTabChargeDistribution",this,"LoopSlot()");
	
	h->AddFrame(layerSelection, new TGLayoutHints(kLHintsCenterX));
	h->AddFrame(bunchCutSelection, new TGLayoutHints(kLHintsCenterX));
	h->AddFrame(calcButton, new TGLayoutHints(kLHintsCenterX));
	
	AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	AddFrame(new TGLabel(this,"Fill histogram With cluster charge for each event."));
	AddFrame(h,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	isCalculated = false;
}

PixelTabChargeDistribution::~PixelTabChargeDistribution(){
	if(ChargeNoTracksL1 != 0) delete ChargeNoTracksL1;
	if(ChargeNoTracksL2 != 0) delete ChargeNoTracksL2;
	if(ChargeNoTracksL3 != 0) delete ChargeNoTracksL3;
	if(ChargeTracksL1 != 0) delete ChargeTracksL1;
	if(ChargeTracksL2 != 0) delete ChargeTracksL2;
	if(ChargeTracksL3 != 0) delete ChargeTracksL3;
	if(stack1 != 0) delete stack1;
	if(stack2 != 0) delete stack2;
	if(stack3 != 0) delete stack3;
	}

void PixelTabChargeDistribution::LoopSlot(){
	Loop(-1);
}

void PixelTabChargeDistribution::Loop(int nevt){

	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("ClN",1);
	fChain->SetBranchStatus("ClCharge",1);
	fChain->SetBranchStatus("ClLadder",1);
	fChain->SetBranchStatus("ClLayer",1);
	fChain->SetBranchStatus("ClType",1);
	fChain->SetBranchStatus("l1A",1);

	if(ChargeNoTracksL1 != 0) delete ChargeNoTracksL1;
	if(ChargeNoTracksL2 != 0) delete ChargeNoTracksL2;
	if(ChargeNoTracksL3 != 0) delete ChargeNoTracksL3;
	if(ChargeTracksL1 != 0) delete ChargeTracksL1;
	if(ChargeTracksL2 != 0) delete ChargeTracksL2;
	if(ChargeTracksL3 != 0) delete ChargeTracksL3;
	if(stack1 != 0) delete stack1;
	if(stack2 != 0) delete stack2;
	if(stack3 != 0) delete stack3;

	ChargeTracksL1 = new TH1F(("ChargeTracksL1" + *thisTabNumber).c_str(),"Charge deposited on tracks",280,0,280);
	ChargeNoTracksL1 = new TH1F(("ChargeNoTracksL1" + *thisTabNumber).c_str(),"Charge deposited off tracks",280,0,280);
	ChargeTracksL2 = new TH1F(("ChargeTracksL2" + *thisTabNumber).c_str(),"Charge deposited on tracks",280,0,280);
	ChargeNoTracksL2 = new TH1F(("ChargeNoTracksL2" + *thisTabNumber).c_str(),"Charge deposited off tracks",280,0,280);
	ChargeTracksL3 = new TH1F(("ChargeTracksL3" + *thisTabNumber).c_str(),"Charge deposited on tracks",280,0,280);
	ChargeNoTracksL3 = new TH1F(("ChargeNoTracksL3" + *thisTabNumber).c_str(),"Charge deposited off tracks",280,0,280);
	stack1 = new THStack(("stack1" + *thisTabNumber).c_str(),"Layer 1 Cluster Charge");
	stack2 = new THStack(("stack2" + *thisTabNumber).c_str(),"Layer 2 Cluster Charge");

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

		if(noGoodBunches->IsDown()){
			for (int ic = 0; ic < ClN; ++ic) {

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 1 && l1A[0] == 0){
					ChargeTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 2 && l1A[0] == 0){
					ChargeNoTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 1 && l1A[0] == 0){
					ChargeTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 2 && l1A[0] == 0){
					ChargeNoTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 1 && l1A[0] == 0){
					ChargeTracksL3->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 2 && l1A[0] == 0){
					ChargeNoTracksL3->Fill(ClCharge[ic]);
				}
			}
		}
		else if(goodBunches->IsDown()){
			for (int ic = 0; ic < ClN; ++ic) {

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 1 && l1A[0] == 1){
					ChargeTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 2 && l1A[0] == 1){
					ChargeNoTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 1 && l1A[0] == 1){
					ChargeTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 2 && l1A[0] == 1){
					ChargeNoTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 1 && l1A[0] == 1){
					ChargeTracksL3->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 2 && l1A[0] == 1){
					ChargeNoTracksL3->Fill(ClCharge[ic]);
				}
			}
		}
		else{//make no cuts
			for (int ic = 0; ic < ClN; ++ic) {

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 1){
					ChargeTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 1 && ClType[ic] == 2){
					ChargeNoTracksL1->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 1){
					ChargeTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 2 && ClType[ic] == 2){
					ChargeNoTracksL2->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 1){
					ChargeTracksL3->Fill(ClCharge[ic]);
				}

				if(ClCharge[ic] < 250 && ClLadder[ic] > -30 && ClLadder[ic] < 30 && ClLayer[ic] == 3 && ClType[ic] == 2){
					ChargeNoTracksL3->Fill(ClCharge[ic]);
				}
			}
		}
	}


	ChargeNoTracksL1->SetLineColor(2);
	ChargeTracksL1->SetTitle("Clusters associated with tracks");
	ChargeNoTracksL1->SetTitle("Clusters not associated with tracks");
	ChargeTracksL1->GetXaxis()->SetTitle("Cluster Charge (ke)");
	ChargeTracksL1->GetYaxis()->SetTitle("Number of Events");
	stack1->Add(ChargeTracksL1);
	stack1->Add(ChargeNoTracksL1);

	ChargeNoTracksL2->SetLineColor(2);
	ChargeTracksL2->SetTitle("Clusters associated with tracks");
	ChargeNoTracksL2->SetTitle("Clusters not associated with tracks");
	ChargeTracksL2->GetXaxis()->SetTitle("Cluster Charge (ke)");
	ChargeTracksL2->GetYaxis()->SetTitle("Number of Events");
	stack2->Add(ChargeTracksL2);
	stack2->Add(ChargeNoTracksL2);

	ChargeNoTracksL3->SetLineColor(2);
	ChargeTracksL3->SetTitle("Clusters associated with tracks");
	ChargeNoTracksL3->SetTitle("Clusters not associated with tracks");
	ChargeTracksL3->GetXaxis()->SetTitle("Cluster Charge (ke)");
	ChargeTracksL3->GetYaxis()->SetTitle("Number of Events");
	stack3 = new THStack(("stack3" + *thisTabNumber).c_str(),"Layer 3 Cluster Charge");
	stack3->Add(ChargeTracksL3);
	stack3->Add(ChargeNoTracksL3);


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
void PixelTabChargeDistribution::remoteExecute(){
	Loop();
}
void PixelTabChargeDistribution::paintCanvas(int layerNum){

	if(layerNum == 1){
		stack1->Draw("nostack");
	}
	else if(layerNum == 2){
		stack2->Draw("nostack");
	}
	else if(layerNum == 3){
		stack3->Draw("nostack");
	}
	fCanvas->BuildLegend(0.5,0.67,0.88,0.88,"Cluster Types");
	fCanvas->Update();
}

TCanvas* PixelTabChargeDistribution::getCanvas(){
	return fCanvas;
}

void PixelTabChargeDistribution::activateCanvas(){

	fCanvas->cd();
}

#endif
