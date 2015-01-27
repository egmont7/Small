#ifndef __TriggerTab_cc
#define __TriggerTab_cc

#include "TriggerTab.h"

using namespace std;

ClassImp(TriggerTab); 

TriggerTab::TriggerTab() : anaPixelTab(){} 

TriggerTab::TriggerTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :anaPixelTab(p,tree, kVerticalFrame){
	
	thisTabNumber = new string(tabNumber);
	
	TGTab *trigTab = new TGTab(this);
	TGCompositeFrame *textTab = trigTab->AddTab("Look at single event");
	TGCompositeFrame *histTab = trigTab->AddTab("Examine distribution of trigger");
	
	
	output = new TGTextView(textTab,500,500,"");
	
	textTab->AddFrame(output, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	bg = new TGHButtonGroup(textTab);
	bg->Connect("Clicked(Int_t)","TriggerTab",this,"bgSlot(Int_t)");
	fed1Radio = new TGRadioButton(bg,"fed1");
	fed1Radio->SetState(EButtonState(kButtonDown));
	fed2Radio = new TGRadioButton(bg,"fed2");
	l1tRadio = new TGRadioButton(bg,"l1t");
	l1taRadio = new TGRadioButton(bg,"l1ta");
	l1ttRadio = new TGRadioButton(bg,"l1tt");
	hltaRadio = new TGRadioButton(bg,"hlta");
	ttARadio = new TGRadioButton(bg,"ttA");
	l1ARadio = new TGRadioButton(bg,"l1A");
	hlARadio = new TGRadioButton(bg,"hlA");
	hltRadio = new TGRadioButton(bg,"hlt");
	textTab->AddFrame(bg, new TGLayoutHints(kLHintsExpandX));
	
	execute = new TGTextButton(textTab,"Execute");
	execute->Connect("Clicked()","TriggerTab",this,"executeAnalysis()");
	
	TGHorizontalFrame *h = new TGHorizontalFrame(textTab,kHorizontalFrame);
	eventInput = new TGTextEntry(h,"");
	eventInput->Connect("ReturnPressed()","TGTextButton",execute,"Clicked()");
	h->AddFrame(new TGLabel(h,"Enter event number:  "));
	h->AddFrame(eventInput, new TGLayoutHints(kLHintsExpandX));
	textTab->AddFrame(h,new TGLayoutHints(kLHintsExpandX));
	
	textTab->AddFrame(execute, new TGLayoutHints(kLHintsCenterX | kLHintsCenterY));
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("Ecanvas" + *thisTabNumber).c_str(),histTab,350,350);
	fCanvas = ECanvas->GetCanvas();
	histTab->AddFrame(ECanvas,new TGLayoutHints(kLHintsExpandX));
	
	TGHorizontalFrame *h2 = new TGHorizontalFrame(histTab,kHorizontalFrame);
	
	trigChoice = new TGComboBox(h2,"");
	trigChoice->Connect("Selected(Int_t)","TriggerTab",this,"updateBitChoice(Int_t)");
	//trigChoice->EnableTextInput(false);
	bitChoice  = new TGComboBox(h2,"");
	//bitChoice->EnableTextInput(false);
	trigChoice->AddEntry("fed1",0);
	trigChoice->AddEntry("fed2",1);
	trigChoice->AddEntry("l1t",2);
	trigChoice->AddEntry("l1ta",3);
	trigChoice->AddEntry("l1tt",4);
	trigChoice->AddEntry("hlta",5);
	trigChoice->AddEntry("ttA",6);
	trigChoice->AddEntry("l1A",7);
	trigChoice->AddEntry("hlA",8);
	trigChoice->AddEntry("hlt",9);
	trigChoice->Select(0);
	histExecute = new TGTextButton(h2,"execute");
	histExecute->Connect("Clicked()","TriggerTab",this,"histExecuteAnalysis()");
	h2->AddFrame(trigChoice,  new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	h2->AddFrame(bitChoice,   new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	h2->AddFrame(histExecute, new TGLayoutHints(kLHintsExpandX));
	histTab->AddFrame(h2,new TGLayoutHints(kLHintsExpandX));
	
	TGHorizontalFrame *h3 = new TGHorizontalFrame(histTab,kHorizontalFrame);
	
	
	manualBinning = new TGCheckButton(h3,"Enable Manual Binning");
	manualBinning->Connect("Clicked()","TriggerTab",this,"setManualBinning()");
	
	TGCompositeFrame *minMaxFrame = new TGCompositeFrame(h3);
	TGMatrixLayout *mLayout = new TGMatrixLayout(minMaxFrame,2,3,0,0);
	minMaxFrame->SetLayoutManager(mLayout);
	
	minMaxFrame->AddFrame(new TGLabel(minMaxFrame,"Range Min"), new TGLayoutHints(kLHintsExpandX));
	minMaxFrame->AddFrame(new TGLabel(minMaxFrame,"Range Max"), new TGLayoutHints(kLHintsExpandX));
	minMaxFrame->AddFrame(new TGLabel(minMaxFrame,"# bins"),    new TGLayoutHints(kLHintsExpandX));
	
	minField = new TGTextEntry(minMaxFrame,"");
	minMaxFrame->AddFrame(minField);
	
	maxField = new TGTextEntry(minMaxFrame,"");
	minMaxFrame->AddFrame(maxField);
	
	nBinsField = new TGTextEntry(minMaxFrame,"");
	minMaxFrame->AddFrame(nBinsField);
	
	h3->AddFrame(manualBinning,new TGLayoutHints(kLHintsLeft | kLHintsCenterY));
	h3->AddFrame(minMaxFrame, new TGLayoutHints(kLHintsExpandX | kLHintsCenterX | kLHintsBottom));
	
	
	histTab->AddFrame(h3, new TGLayoutHints(kLHintsExpandX | kLHintsBottom));
	
	AddFrame(trigTab, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	manualBinning->Clicked();
} 

TriggerTab::~TriggerTab(){
	
	if (trigHist != 0) delete trigHist;
	
}

void TriggerTab::executeAnalysis(){
	
	if(fed1Stream != 0) delete fed1Stream;
	if(fed2Stream != 0) delete fed2Stream;
	if(l1tStream != 0) delete l1tStream;
	if(l1taStream != 0) delete l1taStream;
	if(l1ttStream != 0) delete l1ttStream;
	if(hltaStream != 0) delete hltaStream;
	if(ttAStream != 0) delete ttAStream;
	if(l1AStream != 0) delete l1AStream;
	if(hlAStream != 0) delete hlAStream;
	if(hltStream != 0) delete hltStream;
	
	
	fChain->SetBranchStatus("*",0);
	fChain->SetBranchStatus("fed1",1);
	fChain->SetBranchStatus("fed2",1);
	fChain->SetBranchStatus("l1t",1);
	fChain->SetBranchStatus("l1ta",1);
	fChain->SetBranchStatus("l1tt",1);
	fChain->SetBranchStatus("hlta",1);
	fChain->SetBranchStatus("ttA",1);
	fChain->SetBranchStatus("l1A",1);
	fChain->SetBranchStatus("hlA",1);
	fChain->SetBranchStatus("hlt",1);
	fChain->SetBranchStatus("event",1);
	
	Loop(-1);
}


void TriggerTab::Loop(int nevt){
	
	int eventNumber = atoi(eventInput->GetBuffer()->GetString());
	if (eventNumber == 0 && eventInput->GetBuffer()->GetString() != "0"){
		new TGMsgBox(gClient->GetRoot(), this,"Warning","Couldn't parse input!", kMBIconExclamation, kMBDismiss);
		return;
	}
	if (fChain == 0) return;
	
	Long64_t nentries = fChain->GetEntries();
	
	int step(100000), maxEvents(nentries); 
	if (nevt > 0 && nevt < nentries) maxEvents = nevt; 
	if (maxEvents < 1000000) step = 50000;
	if (maxEvents < 100000)  step = 5000;
	if (maxEvents < 10000)   step = 500;
	if (maxEvents < 1000)    step = 100;
	
	Long64_t nbytes = 0, nb = 0;
	bool finished = false;
	for (Long64_t jentry=0; jentry<nentries && !finished;jentry++) {
		
		if ((nevt > -1) && (jentry > nevt)) break;
		Long64_t ientry = LoadTree(jentry);
		if (ientry < 0) break;
		nb = fChain->GetEntry(jentry);   nbytes += nb;
		if (Cut(ientry) < 0) continue;
////////////////////////////////////////////////////////////////////////
			cout << "event: " << event << endl;
			if (event == eventNumber)
			{
				stringstream buffer(stringstream::in | stringstream::out);
				TGText *temp;
				
				//fed1 Data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				buffer << setw(8) << 0;
				buffer << setw(13) << fed1;
				buffer << "    " << fed1_Comments;
				output->AddLine(buffer.str().c_str()); buffer.str("");
				output->Update();
				
				temp = output->GetText();
				fed1Stream = new TGText(temp);
				output->Clear();
				
				
				//fed2 data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				buffer << setw(8) << 0;
				buffer << setw(13) << fed2;
				buffer << "    " << fed2_Comments;
				output->AddLine(buffer.str().c_str()); buffer.str("");
				output->Update();
				temp = output->GetText();
				fed2Stream = new TGText(temp);
				output->Clear();
				
				//l1t data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				buffer << setw(8) << 0;
				buffer << setw(13) << l1t;
				buffer << "    " << l1t_Comments;
				output->AddLine(buffer.str().c_str()); buffer.str("");
				output->Update();
				temp = output->GetText();
				l1tStream = new TGText(temp);
				output->Clear();
				
				//l1ta[4] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 4; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << l1ta[i];
					buffer << "    " << l1ta_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				l1taStream = new TGText(temp);
				output->Clear();
				
				//l1tt[4] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 4; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << l1tt[i];
					buffer << "    " << l1tt_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				l1ttStream = new TGText(temp);
				output->Clear();
				
				//hlta[8] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 8; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << hlta[i];
					buffer << "    " << hlta_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				hltaStream = new TGText(temp);
				output->Clear();
				
				//ttA[64] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 64; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << ttA[i];
					buffer << "    " << ttA_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				ttAStream = new TGText(temp);
				output->Clear();
				
				//l1A[128] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 128; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << l1A[i];
					buffer << "    " << l1A_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				l1AStream = new TGText(temp);
				output->Clear();
				
				//hlA[256] data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				for (int i = 0; i < 256; i++)
				{
					buffer << setw(8) << i;
					buffer << setw(13) << hlA[i];
					buffer << "    " << hlA_Comments[i];
					output->AddLine(buffer.str().c_str()); buffer.str("");
				}
				output->Update();
				temp = output->GetText();
				hlAStream = new TGText(temp);
				output->Clear();
				
				//hlt data
				buffer << setw(8) << "index";
				buffer << setw(13) << "value";
				buffer << "    " << "Description";
				output->AddLine(buffer.str().c_str()); buffer.str("");
				buffer << setw(8) << 0;
				buffer << setw(13) << hlt;
				buffer << "    " << hlt_Comments;
				output->AddLine(buffer.str().c_str()); buffer.str("");
				
				output->Update();
				temp = output->GetText();
				hltStream = new TGText(temp);
				output->Clear();
				
				
				finished = true;
				updateTextView();
			}
////////////////////////////////////////////////////////////////////////
	}
	if (finished == false)
	{
		new TGMsgBox(gClient->GetRoot(), this,"Event not found","Specified event not found!", kMBIconExclamation, kMBDismiss);
		fed1Stream = new TGText("");
		fed2Stream = new TGText("");
		l1tStream  = new TGText("");
		l1taStream = new TGText("");
		l1ttStream = new TGText("");
		hltaStream = new TGText("");
		ttAStream  = new TGText("");
		l1AStream  = new TGText("");
		hlAStream  = new TGText("");
		hltStream  = new TGText("");
	}
	

}

void TriggerTab::updateBitChoice(Int_t id){

	bitChoice->RemoveAll();
	for (int i = 0; i < triggerArraySizes[id]; i++)
	{
		stringstream ss(stringstream::in | stringstream::out);
		ss << i;
		bitChoice->AddEntry(ss.str().c_str(),i);
	}
	
	
}

void TriggerTab::histExecuteAnalysis(){
	
	int trigID = trigChoice->GetSelected();
	
	if (manualBinning->IsDown())
	{
		
		double max = atof(maxField->GetBuffer()->GetString());
		double min = atof(minField->GetBuffer()->GetString());
		long long nBins = atoll(nBinsField->GetBuffer()->GetString());
		
		if (max <= min)
		{
			new TGMsgBox(gClient->GetRoot(), this,"Invalid Range","Range min must be smaller than range max!", kMBIconExclamation, kMBDismiss);
			return;
		}
		
		gDirectory->RecursiveRemove(trigHist);
		if (trigHist != 0) delete trigHist;
		
		trigHist = new TH1D("trigHist","Trigger Distribution",nBins,min ,max);
		stringstream ss(stringstream::in | stringstream::out);
		
		if (trigID == 0 || trigID == 1 || trigID == 2 || trigID == 9)
		{
			ss << trigChoice->GetTextEntry()->GetBuffer()->GetString();
		}
		else{
			ss << trigChoice->GetTextEntry()->GetBuffer()->GetString()
			   << "[" << bitChoice->GetTextEntry()->GetBuffer()->GetString()
			   << "]";
		}
		
		pixelTree->Project("trigHist",ss.str().c_str());
		trigHist->Draw();
		fCanvas->Update();
		
		
	}
	else{
		if (trigID == 0 || trigID == 1 || trigID == 2 || trigID == 9)
		{
			stringstream ss(stringstream::in | stringstream::out);
			
			ss << trigChoice->GetTextEntry()->GetBuffer()->GetString();
			pixelTree->Draw(ss.str().c_str());
		}
		else{
			stringstream ss(stringstream::in | stringstream::out);
			
			ss << trigChoice->GetTextEntry()->GetBuffer()->GetString()
			   << "[" << bitChoice->GetTextEntry()->GetBuffer()->GetString()
			   << "]";
			
			pixelTree->Draw(ss.str().c_str());
		}
		fCanvas->Update();
	}
	
}


void TriggerTab::setManualBinning(){
	if(manualBinning->IsDown()){
		//enable stuff
		maxField->SetEnabled(true);
		minField->SetEnabled(true);
		nBinsField->SetEnabled(true);
	}
	else{
		//disable stuff
		maxField->SetEnabled(false);
		minField->SetEnabled(false);
		nBinsField->SetEnabled(false);
	}
	
}

void TriggerTab::bgSlot(Int_t id){
	updateTextView();
}

void TriggerTab::updateTextView(){
	
	if(fed1Stream == 0) return;
	
	if(fed1Radio->IsDown()){
		output->Clear();
		output->AddText(fed1Stream);
		
	}
	else if(fed2Radio->IsDown()){
		output->Clear();
		output->AddText(fed2Stream);
		
	}
	else if(l1tRadio->IsDown()){
		output->Clear();
		output->AddText(l1tStream);
		
	}
	else if(l1taRadio->IsDown()){
		output->Clear();
		output->AddText(l1taStream);
		
	}
	else if(l1ttRadio->IsDown()){
		output->Clear();
		output->AddText(l1ttStream);
		
	}
	else if(hltaRadio->IsDown()){
		output->Clear();
		output->AddText(hltaStream);
		
	}
	else if(ttARadio->IsDown()){
		output->Clear();
		output->AddText(ttAStream);
		
	}
	else if(l1ARadio->IsDown()){
		output->Clear();
		output->AddText(l1AStream);
		
	}
	else if(hlARadio->IsDown()){
		output->Clear();
		output->AddText(hlAStream);
		
	}
	else if(hltRadio->IsDown()){
		output->Clear();
		output->AddText(hltStream);
		
	}
	output->Update();
	
}

TCanvas* TriggerTab::getCanvas(){
	return 0;
}

void TriggerTab::activateCanvas(){
	fCanvas->cd();
}

void TriggerTab::remoteExecute(){
	execute->Clicked();
}

#endif
