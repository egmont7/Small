#ifndef __PixelTabMacroRunner_cc
#define __PixelTabMacroRunner_cc

/*  PixelTabMacroRunner.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 6, 2010
 * 		
 * 		This class is intended to allow the user to run commands as one 
 * 		would from the ROOT command line. This means creating objects and
 * 		doing custom cuts to the PixelTree on the fly. It has a secondary
 * 		function of using a file browser to find and run both named and 
 * 		unnamed macros. 
 * 
 */

#include "PixelTabMacroRunner.h"    //include the header file for this class.


using namespace std;

ClassImp(PixelTabMacroRunner); // < ---- Necessary Detail for Class Inheritance;

PixelTabMacroRunner::PixelTabMacroRunner() : PixelTab(){} // <--Default constructor
                                      //   (Do not change!)

PixelTabMacroRunner::PixelTabMacroRunner(TGCompositeFrame *p, TChain *tree, const char *tabNumber)
         :PixelTab(p,tree){
	//Build the GUI here and initialize any class Data members that must
	//be initialized prior to analysis execution. This includes booking 
	//any histograms 
	
	TGVerticalFrame *splitterFrame = new TGVerticalFrame(this,10,10);
	TGHorizontalFrame *topH = new TGHorizontalFrame(splitterFrame,10,10,kFixedHeight);
	TGHorizontalFrame *botH = new TGHorizontalFrame(splitterFrame,10,10);
	TGCompositeFrame  *topC = new TGCompositeFrame(topH,10,10,kSunkenFrame);
	TGCompositeFrame  *botC = new TGCompositeFrame(botH,10,10,kSunkenFrame);
	
	thisTabNumber = new string(tabNumber);
	
	TRootEmbeddedCanvas *ECanvas = new TRootEmbeddedCanvas(("ECanvas" + *thisTabNumber).c_str(),topC,350,350);
	fCanvas = ECanvas->GetCanvas();
	//AddFrame(ECanvas, new TGLayoutHints(kLHintsExpandX));
	
	//TGLabel *l = new TGLabel(this, "Enter a ROOT command into this text field.");
	//AddFrame(l, new TGLayoutHints(kLHintsCenterX));
	
	
	//Enable this code for the TGCommandPlugin type of command interface
	commandFrame = new TGCommandFrame(botC, 700, 300);
	//AddFrame(commandFrame, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	////////////////////////////////////////////////////////////////////
	
	//Enable this code for more useful command history but output is printed
	//to console
	//commandLine = new TGTextEntry(this,"");
	//commandLine->Connect("ProcessedEvent(Event_t*)",
	//                    "PixelTabMacroRunner",this,
	//                     "executeCommand(Event_t*)");
	//AddFrame(commandLine, new TGLayoutHints(kLHintsExpandX));
	////////////////////////////////////////////////////////////////////
	
	topC->AddFrame(ECanvas,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,3,0,0,0));
	botC->AddFrame(commandFrame,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,3,0,0,0));
	topH->AddFrame(topC,new TGLayoutHints(kLHintsTop | kLHintsExpandY | kLHintsExpandX,0,0,1,2));
	botH->AddFrame(botC,new TGLayoutHints(kLHintsTop | kLHintsExpandY | kLHintsExpandX,0,0,1,2));
	topH->Resize(topC->GetDefaultWidth(),topH->GetDefaultHeight()+20);
	botH->Resize(botC->GetDefaultWidth(),botH->GetDefaultHeight()+20);
	splitterFrame->AddFrame(topH, new TGLayoutHints(kLHintsTop | kLHintsExpandX));
	
	TGHSplitter *hSplitter = new TGHSplitter(splitterFrame);
	hSplitter->SetFrame(topH,kTRUE);
	splitterFrame->AddFrame(hSplitter, new TGLayoutHints(kLHintsTop | kLHintsExpandX));
	splitterFrame->AddFrame(botH, new TGLayoutHints(kLHintsBottom | kLHintsExpandX | kLHintsExpandY));
	AddFrame(splitterFrame,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	
	TGGroupFrame *macroFrame = new TGGroupFrame(this,"execute a Macro",kHorizontalFrame);
	macroPath = new TGTextEntry(macroFrame,"");
	browse = new TGTextButton(macroFrame,"Browse for Macro");
	browse->Connect("Clicked()","PixelTabMacroRunner",this,"browseMacro()");
	execute = new TGTextButton(macroFrame,"Execute Macro");
	execute->Connect("Clicked()","PixelTabMacroRunner",this,"executeMacro()");
	
	TGVButtonGroup *bGroup = new TGVButtonGroup(macroFrame,"Choose Macro Type");
	isNamed = new TGRadioButton(bGroup,"Named(Compiled) Macro[.cc]");
	isNamed->SetState(kButtonDown);
	isUnnamed = new TGRadioButton(bGroup,"Unnamed Macro[.C]");
	
	macroFrame->AddFrame(macroPath, new TGLayoutHints(kLHintsExpandX));
	macroFrame->AddFrame(browse , new TGLayoutHints(kLHintsCenterY));
	macroFrame->AddFrame(execute , new TGLayoutHints(kLHintsCenterY));
	macroFrame->AddFrame(bGroup , new TGLayoutHints(kLHintsCenterY | kLHintsRight));
	
	AddFrame(macroFrame,new TGLayoutHints(kLHintsExpandX | kLHintsBottom));
	
	calculated = false;
	historyPosition = 99;
	
	for (int i = 0; i < 100; i++)
	{
		commandHistory[i] = new TString("");
	}
	
} 

PixelTabMacroRunner::~PixelTabMacroRunner(){}


void PixelTabMacroRunner::browseMacro(){
	
	static TString dir(".");
    TGFileInfo fi;
    fi.SetMultipleSelection(false);
    if(isNamed->IsDown()){
		fi.fFileTypes = gOpenFileTypes3;
	}
	else{
		fi.fFileTypes = gOpenFileTypes2;
	}
    fi.fIniDir    = StrDup(dir);
    new TGFileDialog(gClient->GetDefaultRoot(), this, kFDOpen,&fi);
	dir = fi.fIniDir;
	macroPath->SetText(fi.fFilename);
}

void PixelTabMacroRunner::executeMacro(){
	
	TString macroDir(macroPath->GetBuffer()->GetString());
	
	int currentPosition = macroDir.Length();
	while(macroDir(currentPosition,1) != "/"){
		currentPosition--;
	}
	
	cout << "Directory is: " << macroDir(0,currentPosition) << endl;
	TString currentDir(getenv("PWD"));
	TString macroDir2(macroDir(0,currentPosition));
	
	gSystem->ChangeDirectory(macroDir2.Data());
	
	if(isNamed->IsDown()){
		cout << "Named is selected" << endl;
		stringstream ss(stringstream::in | stringstream::out);
		TString macroName(macroDir(currentPosition + 1,(macroDir.Length() - currentPosition)));
		ss << ".L " << macroName.Data() << "++";
		gROOT->ProcessLine(ss.str().c_str());
	}
	else if(isUnnamed->IsDown()){
		cout << "UnNamed is selected" << endl;
		gROOT->Macro(macroPath->GetBuffer()->GetString());
	}
	
	gSystem->ChangeDirectory(currentDir.Data());
}

void PixelTabMacroRunner::remoteExecute(){
	//do nothing
}

void PixelTabMacroRunner::executeCommand(Event_t* event){
	
	char   input[10];
	UInt_t keysym;

	if (event->fType != kGKeyPress) {
		return;
	}
	gVirtualX->LookupString(event, input, sizeof(input), keysym);
	//std::cout << "event : " << event->fCode << " " << event->fState
	//          << "; " << keysym << std::endl;
	
	
	if(keysym == 4100){//enter
	
		for (int i = 0; i < 99; i++)
		{
			
			commandHistory[i] = commandHistory[i + 1];
		}
		
		commandHistory[99] = new TString(commandLine->GetBuffer()->GetString());
		historyPosition = 100;
		
		cout << "executing:  " << commandHistory[99]->Data() << endl;
		gROOT->ProcessLine(commandHistory[99]->Data());
		fCanvas->Update();
		cout << "Finished Executing" << endl << endl;
		commandLine->SetText("");
		
	}
	else if(keysym == 4115){//up arrow
		
		if(historyPosition > 0 ){
			historyPosition--;
			commandLine->SetText(commandHistory[historyPosition]->Data());
		}
		
	}
	else if(keysym == 4117){//down arrow
		
		if( historyPosition < 99 ){
			historyPosition++;
			commandLine->SetText(commandHistory[historyPosition]->Data());
		}
		
	}
	
}

TCanvas* PixelTabMacroRunner::getCanvas(){
	//here you want to return the Canvas that you would like to be 
	//saved via the file menu. If you have multiple canvases you will
	//probably want to have some gui radio buttons to pick which canvas
	//to save. 
	return fCanvas;
}

void PixelTabMacroRunner::activateCanvas(){
	//This is called whenever the user switches tabs to this one. Again,
	//if you have multiple canvases you will need to add some way to switch
	//to the canvas that you want.
	fCanvas->cd();
}
#endif
