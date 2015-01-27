#ifndef __AnalysisSelectionWindow_cc
#define __AnalysisSelectionWindow_cc

/*  AnalysisSelectionWindow.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: August 2, 2010
 * 		
 * 		
 * 	
 */

#include "AnalysisSelectionWindow.h"    //include the header file for this class.

using namespace std;

ClassImp(AnalysisSelectionWindow);

AnalysisSelectionWindow::AnalysisSelectionWindow(const TGWindow *p, int numPack,
                                                 int *analysisChoice, 
                                                 const char *PackageInfo[])
	: TGTransientFrame(gClient->GetRoot(),p, 250, 300, kVerticalFrame){
	
	AddFrame(new TGLabel(this,"Select Which Analysis Package to Load"), new TGLayoutHints(kLHintsCenterX));
	
	analysisSelection = analysisChoice;
	numPackages = numPack;
	
////////////////////////////////////////////////////////////////////////
/////////////////////////Add Components/////////////////////////////////
	radioSelection = new TGButtonGroup(this);
	
	//create and add Check Boxes
	for (int i = 0; i < numPackages; i++)
	{
		stringstream ss(stringstream::in | stringstream::out);
		ss << PackageInfo[(i * 2)] << "; Tabs: " << PackageInfo[(i * 2) + 1];
		radioArray[i] = new TGRadioButton(radioSelection,new TGHotString( ss.str().c_str() ));
	}
	
	TGHorizontalFrame *buttonFrame = new TGHorizontalFrame(this);
	ok = new TGTextButton(buttonFrame, "OK");
	ok->Connect("Clicked()","AnalysisSelectionWindow",this,"okProcess()");
	cancel = new TGTextButton(buttonFrame,"Cancel");
	cancel->Connect("Clicked()","AnalysisSelectionWindow",this,"cancelProcess()");
	
	buttonFrame->AddFrame(ok, new TGLayoutHints(kLHintsCenterX));
	buttonFrame->AddFrame(cancel, new TGLayoutHints(kLHintsCenterX));
	
	
	AddFrame(radioSelection, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(buttonFrame,   new TGLayoutHints(kLHintsExpandX));
	
	SetWindowName("Select Analysis Package");
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	Resize(250,300);
	
	gClient->WaitFor(this);
} 

AnalysisSelectionWindow::~AnalysisSelectionWindow(){
	
}

void AnalysisSelectionWindow::okProcess(){
	int choice = -1;
	for (int i = 0; i < numPackages; i++)
	{
		if(radioArray[i]->IsDown()){
			choice = i;
		}
	}
	*analysisSelection = choice;
	CloseWindow();
}

void AnalysisSelectionWindow::cancelProcess(){
	
	*analysisSelection = -1;
	CloseWindow();
}

#endif


