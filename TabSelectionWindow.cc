#ifndef __TabSelectionWindow_cc
#define __TabSelectionWindow_cc

/*  TabSelectionWindow.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 30, 2010
 * 		Window used to select choose to execute multiple tabs back to back.
 * 		
 * 	
 */

#include "TabSelectionWindow.h"    //include the header file for this class.

using namespace std;

ClassImp(TabSelectionWindow);

TabSelectionWindow::TabSelectionWindow(const TGWindow *p, int numTabsIn, PixelTab ** tabArray)
	: TGTransientFrame(gClient->GetRoot(),p, 250, 300, kVerticalFrame){
	
	AddFrame(new TGLabel(this,"Select Which Analysis to run."), new TGLayoutHints(kLHintsCenterX));
	
	numTabs = numTabsIn;
	fTabArray = tabArray;
////////////////////////////////////////////////////////////////////////
/////////////////////////Add Components/////////////////////////////////
	TGCompositeFrame *checkBoxFrame = new TGCompositeFrame(this);
	
	//create and add Check Boxes
	for (int i = 0; i < numTabs; i++)
	{
		cout << "before" << endl;
		selectionArray[i] = new TGCheckButton(checkBoxFrame,new TGHotString(tabArray[i]->ClassName()));
		cout << "after" << endl;
		checkBoxFrame->AddFrame(selectionArray[i]);
	}
	openSave = new TGCheckButton(this,"Open save dialog when finished?");
	
	
	TGHorizontalFrame *buttonFrame = new TGHorizontalFrame(this);
	ok = new TGTextButton(buttonFrame, "OK");
	ok->Connect("Clicked()","TabSelectionWindow",this,"okProcess()");
	cancel = new TGTextButton(buttonFrame,"Cancel");
	cancel->Connect("Clicked()","TabSelectionWindow",this,"cancelProcess()");
	
	buttonFrame->AddFrame(ok, new TGLayoutHints(kLHintsCenterX));
	buttonFrame->AddFrame(cancel, new TGLayoutHints(kLHintsCenterX));
	
	AddFrame(checkBoxFrame, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(openSave,      new TGLayoutHints(kLHintsExpandX));
	AddFrame(buttonFrame,   new TGLayoutHints(kLHintsExpandX));
	
	SetWindowName("Select Tabs to Execute");
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	Resize(250,300);
	
	gClient->WaitFor(this);
} 

TabSelectionWindow::~TabSelectionWindow(){
	
}

void TabSelectionWindow::okProcess(){
	
	for (int i = 0; i < numTabs; i++)
	{
		if(selectionArray[i]->IsDown()){
			fTabArray[i]->activateCanvas();
			fTabArray[i]->remoteExecute();
			stringstream ss(stringstream::in | stringstream::out);
			ss << selectionArray[i]->GetString().Data() << " - finished!";
			selectionArray[i]->SetText(ss.str().c_str());
			gSystem->ProcessEvents();
		}
	}
	
	if (openSave->IsDown())
	{
		new SaveSelectionWindow(this,numTabs,fTabArray);
	}
	
	
	
	CloseWindow();
}

void TabSelectionWindow::cancelProcess(){
	CloseWindow();
}

#endif

