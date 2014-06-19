#ifndef __SaveSelectionWindow_cc
#define __SaveSelectionWindow_cc

/*  SaveSelectionWindow.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: August 25, 2010
 * 		Window used to select multiple canvases for automated saving.
 * 		
 * 	
 */

#include "SaveSelectionWindow.h"    //include the header file for this class.

using namespace std;

ClassImp(SaveSelectionWindow);

SaveSelectionWindow::SaveSelectionWindow(const TGWindow *p, int numTabsIn, PixelTab ** tabArray)
	: TGTransientFrame(gClient->GetRoot(),p, 250, 300, kVerticalFrame){
	
	AddFrame(new TGLabel(this,"Select Which Canvases to Save"), new TGLayoutHints(kLHintsCenterX));	
	savePrefix = new TGTextEntry(this,"Enter save prefix here!!");
	AddFrame(savePrefix, new TGLayoutHints(kLHintsCenterX | kLHintsExpandX));
	
	numTabs = numTabsIn;
	fTabArray = tabArray;
////////////////////////////////////////////////////////////////////////
/////////////////////////Add Components/////////////////////////////////
	TGCompositeFrame *checkBoxFrame = new TGCompositeFrame(this);
	
	//create and add Check Boxes
	for (int i = 0; i < numTabs; i++)
	{
		selectionArray[i] = new TGCheckButton(checkBoxFrame,new TGHotString(fTabArray[i]->ClassName()));
		checkBoxFrame->AddFrame(selectionArray[i]);
	}
	
	TGHorizontalFrame *buttonFrame = new TGHorizontalFrame(this);
	ok = new TGTextButton(buttonFrame, "OK");
	ok->Connect("Clicked()","SaveSelectionWindow",this,"okProcess()");
	cancel = new TGTextButton(buttonFrame,"Cancel");
	cancel->Connect("Clicked()","SaveSelectionWindow",this,"cancelProcess()");
	
	buttonFrame->AddFrame(ok, new TGLayoutHints(kLHintsCenterX));
	buttonFrame->AddFrame(cancel, new TGLayoutHints(kLHintsCenterX));
	
	AddFrame(checkBoxFrame, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	AddFrame(buttonFrame,   new TGLayoutHints(kLHintsExpandX));
	
	SetWindowName("Select Plots to Save");
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	Resize(250,300);
	
	gClient->WaitFor(this);
} 

SaveSelectionWindow::~SaveSelectionWindow(){
	//~ if(savePrefix != 0) delete savePrefix;
	//~ if(savePrefix != 0) delete ok;
	//~ if(savePrefix != 0) delete cancel;
	//~ for (int i = 0; i < 20; i++)
	//~ {
		//~ if(savePrefix != 0) delete selectionArray[i];
	//~ }
	
}

void SaveSelectionWindow::okProcess(){
	
	for (int i = 0; i < numTabs; i++)
	{
		cout << "saving: ";
		if(selectionArray[i]->IsDown()){
			
			TCanvas *canvas = fTabArray[i]->getCanvas();
			stringstream ss(stringstream::in | stringstream::out);
			ss << savePrefix->GetBuffer()->GetString();
			ss << fTabArray[i]->ClassName() << ".pdf" ;
			cout  << ss << endl;
			canvas->SaveAs(ss.str().c_str());
		}
	}
	CloseWindow();
}

void SaveSelectionWindow::cancelProcess(){
	CloseWindow();
}

#endif

