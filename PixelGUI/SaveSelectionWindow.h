#ifndef __SaveSelectionWindow_H
#define __SaveSelectionWindow_H

/*  SaveSelectionWindow.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 30, 2010
 * 		Window used to select choose to execute multiple tabs back to back.
 * 		
 */

#include "PixelGUI.h"
#include "TGButton.h"
#include "TGString.h"
#include "PixelTab.h"
using namespace std;


class SaveSelectionWindow : public TGTransientFrame{
	
private:

	////////////////////////////////////////////////////////////////////
    /////////////////////CLASS DATA MEMBERS/////////////////////////////
    ////////////////////////////////////////////////////////////////////
	
	int numTabs;
	bool saveChoice[20];
	TGCheckButton *selectionArray[20];
	PixelTab ** fTabArray;
	
	TGTextButton *ok, *cancel; 
	TGTextEntry *savePrefix;
	////////////////////////////////////////////////////////////////////
    ////////////////////////////METHODS/////////////////////////////////
    ////////////////////////////////////////////////////////////////////


public:

	SaveSelectionWindow(const TGWindow *p, int numTabs, PixelTab ** tabArray);
	
	void okProcess();
	void cancelProcess();
	
	virtual ~SaveSelectionWindow();
	
	ClassDef(SaveSelectionWindow,1); // < ---- Necessary Detail for Class Inheritance;
};

#endif
