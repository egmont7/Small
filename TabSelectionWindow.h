#ifndef __TabSelectionWindow_H
#define __TabSelectionWindow_H

/*  TabSelectionWindow.h
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


class TabSelectionWindow : public TGTransientFrame{
	
private:

	////////////////////////////////////////////////////////////////////
    /////////////////////CLASS DATA MEMBERS/////////////////////////////
    ////////////////////////////////////////////////////////////////////
	
	int numTabs;
	TGCheckButton *selectionArray[20];
	TGCheckButton *openSave;
	PixelTab ** fTabArray;
	
	TGTextButton *ok, *cancel; 
	////////////////////////////////////////////////////////////////////
    ////////////////////////////METHODS/////////////////////////////////
    ////////////////////////////////////////////////////////////////////


public:

	TabSelectionWindow(const TGWindow *p, int numTabs, PixelTab ** tabArray);
	
	void okProcess();
	void cancelProcess();
	
	virtual ~TabSelectionWindow();
	
	ClassDef(TabSelectionWindow,1); // < ---- Necessary Detail for Class Inheritance;
};

#endif
