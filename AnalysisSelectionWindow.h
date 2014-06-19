#ifndef __AnalysisSelectionWindow_H
#define __AnalysisSelectionWindow_H

/*  AnalysisSelectionWindow.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: August 2, 2010
 * 
 * 		
 */

#include "PixelGUI.h"
#include "TGButton.h"
#include "TGString.h"
using namespace std;


class AnalysisSelectionWindow : public TGTransientFrame{
	
private:

	////////////////////////////////////////////////////////////////////
    /////////////////////CLASS DATA MEMBERS/////////////////////////////
    ////////////////////////////////////////////////////////////////////
	
	int *analysisSelection;
	int numPackages;
	TGButtonGroup *radioSelection;
	TGRadioButton *radioArray[20];
	
	TGTextButton *ok, *cancel;
	////////////////////////////////////////////////////////////////////
    ////////////////////////////METHODS/////////////////////////////////
    ////////////////////////////////////////////////////////////////////


public:

	AnalysisSelectionWindow(const TGWindow *p,int numPack, int *analysisChoice, const char *PackageInfo[]);
	
	void okProcess();
	void cancelProcess();
	
	virtual ~AnalysisSelectionWindow();
	
	ClassDef(AnalysisSelectionWindow,1); // < ---- Necessary Detail for Class Inheritance;
};

#endif
