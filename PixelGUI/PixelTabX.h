#ifndef __PixelTabX_H
#define __PixelTabX_H

/*  PixelTabX.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 24, 2010
 * 		
 * 		This is a generic class outline for a potential tab to be used
 * 		with PixelGUI. It inherites from PixelTab which gives it the 
 * 		basic functionality it needs to interface with the main GUI.
 * 		To use this class you need simply rename it
 * 		to something appropriate and fill in the necessary methods with the 
 * 		requisite functionality outlined in the method implementations.
 * 		To use this tab you you need to make the following changes to 
 * 		PixelGUI.
 * 
 * 			1)modify numTabs in PixelGUI.h to the correct number of tabs
 * 			  you want in the GUI.
 * 			2)Include PixelTabX.h(or whatever your header file is called)
 *            in PixelGUI.h
 * 			3)in PixelGUI.cc you need to create in instance of your class
 *            following the pattern seen there in the buildTabs() method.
 *
 * 		Finally you will need to add your new class to the Makefile and
 * 		to PixelGUI_Linkdef.h following the pattern set up there. 
 * 
 * 		To compile your code in BASH simply type
 * 		$ make
 * 		NOTE: you must have g++ installed for this to work.
 * 
 * 		To run the GUI type
 * 		$ ./PixelGUI
 * 
 */




#include "anaPixelTab.h"
//NOTE:PixelTab.h includes many of the C++ and ROOT classes and functions
//     that you will need in your analysis. Any additional classes
//     not included there should be included here.

using namespace std;

class PixelTabX : public anaPixelTab // <-- ":" indicates inheritance relationship
{


public:
	//ctor
	PixelTabX();
	PixelTabX(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	//We need to get a "tab number" in the constructor because of the way
	//that ROOT accesses certain objects such at histograms. Remember that
	//in the constructor for a histogram you can to pass a char into the
	//first argument. ROOT uses that char as essentially a variable
	//name for that object. This becomes a problem because if you create
	//two histograms with identical chars in that argument, the first one
	//will be overwritten EVEN IF THEY BELONG TO DIFFERENT OBJECTS. To get
	//around this problem we get the number of the tab that this analysis
	//belongs to and append that to the end of the char for each object that
	//requires it. See "PixelTabOne.cc" for some good examples of how this
	//technique works.
	
	//These are methods inherited from PixelTab. They MUST be implemented
	//with the proper features for the GUI to function properly.
	TCanvas* getCanvas();
	void activateCanvas();
	
	//This method is inherited from anaPixelTree and is where one can loop 
	//over events.
	void Loop(int nevt = -1);
	
	//dtor
	virtual ~PixelTabX();
	
	//Add any additional public methods here.
	//NOTE: any methods that are connected to event sources such as buttons
	//need to be delclared public. They are called "slots" and the event
	//sources are called "signals"
	void executeAnalysis();// <--------------"Slot Method"
	
	
	
private:

	//Declare All GUI elements. eg. TGTextButtons, or TGLabels
	//
	TGTextButton *execute;
	
	//Declare All Histograms and Plots
	TH1F *ChargeTracksL1, *ChargeNoTracksL1;
	THStack *stack1; //here THStack is used simply to superimpose two histograms
	//Declare All other Class Data members
	TCanvas *fCanvas;
	string *thisTabNumber;
	bool calculated;
	
	//Declare All Private Methods
	void paintCanvas();
	
	
	ClassDef(PixelTabX,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
