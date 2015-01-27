#ifndef __PixelTabMacroRunner_H
#define __PixelTabMacroRunner_H

/*  PixelTabMacroRunner.h
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


#include "PixelTab.h"
#include "TROOT.h"        //allows execution of ROOT macros using ACLIC
#include "TGCommandFrame.h"
#include "TGTextEdit.h"
#include "TInterpreter.h"
#include "Getline.h"
#include "TGTextView.h"
#include "TGSplitter.h"

using namespace std;

class PixelTabMacroRunner : public PixelTab // <-- ":" indicates inheritance relationship
{


public:
	//ctor
	PixelTabMacroRunner();
	PixelTabMacroRunner(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	//We need to get a "tab number" in the constructor because of the way
	//that ROOT accesses certain objects such at histograms. Remember that
	//in the constructor for a histogram you need to pass a char into the
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
	void remoteExecute();
	
	//dtor
	virtual ~PixelTabMacroRunner();
	
	//Add any additional methods here.
	void executeMacro();
	void browseMacro();
	
	void executeCommand(Event_t* event);
	
	
private:

	//Declare All GUI elements. eg. TGTextButton, or TGLabel
	
	
	TGTextEntry *commandLine;
	TString *commandHistory[100];
	int historyPosition;
	
	TGTextView   *status;
	
	TGTextEntry *macroPath;
	TGTextButton *browse;
	TGTextButton *execute;
	TGRadioButton *isNamed, *isUnnamed;
	TGCommandFrame *commandFrame;
	
	//Declare All Histograms and Plots
	TGraph *simpleGraph;
	
	//Declare All other Class Data members
	TCanvas *fCanvas;
	string *thisTabNumber;
	bool calculated;

	ClassDef(PixelTabMacroRunner,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
