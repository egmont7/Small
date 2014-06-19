#ifndef __PixelTab_H
#define __PixelTab_H

/*  PixelTab.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 8, 2010
 * 	
 * 		This is the parent class of all Tabs to be used in PixelGUI. It
 * 		Provides some essential functionality to the GUI such as providing
 * 		its child classes with with access to the pixelTree and and providing
 * 		the main gui with a TCanvas that can be saved to a file.(.pdf,.ps, etc)
 * 		
 * 
 */




// C/C++ standard library imports
#include <math.h>       //standard math functions such as pow,max,min...etc.
#include <vector>       //allows arrays with dynamic sizes
#include <list>         //reqired to use vector
#include <string.h>     //allows creation of C style strings
#include <fstream>      //allow streams to and from files
#include <sstream>      //string stream (very usefull for string manipulation)
#include <iostream>     //allows printing to console
#include <stdlib.h>     //imports C standard function library such as 
                        //parsing numbers from strings

// ROOT standard library imports(GUI Elements)
#include <TGFrame.h>              //Imports all ROOT Frames including TGMainFrame
#include <TGButton.h>             //Imports a variety of Button widgets
#include <TGLabel.h>              //A Simple Text Label that can be put in the GUI
#include "TGTextEntry.h"          //A Text Box for either Displaying data or
                                  //Data Input
#include "TGFileDialog.h"
#include "TGFileBrowser.h"
#include <TRootEmbeddedCanvas.h>  //Needed to "Embed" a TCanvas in you GUI
#include "TGComboBox.h"           //Imports both Combo(Drop-down) boxes and
                                  //the TListBox as used here
#include "TGButtonGroup.h"        //
#include <TCanvas.h>              //Standard TCanvas 
#include <TGraph.h>               //Standard TGraph
#include "TMultiGraph.h"          //Allows superimposing multiple Graphs
                                  //on a single TCanvas
#include <TLegend.h>              //Allows putting a legend on a TCanvas

#include "TAxis.h"                //Needed to set the axis labels and units
								  //on a TMultiGraph
#include "TH1.h"				  //allows use of 1-Dimensional Histograms
#include "TH2.h"				  //allows use of 2-Dimensional Histograms
#include "TH3.h"				  //allows use of 3-Dimensional Histograms
#include "THStack.h"
#include "TTree.h"
#include "TChain.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TObjString.h"
#include "TPad.h"
#include "TRint.h"

using namespace std;

static const char *gOpenFileTypes[] = {
   "ROOT files",   "*.root",
   "All files",    "*",
   0,             0
};

static const char *gOpenFileTypes2[] = {
	"ROOT Unnamed Macros", "*.C",
	"All files",   "*",
	0,             0
};

static const char *gOpenFileTypes3[] = {
	"ROOT Named Macros", "*.cc",
	"All files",   "*",
	0,             0
};

static const char *gOpenFileTypes4[] = {
	"Text Files", "*.txt",
	"All files",  "*",
	0,             0
};

static const char *gSaveFileTypes[] = {
    "PDF files",   "*.pdf",
    "PS files",   "*.ps",
	"EPS files",	"*.eps",
	"SVG files",	"*.svg",
	"GIF files",	"*.gif",
	"Macro files",	"*.C",
	"ROOT files",	"*.root",
	"XML files",	"*.xml",
	"PNG files",	"*.png",
	"XPM files",	"*.xpm",
	"JPG files",	"*.jpg",
	"TIFF files",	"*.tiff",
	"XCF files",	"*.xcf",
   "All files",    "*",
   0,             0
};



class PixelTab : public TGCompositeFrame // <-- ":" indicates inheritance relationship
{

public:

	PixelTab();
	PixelTab(TGCompositeFrame *p, TChain *tree, int frameOption = kVerticalFrame);
	
	virtual ~PixelTab();
	
	virtual TCanvas* getCanvas() = 0;
	
	virtual void activateCanvas() = 0;
	
	virtual void remoteExecute() = 0;
	
protected:

	TChain *pixelTree;

	ClassDef(PixelTab,0); // < ---- Necessary Detail for Class Inheritance
};

#endif
