#ifndef __PixelGUI_H
#define __PixelGUI_H

/*  PixelGUI.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 11, 2010
 * 
 * 		This is a GUI to aid in the analysis of pixelTree type files 
 * 		such as those created by Urs Langenegger. One can add additional
 * 		analysis tools to this code in the form of a graphical tab that
 * 		inherits from PixelTab or anaPixelTab
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
// ROOT standard library imports
#include "TGFrame.h"              //Imports all ROOT Frames including TGMainFrame
#include "TApplication.h"         //needed to compile to standalone executable
#include "TCanvas.h"              //Standard TCanvas 
#include "TGLabel.h"              //A Simple Text Label that can be put in the GUI
#include "TGTab.h"                //A Tabbed Frame
#include "TGMenu.h"               //Facilitates A Menu Bar
#include "TFile.h"                //allows use of .root files
#include "TTree.h"                //Use ROOT style n-tuple's
#include "TChain.h"               //like multiple TChains together
#include "TString.h"              //ROOT style string
#include "TGFileDialog.h"         //File browsing Dialog
#include "TColor.h"

//auxilary classes
#include "TabSelectionWindow.h"
#include "AnalysisSelectionWindow.h"
#include "SaveSelectionWindow.h"

//Include all tab header files here

//parent Classes
#include "PixelTab.h"
#include "anaPixelTree.h"
#include "anaPixelTab.h"

//helper tabs
#include "PixelTabMacroRunner.h"

//package 1
#include "DigiModuleMap.h"
#include "DigiROCMap.h"
#include "anaPixelTabBadEdges.h"
#include "LowROCOccupancies.h"
#include "ROCEffTab.h"
#include "PChemTab.h"

//package 2
#include "PixelTabChargeDistribution.h"
#include "kShortTab.h"
#include "TabX.h"

//package 3
#include "LumiblockTab.h"
#include "OccEventTab.h"
#include "OccTab.h"
#include "TkPerEventTab.h"
#include "PrimaryVertexTab.h"
#include "TrackMomentumTab.h"
#include "PixelTabSizeDistribution.h"

//package 4
#include "TriggerTab.h"

using namespace std;

enum EmenuOptions{
	menuExit, menuOpenSingle, menuOpenList, menuSave, menuClose, menuExecuteMultiple, menuSaveMultiple
};

static const char * gAnalysisPackageInfo[] = 
	{"Occupancy Analysis", "7",
	 "Charge Distribution", "3",
	 "Austin's Analysis", "8",
	 "Trigger Analysis", "2",
	 "Pileup Analysis", "1"};

class PixelGUI : public TGMainFrame // <-- ":" indicates inheritance relationship
{
private:

	////////////////////////////////////////////////////////////////////
    /////////////////////CLASS DATA MEMBERS/////////////////////////////
    ////////////////////////////////////////////////////////////////////
	
	TChain *fPixelTree;
	
	TGMenuBar *fMenuBar;
	TGPopupMenu *fFile, *fAutomate;
	
	TGTab *fTab;
	
	int fNumTabs;
	
	//impose maximum of twenty tabs per analysis
	TGCompositeFrame *fFrameArray[20];
	PixelTab *fTabArray[20];
	
	
	int fPackageChoice;
	static const int fNumPackages = 5;
	
	
	////////////////////////////////////////////////////////////////////
    ////////////////////////////METHODS/////////////////////////////////
    ////////////////////////////////////////////////////////////////////
	
	void saveProcess();
	void openProcess(int openOption);
	void openProcessSpecial(const char* fileName,int fileType); //Package and/or file specified
	void closeProcess();
	
	bool selectAnalysisPackage();
	void buildTabs();
	void remoteExecute();
	void remoteSave();
	
	
public:
	
	PixelGUI(const TGWindow *p, UInt_t w, UInt_t h, const char* filename,int filetype, int packageNum);
	
	void handleMenuSlot(int option);
	void makeCanvasActive(Int_t);
	void exitProcess();
	
	virtual ~PixelGUI();
	
	ClassDef(PixelGUI,1); // < ---- Necessary Detail for Class Inheritance;
};

#endif
