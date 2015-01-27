#ifndef __PixelTab_cc
#define __PixelTab_cc

/*  PixelTab.cc
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

#include "PixelTab.h"    //include the header file for this class.

using namespace std;

ClassImp(PixelTab);

PixelTab::PixelTab() : TGCompositeFrame(){}

PixelTab::PixelTab(TGCompositeFrame *p, TChain *tree, int frameOption)
         :TGCompositeFrame(p, frameOption){
	pixelTree = tree;
} 

PixelTab::~PixelTab(){}

#endif
