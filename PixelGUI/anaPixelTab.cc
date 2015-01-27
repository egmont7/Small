#ifndef __anaPixelTab_cc
#define __anaPixelTab_cc

/*  anaPixelTab.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 21, 2010
 * 	
 * 		This is the parent class for all tabs in PixelGUI that want to 
 * 		make use of the functionality of anaPixelTree.
 */
#include "anaPixelTab.h"    //include the header file for this class.

using namespace std;

ClassImp(anaPixelTab);

anaPixelTab::anaPixelTab() : PixelTab(), anaPixelTree(){}

anaPixelTab::anaPixelTab(TGCompositeFrame *p, TChain *tree, int frameOption)
         : PixelTab(p,tree, frameOption)
         , anaPixelTree(tree){
} 

anaPixelTab::~anaPixelTab(){}

#endif
