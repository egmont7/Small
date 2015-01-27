#ifndef __anaPixelTab_H
#define __anaPixelTab_H

/*  anaPixelTab.h
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: July 21, 2010
 * 	
 * 		This is the parent class for all tabs in PixelGUI that want to 
 * 		make use of the functionality of anaPixelTree.
 */

#include "PixelTab.h"
#include "anaPixelTree.h"

using namespace std;

class anaPixelTab : public PixelTab
                  , public anaPixelTree // <-- ":" indicates inheritance relationship
{


public:

	anaPixelTab();
	anaPixelTab(TGCompositeFrame *p, TChain *tree, int frameOption = kVerticalFrame);
	
	virtual ~anaPixelTab();
	
protected:

	ClassDef(anaPixelTab,0); // < ---- Necessary Detail for Class Inheritance;
};

#endif
