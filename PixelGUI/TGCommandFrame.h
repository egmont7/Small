// @(#)root/gui:$Id: TGCommandPlugin.h 23115 2008-04-10 13:35:37Z rdm $
// Author: Bertrand Bellenot   26/09/2007
// Adapted for use with PixelGUI by Caleb Fangmeier 7/09/2010

#ifndef ROOT_TGCommandFrame
#define ROOT_TGCommandFrame

#ifndef ROOT_TGFrame
#include "TGFrame.h"
#endif

class TGLabel;
class TGComboBox;
class TGTextEntry;
class TGTextBuffer;
class TGTextView;
class TTimer;

class TGCommandFrame : public TGCompositeFrame {

protected:
   Int_t              fPid;               // current process id
   TGHorizontalFrame *fHf;                // horizontal frame
   TGLabel           *fLabel;             // "command :" label
   TGComboBox        *fComboCmd;          // commands combobox
   TGTextEntry       *fCommand;           // command text entry widget
   TGTextBuffer      *fCommandBuf;        // command text buffer
   TGTextView        *fStatus;            // output capture view
   TTimer            *fTimer;             // for local/remote update
   Int_t              fCmdHistIndex;      // to facilitate arrow perusal of
                                          // command history

public:

   TGCommandFrame(const TGCompositeFrame *p, UInt_t w, UInt_t h);
   virtual ~TGCommandFrame();

   void           CheckRemote(const char * /*str*/);
   void           HandleCommand();

   virtual Bool_t HandleTimer(TTimer *t);

   ClassDef(TGCommandFrame, 0) // Command (I/O redirection) plugin for the new ROOT Browser
};

#endif
