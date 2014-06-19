#ifndef __TriggerTab_H
#define __TriggerTab_H

#include "anaPixelTab.h"
#include "TGTextView.h"
#include "TGText.h"
#include <iomanip>
#include "TGMsgBox.h"
#include "TGComboBox.h"
#include "TGTab.h"
#include "TProfile.h"
#include "TDirectory.h"

static const char * fed1_Comments = "fed 1";

static const char * fed2_Comments = "fed 2";

static const char * l1t_Comments = "level 1 trigger";//---------

static const char * l1ta_Comments[] = //length:4
{"","","",""};

static const char * l1tt_Comments[] = //length:4
{"","","",""};

static const char * hlta_Comments[] = //length:8
{"","","","","","","",""};

static const char * ttA_Comments[] = //length:64
{"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
};

static const char * l1A_Comments[] = //length:128
{"BPTX+ and BPTX-"
,"BPTX+ (beam 1)"
,"BPTX- (beam 2)"
,"BPTX+ or BPTX-"
,"identical to L1Tech_BPTX_plus_AND_minus.v0"
,"BPTX+ and not BPTX-"
,"BPTX- and not BPTX+"
,"not (BPTX+ or BPTX-)"
,"Coincidence between any two wedges in HF"
,"Coincidence between any wedge in HF- and any wedge in HF+"
,"Coincidence among any three HF wedges with at least one in HF- and at least one in HF+"
,"HCAL: HO total OR"
,"HCAL: HB/HE total OR (2/3 of channels)"
,"currently unused"
,"currently unused"
,"currently unused"
,"","","",""
,"DT chamber global OR or 3X3 sectors FARNEAR coincidence (DT cosmics)"
,"","",""
,"RPC: RBC-TTU (cosmics), barrel (5 wheels)"
,"RPC: RBC-TTU (cosmics), pointing trigger (5 wheels)"
,"RPC: RBC-TTU (cosmics), wheel RB +2"
,"RPC: RBC-TTU (cosmics), wheel RB +1"
,"RPC: RBC-TTU (cosmics), wheel RB 0 "
,"RPC: RBC-TTU (cosmics), wheel RB -1"
,"RPC: RBC-TTU (cosmics), wheel RB -2"
,"coincidence of 2 inner RPC layers (station 1) with collision timing"
,"BSC minbias: hits +z side >= 1 and hits -z side >= 1 (inner segments only)"
,"BSC minbias: hits +z side >= 2 and hits -z side >= 2 (inner segments only)"
,"BSC OR: there is at least one hit, anywhere in the BSC"
,"BSC high multiplicity: all segments fire on both sides in coincidence (before June 1, 2010, only inner segments were required)"
,"BSC halo: beam 2 inner "
,"BSC halo: beam 2 outer"
,"BSC halo: beam 1 inner"
,"BSC halo: beam 1 outer"
,"BSC minbias: hits +z side >= 1 and hits -z side >= 1 (all segments: inner rings, outer petals) "
,"BSC minbias: hits +z side >= 2 and hits -z side >= 2 (all segments: inner rings, outer petals)"
,"BSC splash trigger: beam 1, threshold = 2; inner ring hits on -z side >= 2"
,"BSC splash trigger: beam 2, threshold = 2; inner ring hits on +z side >= 2"
,"","","","","","","",""
,"at least 1 CASTOR phi-sector with low but constant energy deposit along z"
,"at least 1 CASTOR phi-sector above (low) threshold"
,"at least 1 CASTOR phi-sector above (high) threshold"
,"not any CASTOR phi-sector above (noise) threshold"
,"","","",""
,"ZDC+ sum of EM and hadronic energy > E_beam/10"
,"ZDC- sum of EM and hadronic energy > E_beam/10"
,"ZDC tight vertex: TAN scintillators fire within 5ns of each other"
,"ZDC loose vertex: TAN scintillators fire within 15ns of each other"
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""};

static const char * hlA_Comments[] = //length:256
{"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""
,"","","","","","","","","","","","","","","",""};

static const char * hlt_Comments = "high level trigger";

static const int triggerArraySizes[] = {
	1,     //fed1
	1,     //fed2
	1,     //l1t
	4,     //l1ta
	4,     //l1tt
	8,     //hlta
	64,    //ttA
	128,   //l1A
	256,   //hlA
	1      //hlt
};

using namespace std;

class TriggerTab : public anaPixelTab
{

public:
	
	TriggerTab();
	TriggerTab(TGCompositeFrame *p, TChain *tree, const char *tabNumber);
	
	TCanvas* getCanvas();
	void activateCanvas();
	void remoteExecute();
	
	void Loop(int nevt = -1);
	void executeAnalysis();
	
	void histExecuteAnalysis();
	void updateBitChoice(Int_t id);
	void setManualBinning();
	
	void bgSlot(Int_t);
	
	
	virtual ~TriggerTab();
	
	
private:
	
	
	string *thisTabNumber;
	//textTab
	TGTextButton *execute;
	TGHButtonGroup *bg;
	TGRadioButton *fed1Radio, *fed2Radio, *l1tRadio, *l1taRadio,
	              *l1ttRadio, *hltaRadio, *ttARadio, *l1ARadio,
	              *hlARadio,  *hltRadio;
	TGText       *fed1Stream, *fed2Stream, *l1tStream, *l1taStream,
	             *l1ttStream, *hltaStream, *ttAStream, *l1AStream,
	             *hlAStream,  *hltStream;
	TGTextView *output;
	TGTextEntry *eventInput;
	
	//histTab
	TCanvas *fCanvas;
	TGComboBox *trigChoice, *bitChoice;
	TGTextButton *histExecute;
	TGCheckButton *manualBinning;
	TGTextEntry *maxField, *minField, *nBinsField;
	TGRadioButton *automatic, *fixed, *findRange;
	TH1D *trigHist;
	void updateTextView();
	
	
	ClassDef(TriggerTab,0);
};

#endif
