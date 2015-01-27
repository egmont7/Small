#ifndef __PixelGUI_cc
#define __PixelGUI_cc

/*  PixelGUI.cc
 *  	Author: Caleb Fangmeier
 * 		contact: cfangmeier@hotmail.com
 * 		date: June 11, 2010
 * 
 * 		This is a GUI to aid in the analysis of pixelTree type files 
 * 		such as those created by Urs Langenegger. One can add additional
 * 		analysis tools to this code in the form of a graphical tab that
 * 		inherits from PixelTab or anaPixelTab. see PixelTabX for further
 * 		instructions.
 * 	
 */

#include "PixelGUI.h"    //include the header file for this class.

using namespace std;

ClassImp(PixelGUI);

PixelGUI::PixelGUI(const TGWindow *p, UInt_t w, UInt_t h, const char* fileName,int fileType, int packageNum)
	: TGMainFrame(p, w, h, kVerticalFrame){
	
	Connect("CloseWindow()","PixelGUI",this,"exitProcess()");
	
	fPackageChoice = packageNum;
	
////////////////////////////////////////////////////////////////////////
/////////////////////Create File Menu///////////////////////////////////
	TGCompositeFrame *menuFrame = new TGCompositeFrame(this, kHorizontalFrame);

	fMenuBar = new TGMenuBar(this);
	fFile = fMenuBar->AddPopup("&File");
	
	fFile->AddEntry("&Open Single Tree", menuOpenSingle);
	fFile->AddEntry("Open &List of Trees", menuOpenList);
	fFile->AddEntry("&Save Current Canvas", menuSave);
	fFile->AddEntry("&Close Current Analysis",menuClose);
	fFile->DisableEntry(3);
	fFile->DisableEntry(4);
	fFile->AddSeparator();
	fFile->AddEntry("&Exit", menuExit);
	fFile->Connect("Activated(Int_t)", "PixelGUI", this,"handleMenuSlot(int)");
	
	menuFrame->AddFrame(fMenuBar,new TGLayoutHints(kLHintsExpandX));
	
////////////////////////////////////////////////////////////////////////
/////////////////////////Add Components/////////////////////////////////
	
	AddFrame(menuFrame, new TGLayoutHints(kLHintsExpandX));	
	
	SetWindowName("PixelGUI - No Open File");
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	Resize(w,h);
	
	//Set the palette to something decent
	const Int_t NRGBs = 5;
	const Int_t NCont = 255;
	Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
	Double_t red[NRGBs]   = { 0.00, 0.00, 0.87, 1.00, 0.51 };
	Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
	Double_t blue[NRGBs]  = { 0.51, 1.00, 0.12, 0.00, 0.00 };
	TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
	gStyle->SetNumberContours(NCont);
	
	
	
	if (fileName != "")
	{
		openProcessSpecial(fileName, fileType);
	}
} 

PixelGUI::~PixelGUI(){}

bool PixelGUI::selectAnalysisPackage(){
	
	new AnalysisSelectionWindow(this,fNumPackages,&fPackageChoice,gAnalysisPackageInfo);
	
	if(fPackageChoice == -1)
	{
		return false;
	}
	else
	{
		fNumTabs = atoi(gAnalysisPackageInfo[(fPackageChoice * 2) +1]);
		return true;
	}
}


void PixelGUI::buildTabs(){
	
	fAutomate = fMenuBar->AddPopup("Automate");
	fAutomate->AddEntry("Execute Multiple Tabs", menuExecuteMultiple);
	fAutomate->AddEntry("Save Multiple Canvases", menuSaveMultiple);
	fAutomate->Connect("Activated(Int_t)", "PixelGUI", this, "handleMenuSlot(int)");
	
	fTab = new TGTab(this);
	fTab->Connect("Selected(Int_t)","PixelGUI",this,"makeCanvasActive(Int_t)");
	AddFrame(fTab,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
	
	switch (fPackageChoice)
	{
		case 0:
			fFrameArray[0] = fTab->AddTab("ROOT Console");
			fTabArray[0] = new PixelTabMacroRunner(fFrameArray[0],fPixelTree, "0");
			fFrameArray[0]->AddFrame(fTabArray[0],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
				
			fFrameArray[1] = fTab->AddTab("Module Hit Map");
			fTabArray[1] = new DigiModuleMap(fFrameArray[1],fPixelTree, "1");
			fFrameArray[1]->AddFrame(fTabArray[1],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[2] = fTab->AddTab("ROC Hit Map");
			fTabArray[2] = new DigiROCMap(fFrameArray[2],fPixelTree, "2");
			fFrameArray[2]->AddFrame(fTabArray[2],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[3] = fTab->AddTab("Edge Occupancy");
			fTabArray[3] = new anaPixelTabBadEdges(fFrameArray[3],fPixelTree, "3");
			fFrameArray[3]->AddFrame(fTabArray[3],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[4] = fTab->AddTab("ROC Occupancy");
			fTabArray[4] = new LowROCOccupancies(fFrameArray[4],fPixelTree, "4");
			fFrameArray[4]->AddFrame(fTabArray[4],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[5] = fTab->AddTab("ROC Relative Eff");
			fTabArray[5] = new ROCEffTab(fFrameArray[5],fPixelTree, "5");
			fFrameArray[5]->AddFrame(fTabArray[5],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[6] = fTab->AddTab("PChemTab");
			fTabArray[6] = new PChemTab(fFrameArray[6],fPixelTree, "6");
			fFrameArray[6]->AddFrame(fTabArray[6],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			break;
		case 1:
			fFrameArray[0] = fTab->AddTab("ROOT Console");
			fTabArray[0] = new PixelTabMacroRunner(fFrameArray[0],fPixelTree, "0");
			fFrameArray[0]->AddFrame(fTabArray[0],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[1] = fTab->AddTab("Charge Distribution");
			fTabArray[1] = new PixelTabChargeDistribution(fFrameArray[1],fPixelTree, "1");
			fFrameArray[1]->AddFrame(fTabArray[1],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[2] = fTab->AddTab("Theta Distribution(TabX)");
			fTabArray[2] = new TabX(fFrameArray[2],fPixelTree, "2");
			fFrameArray[2]->AddFrame(fTabArray[2],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			break;
		case 2:
			fFrameArray[0] = fTab->AddTab("ROOT Console");
			fTabArray[0] = new PixelTabMacroRunner(fFrameArray[0],fPixelTree, "0");
			fFrameArray[0]->AddFrame(fTabArray[0],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[1] = fTab->AddTab("Lumiblock");
			fTabArray[1] = new LumiblockTab(fFrameArray[1],fPixelTree, "1");
			fFrameArray[1]->AddFrame(fTabArray[1],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			
			fFrameArray[2] = fTab->AddTab("Occ Events");
			fTabArray[2] = new OccEventTab(fFrameArray[2],fPixelTree, "2");
			fFrameArray[2]->AddFrame(fTabArray[2],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[3] = fTab->AddTab("Occupancy");
			fTabArray[3] = new OccTab(fFrameArray[3],fPixelTree, "3");
			fFrameArray[3]->AddFrame(fTabArray[3],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
				
			fFrameArray[4] = fTab->AddTab("Track/ClustersOnTrack Per Event");
			fTabArray[4] = new TkPerEventTab(fFrameArray[4],fPixelTree, "4");
			fFrameArray[4]->AddFrame(fTabArray[4],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[5] = fTab->AddTab("Primary Vertex");
			fTabArray[5] = new PrimaryVertexTab(fFrameArray[5],fPixelTree, "5");
			fFrameArray[5]->AddFrame(fTabArray[5],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[6] = fTab->AddTab("Track Momentum");
			fTabArray[6] = new TrackMomentumTab(fFrameArray[6],fPixelTree, "6");
			fFrameArray[6]->AddFrame(fTabArray[6],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[7] = fTab->AddTab("Cluster Size");
			fTabArray[7] = new PixelTabSizeDistribution(fFrameArray[7],fPixelTree, "7");
			fFrameArray[7]->AddFrame(fTabArray[7],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			break;
		case 3:
			fFrameArray[0] = fTab->AddTab("ROOT Console");
			fTabArray[0] = new PixelTabMacroRunner(fFrameArray[0],fPixelTree, "0");
			fFrameArray[0]->AddFrame(fTabArray[0],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
									
			fFrameArray[1] = fTab->AddTab("Trigger Analysis");
			fTabArray[1] = new TriggerTab(fFrameArray[1],fPixelTree, "1");
			fFrameArray[1]->AddFrame(fTabArray[1],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			break;
		case 4:
			fFrameArray[0] = fTab->AddTab("ROOT Console");
			fTabArray[0] = new PixelTabMacroRunner(fFrameArray[0],fPixelTree, "0");
			fFrameArray[0]->AddFrame(fTabArray[0],
									new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
			break;
		default:
			cout << "error selecting package" << endl;
			
	}
	
	
	fTabArray[0]->activateCanvas();
	
}

void PixelGUI::remoteExecute(){	
	new TabSelectionWindow(this, fNumTabs, fTabArray);
}

void PixelGUI::remoteSave(){
	new SaveSelectionWindow(this,fNumTabs,fTabArray);
}

void PixelGUI::handleMenuSlot(int option){
	
	switch (option)
	{
		case menuExit:
			exit(1);
			break;
		case menuOpenSingle:
            openProcess(0);
			break;
		case menuOpenList:
			openProcess(1);
			break;
		case menuSave:
			saveProcess();
			break;
		case menuClose:
			closeProcess();
			break;
		case menuExecuteMultiple:
			remoteExecute();
			break;
		case menuSaveMultiple:
			remoteSave();
			break;
		default:
			cout << "There was a problem with the menus" << endl;
	}
}

void PixelGUI::makeCanvasActive(Int_t id){	
	fTabArray[id]->activateCanvas();
}

void PixelGUI::saveProcess(){
	
	int currentTabIndex = fTab->GetCurrent();
	
	TCanvas *canvas = fTabArray[currentTabIndex]->getCanvas();
	if(canvas != 0){
		static TString dir(".");
		TGFileInfo fi;
		fi.fFileTypes = gSaveFileTypes;
		fi.fIniDir    = StrDup(dir);
		new TGFileDialog(gClient->GetDefaultRoot(), this, kFDSave,&fi);
		dir = fi.fIniDir;
		if(fi.fFilename){ 
			TString name = fi.fFilename;
			
			bool validExtension = false;
			for (int i = 1; i < 26 && !validExtension; i = i + 2){
				string extension(gSaveFileTypes[i]);
				int extensionLength = extension.length();
				validExtension = name.EndsWith(extension.substr(1,extensionLength).c_str());
			}
			
			if(validExtension){//user added file extension
				canvas->SaveAs(fi.fFilename);
			} 
			else{//user did not add file extension
				stringstream ss(stringstream::in | stringstream::out);
				ss << fi.fFilename;
				string extension(gSaveFileTypes[(fi.fFileTypeIdx +1)]);
				int extensionLength = extension.length();
				ss << extension.substr(1,extensionLength);
				canvas->SaveAs(ss.str().c_str());
			}
		}
	}
}

void PixelGUI::openProcess(int openChoice){
	
	fPixelTree = new TChain("pixelTree","");
	
	static TString dir(".");
	TGFileInfo fi;
	fi.SetMultipleSelection(false);
	fi.fIniDir    = StrDup(dir);
	char title[2000];
		
	if(openChoice == 0){//single Tree
		fi.fFileTypes = gOpenFileTypes;// ".root" files
		new TGFileDialog(gClient->GetDefaultRoot(), this, kFDOpen,&fi);
		if(!fi.fFilename) return;
		dir = fi.fIniDir;
		fPixelTree->Add(fi.fFilename,0);
		sscanf(fi.fFilename,"%s",title);
		
	}
	
	else if(openChoice == 1){//List of Trees
		fi.fFileTypes = gOpenFileTypes4;// ".txt" files
		new TGFileDialog(gClient->GetDefaultRoot(), this, kFDOpen,&fi);
		if(!fi.fFilename) return;
		dir = fi.fIniDir;

		TString meta = fi.fFilename;
		ifstream is(meta);
		char pName[2000];
		
		meta.ReadLine(is);
		sscanf(meta.Data(), "%s", title);
		
		while(meta.ReadLine(is) && (!meta.IsNull())){
			
			if (meta.Data()[0] == '#') continue; 
			sscanf(meta.Data(), "%s", pName); 
			cout << "adding: " << pName << endl;
			fPixelTree->Add(pName,0);
			
		}
		is.close();
		
	}
	if (fPackageChoice == -1){
		if(!selectAnalysisPackage()) return;
	}
	stringstream ss(stringstream::in | stringstream::out);
	ss << "PixelGUI - " << title;
	SetWindowName(ss.str().c_str());
	
	buildTabs();
	
	gPad->cd(1);
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	
	fFile->EnableEntry(3);
	fFile->EnableEntry(4);
	fFile->DisableEntry(2);
	fFile->DisableEntry(1);
	
}

void PixelGUI::openProcessSpecial(const char* fileName,int fileType){
	
	fPixelTree = new TChain("pixelTree","");
	
	static TString dir(".");
	TGFileInfo fi;
	fi.SetMultipleSelection(false);
	fi.fIniDir    = StrDup(dir);
	char title[2000];
	
	if(fileType == 0){//single Tree
		fPixelTree->Add(fileName,0);
		sscanf(fileName,"%s",title);
	}
	else if(fileType == 1){//List of Trees

		TString meta = fileName;
		ifstream is(meta);
		char pName[2000];
		
		meta.ReadLine(is);
		sscanf(meta.Data(), "%s", title);
		
		while(meta.ReadLine(is) && (!meta.IsNull())){
			
			if (meta.Data()[0] == '#') continue; 
			sscanf(meta.Data(), "%s", pName); 
			cout << "adding: " << pName << endl;
			fPixelTree->Add(pName,0);
		}
		is.close();
	}
	
	if (fPackageChoice == -1){
		if(!selectAnalysisPackage()) return;
	}
	
	stringstream ss(stringstream::in | stringstream::out);
	ss << "PixelGUI - " << title;
	SetWindowName(ss.str().c_str());
	fNumTabs = atoi(gAnalysisPackageInfo[(fPackageChoice * 2) +1]);
	buildTabs();
	
	gPad->cd(1);
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
	
	fFile->EnableEntry(3);
	fFile->EnableEntry(4);
	fFile->DisableEntry(2);
	fFile->DisableEntry(1);
	
}

void PixelGUI::closeProcess(){
	
	//remove tabs from GUI
	for (int i = 0; i < fNumTabs; i++)
	{
		fTab->RemoveTab(0, false);
	}
	
	//delete tab objects
	for (int i = 0; i < fNumTabs; i++)
	{
		delete fTabArray[i];
		
		delete fFrameArray[i];
		
	}
	
	RemoveFrame(fTab);
	
	delete fTab;
	
	delete fPixelTree;
	
	SetWindowName("PixelGUI - No Open File");
	fPackageChoice = -1;
	fFile->DisableEntry(3);
	fFile->DisableEntry(4);
	fFile->EnableEntry(2);
	fFile->EnableEntry(1);
	
	fMenuBar->RemovePopup("Automate");
	Resize(100,100);
	
}

void PixelGUI::exitProcess(){
	gApplication->Terminate(0);
}

int main(int ac, char *av[]){
	
	int packageNum = -1;
	const char* filename = "";
	int filetype = -1; // 0->ROOT file, 1-> text file
	
	for (int i = 0; i < ac; i++)
	{
		if (!strcmp(av[i], "-r"))
		{
			filename = av[i+1];
			filetype = 0;
		}
		else if (!strcmp(av[i], "-t"))
		{
			filename = av[i+1];
			filetype = 1;
		}
		
		else if (!strcmp(av[i], "-p"))
		{
			packageNum = atoi(av[i+1]);
		}
	}
	TApplication theApp("App", &ac, av);
	PixelGUI mainWindow(gClient->GetRoot(), 100, 100,filename, filetype, packageNum);
	theApp.Run();
	
	return 0;
}
#endif
