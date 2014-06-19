 #
 #  Makefile
 #  
 #
 #  Created by Tony Kelly II on 5/28/10.
 #  Modified by Caleb Fangmeier on 7/09/10 to build PixelGUI
 #  This is the makefile which is used to compile all the
 #		necessary implimentation files and build all the
 #		libraries that the GUI needs.
 #
 #

#
#  Defining some variables to make changing the compiling
#	options later an easier task.  This is just in case
#	the GUI grows to unforseen proportions.
#

CC=g++
CFLAGS=`root-config --cflags --glibs`
LDFLAGS=
ROOTCFLAGS = $(shell $(ROOTSYS)/bin/root-config --cflags)
ROOTLIBS = $(shell $(ROOTSYS)/bin/root-config --libs)
ROOTGLIBS = $(shell $(ROOTSYS)/bin/root-config --glibs)

baseClasses_h = PixelGUI.h TabSelectionWindow.h AnalysisSelectionWindow.h
abcClasses_h = PixelTab.h anaPixelTab.h anaPixelTree.h
helperClasses_h = PixelTabMacroRunner.h TGCommandFrame.h

pkg1Classes_h=anaPixelTabBadEdges.h DigiModuleMap.h DigiROCMap.h LowROCOccupancies.h
pkg2Classes_h=TabX.h PixelTabSizeDistribution.h TrackMomentumTab.h PixelTabChargeDistribution.h PrimaryVertexTab.h TkPerEventTab.h OccTab.h   TriggerTab.h LumiblockTab.h OccEventTab.h

baseClasses_cc = PixelGUI.cc TabSelectionWindow.cc AnalysisSelectionWindow.cc
abcClasses_cc = PixelTab.cc anaPixelTab.cc anaPixelTree.cc
helperClasses_cc =PixelTabMacroRunner.cc TGCommandFrame.cc

pkg1Classes_cc = anaPixelTabBadEdges.cc DigiModuleMap.cc DigiROCMap.cc LowROCOccupancies.cc
pkg2Classes_cc = TabX.cc PixelTabSizeDistribution.cc TrackMomentumTab.cc PixelTabChargeDistribution.cc PrimaryVertexTab.cc TkPerEventTab.cc OccTab.cc TriggerTab.cc LumiblockTab.cc OccEventTab.cc
	
#
# Below are the make argument options
#  Usually the syntax below should suffice for a FULL remake:
#		make clean all
#
 
PixelGUI: PixelGUIDict
	$(CC) $(CFLAGS) -o PixelGUI $(baseClasses_cc) $(abcClasses_cc) $(helperClasses_cc) $(pkg1Classes_cc) PixelGUI_Dict.cc
	rm PixelGUI_Dict.cc
	rm PixelGUI_Dict.h
	clear
	echo Done Compiling all Routines

PixelGUIDict:
#add all class header files to this list before PixelGUI_Linkdef.h
	rootcint -f PixelGUI_Dict.cc -c $(baseClasses_h) $(abcClasses_h) $(helperClasses_h) $(pkg1Classes_h) PixelGUI_Linkdef.h

clean:
	rm -rf *.o PixelGUI



