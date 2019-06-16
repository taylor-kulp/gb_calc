# gb_calc
A calculator application for the GameBoy

This program is mostly an excercise in playing with the GameBoy assembly language (Z80 subset).  The initial development was heavily influenced by the tutorial at http://gameboy.mongenel.com/asmschool.html

The development environment used for this included:
GameBoy Tile Designer
http://www.devrs.com/gb/hmgd/gbtd.html
Rednex Game Boy Development System
https://github.com/rednex/rgbds
Also reusing the famous HARDWARE.INC file
https://github.com/gbdev/hardware.inc/blob/master/hardware.inc

Testing was done using the BGB emulator http://bgb.bircd.org/ 

Quick Start:
Ensure that rgbds folder (above) has been added to the path
Run "build calc"
Run "calc.gb" in the BGB emulator