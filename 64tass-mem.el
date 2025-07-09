;;; 64tass-mem.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2025 Sven Johansson

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Contains a map of the Commodore 64 memory layout.
;; Largely sourced from https://sta.c64.org/cbm64mem.html

;;; Code:

(defconst c64-memory-map
  '((0 . ((text . ("Processor port data directionr register. Bits:" "* Bit #x: 0 = Bit x in processor port can only be read, 1 = Bit x in processor port can be read and written"))))
    (1 . ((text . ("Processor port. Bits:" "" "* Bits #0-#2: Configuration for memory areas $A000-$BFFF, $D000-$DFFF and $E000-$FFFF" "    - %x00: RAM visible in all three areas" "    - %x01: RAM visible at $A000-$BFFF and $E000-$FFFF" "    - %x10: RAM visible at $A000-$BFFF" "    - %x11: KERNAL and BASIC ROMs visible at $A000-$BFFF and $E000-$FFFF" "    - %0xx: Character ROM visible at $D000-$DFFF (Except for %000)" "    - %1xx: I/O area visible att $D000-$DFFF (Except for value %100)" "* Bit #3: Datasette output signal" "* Bit #4: Datasette button status: 0 = One or more control buttons are pressed, 1 = No button pressed" "* Bit #5: Datasette motor control: 0 = On, 1 = Off"))))
    (2 . ((text . ("Unused"))))
    (3 . ((text . ("Unused." "(Default: $aa, Low Byte of execution address of routine converting floating point to integer.)"))))
    (4 . ((text . ("Unused." "(Default: $b1, High Byte of execution address of routine converting floating point to integer.)"))))
    (5 . ((text . ("Unused." "(Default: $91, Low Byte of execution address of routine converting integer to floating point.)"))))
    (6 . ((text . ("Unused." "(Default: $b3, High Byte of execution address of routine converting integer to floating point.)"))))
    (7 . ((text . ("- Byte being searched for during various operations." "- Current digit of number being input." "- Low byte of first integer operand during AND and OR." "- Low byte of integer-format FAC during INT()."))))
    (8 . ((text . ("- Byte being search for during various operations." "- Current byte of BASIC line during tokenization." "- High byte of first integer operand during AND and OR."))))
    (11 . ((text . ("Current token during tokenization." "Length of BASIC line during insertion of line." "AND/OR switch; $00 = AND; $FF = OR." "Number of dimensions during array operations."))))
    (19 . ((text . ("Current I/O device number." "(Default: $00 = keyboard for input and screen for output."))))
    (22 . ((text . ("Pointer to next expression in string stack. Values: $19; $1c; $1f; $22." "(Default: $19)"))))
    (23 . ((text . ("(Low Byte) Pointer to previous expression in string stack."))))
    (24 . ((text . ("(High Byte) Pointer to previous expression in string stack."))))
    (43 . ((text . ("(Low Byte) Pointer to beginning of BASIC area." "(Default: $01, of $0801, 2049)"))))
    (44 . ((text . ("(High Byte) Pointer to beginning of BASIC area." "(Default: $08, of $0801, 2049)"))))
    (47 . ((text . ("(Low Byte) Pointer to beginning of array variable area."))))
    (48 . ((text . ("(High Byte) Pointer to beginning of array variable area."))))
    (51 . ((text . ("(Low Byte) Pointer to beginning of string variable area. (Grows downwards from end of BASIC area.)"))))
    (52 . ((text . ("(High Byte) Pointer to beginning of string variable area. (Grows downwards from end of BASIC area.)"))))
    (55 . ((text . ("(Low Byte) Pointer to end of BASIC area." "(Default) $a000"))))
    (56 . ((text . ("(High Byte) Pointer to end of BASIC area." "(Default) $a000"))))
    (83 . ((text . ("Step size of garbage collection. Values: $03; $07."))))
    (84 . ((text . ("JMP ABS machine instruction, jump to current basic function" "Uses $55-$56"))))
    (85 . ((text . ("(Low Byte) Execution address of current BASIC function."))))
    (86 . ((text . ("(High Byte) Execution address of current BASIC function."))))
    (87 . ((text . ("Arithmetic register #3 (Byte 1/5)."))))
    (88 . ((text . ("Arithmetic register #3 (Byte 2/5)."))))
    (89 . ((text . ("Arithmetic register #3 (Byte 3/5)."))))
    (90 . ((text . ("Arithmetic register #3 (Byte 4/5)."))))
    (91 . ((text . ("Arithmetic register #3 (Byte 5/5)."))))
    (92 . ((text . ("Arithmetic register #4 (Byte 1/5)."))))
    (93 . ((text . ("Arithmetic register #4 (Byte 2/5)."))))
    (94 . ((text . ("Arithmetic register #4 (Byte 3/5)."))))
    (95 . ((text . ("Arithmetic register #4 (Byte 4/5)."))))
    (96 . ((text . ("Arithmetic register #4 (Byte 5/5)."))))
    (97 . ((text . ("FAC, arithmetic register #1 (Byte 1/5)."))))
    (98 . ((text . ("FAC, arithmetic register #1 (Byte 2/5)."))))
    (99 . ((text . ("FAC, arithmetic register #1 (Byte 3/5)."))))
    (100 . ((text . ("FAC, arithmetic register #1 (Byte 4/5)."))))
    (101 . ((text . ("FAC, arithmetic register #1 (Byte 5/5)."))))
    (102 . ((text . ("Sign of FAC. Bits:" "    - Bit #7: 0 = Positive, 1 = Negative."))))
    (104 . ((text . ("Temporary area for various operations."))))
    (105 . ((text . ("FAC, arithmetic register #2 (Byte 1/5)."))))
    (106 . ((text . ("FAC, arithmetic register #2 (Byte 2/5)."))))
    (107 . ((text . ("FAC, arithmetic register #2 (Byte 3/5)."))))
    (108 . ((text . ("FAC, arithmetic register #2 (Byte 4/5)."))))
    (109 . ((text . ("FAC, arithmetic register #2 (Byte 5/5)."))))
    (111 . ((text . ("(Low Byte) Pointer to first string expression during string comparison."))))
    (112 . ((text . ("(High Byte) Pointer to first string expression during string comparison."))))
    (113 . ((text . ("Low Byte of either: " "- Auxiliary pointer during array operations." "- Temporary area for saving original pointer to current BASIC instruction during VAL()" "- Pointer to current item of polynomial table during polynomial evaluation."))))
    (114 . ((text . ("High Byte of either: " "- Auxiliary pointer during array operations." "- Temporary area for saving original pointer to current BASIC instruction during VAL()" "- Pointer to current item of polynomial table during polynomial evaluation."))))
    (115 . ((text . ("CHRGET. Machine code routine to read next byte from BASIC program or direct command."))))
    (121 . ((text . ("CHRGOT. Read current byte from BASIC program or direct command."))))
    (122 . ((text . ("(Low Byte) Pointer to current byte in BASIC program or direct command."))))
    (123 . ((text . ("(High Byte) Pointer to current byte in BASIC program or direct command."))))
    (144 . ((text . ("Value of ST variable, device status for serial bus and datasette input/output. Serial bus bits:" "    - Bit #0: Transfer direction during which the timeout occured; 0 = Input; 1 = Output." "    - Bit #1: 1 = Timeout occurred." "    - Bit #4: 1 = VERIFY error occurred (only during VERIFY), the file read from the device did not match that in the memory." "    - Bit #6: 1 = End of file has been reached." "    - Bit #7: 1 = Device is not present." "Datasette bits:" "    - Bit #2: 1 = Block is too short (shorter than 192 bytes)." "    - Bit #3: 1 = Block is too long (longer than 192 bytes)." "    - Bit #4: 1 = Not all bytes read with error during pass 1 could be corrected during pass 2, or a VERIFY error occurred, the file read from the device did not match that in the memory." "    - Bit #5: 1 = Checksum error occurred." "    - Bit #6: 1 = End of file has been reached (only during reading data files)."))))
    (153 . ((text . ("Current input device number." "Default: $00 = Keyboard"))))
    (154 . ((text . ("Current output device number." "Default: $03 = Screen"))))
    (157 . ((text . ("System error display switch. Bits:" "    - Bit #6: 0 = Suppress I/O error messages; 1 = Display them." "    - Bit #7: 0 = Suppress system messages; 1 = Display them."))))
    (178 . ((text . ("Pointer to datasette buffer, Low Byte"))))
    (179 . ((text . ("Pointer to datasette buffer, High Byte"))))
    (193 . ((text . ("(Low Byte) Start address during SAVE to serial bus, LOAD and VERIFY from datasette and SAVE to datasette." "OR" "Pointer to current byte during memory test."))))
    (194 . ((text . ("(High Byte) Start address during SAVE to serial bus, LOAD and VERIFY from datasette and SAVE to datasette." "OR" "Pointer to current byte during memory test."))))
    (195 . ((text . ("(Low Byte) Start address for a secondary address of 0 for LOAD and VERIFY from serial bus or datasette." "OR" "Pointer to ROM table of default vectors during initialization of I/O vectors."))))
    (196 . ((text . ("(High Byte) Start address for a secondary address of 0 for LOAD and VERIFY from serial bus or datasette." "OR" "Pointer to ROM table of default vectors during initialization of I/O vectors."))))
    (197 . ((text . ("Matrix code of key previously pressed. Values:" "  $00-$3F = Keyboard matrix code." "  $40     = No key was pressed at the time of previous check."))))
    (198 . ((text . ("Length of keyboard buffer"))))
    (199 . ((text . ("Reverse mode switch. Values:" " $00 = Normal mode." " $12: Reverse mode."))))
    (201 . ((text . ("Cursor row during screen input. Values: $00-$18, 0-24."))))
    (202 . ((text . ("Cursor column during screen input. Values: $00-$27, 0-39."))))
    (203 . ((text . ("Matrix code of key currently being pressed. Values:" "  $00-$3F = Keyboard matrix code." "  $40     = No key is currently pressed."))))
    (204 . ((text . ("Cursor visibility switch. Values:" " $00 = Cursor is on." " $01-$FF: Cursor is off."))))
    (205 . ((text . ("Delay counter for changing cursor phase. Values:" " $00 = 0: Must change cursor phase." " $01-$14 = 1-20: Delay."))))
    (207 . ((text . ("Cursor phase switch. Values:" " $00 = Cursor off phase, original character visible" " $01 = Cursor on phase, reverse character visible"))))
    (208 . ((text . ("End of line switch during screen input. Values:" " $00 = Return character reached, end of line." " $01-$FF = Still reading characters from line."))))
    (209 . ((text . ("(Low byte) Pointer to current line in screen memory"))))
    (210 . ((text . ("(Low byte) Pointer to current line in screen memory"))))
    (211 . ((text . ("Current cursor column. Values: $00-$27, 0-39."))))
    (212 . ((text . ("Quotation mode switch. Values:" " $00 = Normal mode." " $01 = Quotation mode."))))
    (214 . ((text . ("Current cursor row. Values: $00-$18, 0-24."))))
    (215 . ((text . ("PETSCII code of character during screen input/output." "Bit buffer during datasette input." "Block checksum during datasette output."))))
    (216 . ((text . ("Number of insertions. Values:" " $00: No insertions made, normal mode, control codes change screen layout or behavior." " $01-$FF: Number of insertions, when inputting this many character next, those must be turned into control codes, similarly to quotation mode."))))
    (217 . ((text . ("High Byte of pointer to screen line 1.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (218 . ((text . ("High Byte of pointer to screen line 2.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (219 . ((text . ("High Byte of pointer to screen line 3.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (220 . ((text . ("High Byte of pointer to screen line 4.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (221 . ((text . ("High Byte of pointer to screen line 5.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (222 . ((text . ("High Byte of pointer to screen line 6.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (223 . ((text . ("High Byte of pointer to screen line 7.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (224 . ((text . ("High Byte of pointer to screen line 8.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (225 . ((text . ("High Byte of pointer to screen line 9.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (226 . ((text . ("High Byte of pointer to screen line 10.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (227 . ((text . ("High Byte of pointer to screen line 11.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (228 . ((text . ("High Byte of pointer to screen line 12.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (229 . ((text . ("High Byte of pointer to screen line 13.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (230 . ((text . ("High Byte of pointer to screen line 14.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (231 . ((text . ("High Byte of pointer to screen line 15.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (232 . ((text . ("High Byte of pointer to screen line 16.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (233 . ((text . ("High Byte of pointer to screen line 17.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (234 . ((text . ("High Byte of pointer to screen line 18.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (235 . ((text . ("High Byte of pointer to screen line 19.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (236 . ((text . ("High Byte of pointer to screen line 20.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (237 . ((text . ("High Byte of pointer to screen line 21.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (238 . ((text . ("High Byte of pointer to screen line 22.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (239 . ((text . ("High Byte of pointer to screen line 23.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (240 . ((text . ("High Byte of pointer to screen line 24.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (241 . ((text . ("High Byte of pointer to screen line 25.
Values:
 $00-$7f = Pointer high byte.
 $80-$ff = No pointer. line is an extension of previous line on screen"))))
    (245 . ((text . ("(Low Byte) Pointer to current conversion table during conversion from keyboard matrix codes to PETSCII codes."))))
    (246 . ((text . ("(High Byte) Pointer to current conversion table during conversion from keyboard matrix codes to PETSCII codes."))))
    (255 . ((text . ("Buffer for conversion from floating point to string (12 bytes.)"))))
    (256 . ((text . ("Bottom of Processor Stack Area ($0100-$01ff)"))))
    (641 . ((text . ("(High Byte) Pointer to beginning of BASIC area after memory test." "Default: $0800)"))))
    (642 . ((text . ("(Low Byte) Pointer to beginning of BASIC area after memory test." "Default: $0800)"))))
    (646 . ((text . ("Current color, cursor color. Values: $00-$0F, 0-15."))))
    (648 . ((text . ("High byte of pointer to screen memory for screen input/output." "Default: $04, $0400, 1024."))))
    (649 . ((text . ("Maximum length of keyboard buffer. Values:" " $00 = No buffer" " $01-$0f = 1-15, buffer size"))))
    (651 . ((text . ("Delay counter during repeat sequence, for delaying between successive repeats. Values:" " $00 = 0: Must repeat key." " $01-$04, 1-4: Delay repetition."))))
    (652 . ((text . ("Repeat sequence delay counter, for delaying before first repetition. Values:" " $00 = 0: Must start repeat sequence." " $01-$10, 1-16: Delay repeat sequence."))))
    (653 . ((text . ("Shift key indicator. Bits:" "    - Bit #0: 1 = One or more of left Shift, right Shift or Shift Lock is currently being pressed or locked." "    - Bit #1: 1 = Commodore is currently being pressed." "    - Bit #2: 1 = Control is currently being pressed."))))
    (654 . ((text . ("Previous value of shift key indicator. Bits:" "    - Bit #0: 1 = One or more of left Shift, right Shift or Shift Lock was pressed or locked at the time of previous check." "    - Bit #1: 1 = Commodore was pressed at the time of previous check." "    - Bit #2: 1 = Control was pressed at the time of previous check."))))
    (655 . ((text . ("(Low Byte) Execution address of routine that, based on the status of shift keys, sets" "the pointer at memory address $00F5-$00F6 to the appropriate conversion table for converting" "keyboard matrix codes to PETSCII codes." "(Default: $eb48)"))))
    (656 . ((text . ("(High Byte) Execution address of routine that, based on the status of shift keys, sets" "the pointer at memory address $00F5-$00F6 to the appropriate conversion table for converting" "keyboard matrix codes to PETSCII codes." "(Default: $eb48)"))))
    (658 . ((text . ("Scroll direction switch during scrolling the screen. Values:" " $00 = Insertion of line before current line, current line and all lines below it must be scrolled 1 line downwards." " $01-$FF = Bottom of screen reached, complete screen must be scrolled 1 line upwards."))))
    (678 . ((text . ("PAL/NTSC switch, for selecting RS232 baud rate from the proper table. Values:" "  $00 = NTSC." "  $01 = PAL."))))
    (788 . ((text . ("(Low Byte) Execution address of interrupt service routine." "(Default: $ea31)"))))
    (789 . ((text . ("(High Byte) Execution address of interrupt service routine." "(Default: $ea31)"))))
    (790 . ((text . ("(Low Byte) Execution address of BRK service routine." "(Default: $fe66)"))))
    (791 . ((text . ("(High Byte) Execution address of BRK service routine." "(Default: $fe66)"))))
    (792 . ((text . ("(Low Byte) Execution address of non-maskable interrupt service routine." "(Default: $fe47)"))))
    (793 . ((text . ("(High Byte) Execution address of non-maskable interrupt service routine." "(Default: $fe47)"))))
    (804 . ((text . ("(Low Byte) Execution address of CHRIN." "Data input routine, except for keyboard and RS232 input" "(Default: $f157)"))))
    (805 . ((text . ("(High Byte) Execution address of CHRIN." "Data input routine, except for keyboard and RS232 input" "(Default: $f157)"))))
    (806 . ((text . ("(Low Byte) Execution address of CHROUT." "General purpose data output routine" "(Default: $f1ca)"))))
    (807 . ((text . ("(High Byte) Execution address of CHROUT." "General purpose data output routine" "(Default: $f1ca)"))))
    (808 . ((text . ("(Low Byte) Execution address of STOP" "(Default: $f6ed)"))))
    (809 . ((text . ("(High Byte) Execution address of STOP" "(Default: $f6ed)"))))
    (1024 . ((text . ("Start of Default Area of Screen Memory"))))
    (2024 . ((text . ("Unused (16 bytes)"))))
    (2040 . ((text . ("Default area for sprite pointers (8 bytes)."))))
    (2048 . ((text . ("Unused. (Must contain a value of 0 so that the BASIC program can be RUN.)"))))
    (2049 . ((text . ("Default BASIC area (38911 bytes)."))))
    (32768 . ((text . ("Optional cartridge ROM (8192) bytes)." "$8000-$8001, 32768-32769: Execution address of cold reset." "$8002-$8003, 32770-32771: Execution address of non-maskable interrupt service routine." "$8004-$8008, 32772-32776: Cartridge signature. If contains the uppercase PETSCII string \"CBM80\" ($C3,$C2,$CD,$38,$30) then the routine vectors are accepted by the KERNAL."))))
    (40960 . ((text . ("BASIC ROM or RAM area (8192 bytes); depends on the value of bits #0-#2 of the processor port at memory address $0001:" "%x00, %x01 or %x10: RAM area." "%x11: BASIC ROM."))))
    (41846 . ((text . ("BASIC_ROM" "First byte of \"READY\""))))
    (49152 . ((text . ("Upper RAM area (4096 bytes)."))))
    (53248 . ((text . ("Sprite #0 X-coordinate (bits 0-7)"))))
    (53249 . ((text . ("Sprite #0 Y-coordinate"))))
    (53250 . ((text . ("Sprite #1 X-coordinate (bits 0-7)"))))
    (53251 . ((text . ("Sprite #1 Y-coordinate"))))
    (53252 . ((text . ("Sprite #2 X-coordinate (bits 0-7)"))))
    (53253 . ((text . ("Sprite #2 Y-coordinate"))))
    (53254 . ((text . ("Sprite #3 X-coordinate (bits 0-7)"))))
    (53255 . ((text . ("Sprite #3 Y-coordinate"))))
    (53256 . ((text . ("Sprite #4 X-coordinate (bits 0-7)"))))
    (53257 . ((text . ("Sprite #4 Y-coordinate"))))
    (53258 . ((text . ("Sprite #5 X-coordinate (bits 0-7)"))))
    (53259 . ((text . ("Sprite #5 Y-coordinate"))))
    (53260 . ((text . ("Sprite #6 X-coordinate (bits 0-7)"))))
    (53261 . ((text . ("Sprite #6 Y-coordinate"))))
    (53262 . ((text . ("Sprite #7 X-coordinate (bits 0-7)"))))
    (53263 . ((text . ("Sprite #7 Y-coordinate"))))
    (53264 . ((text . ("Sprite #0-7 X-coordinates (bit 8)"))))
    (53265 . ((text . ("Screen Control Register. Bits:" "    - Bits #0-#2 - Vertical Raster Scroll" "    - Bit #3     - 0 = 24 row screen, 1 = 25 row screen" "    - Bit #4     - 0 = Screen off, complete screen is covered by border" "    - Bit #5     - 0 = Text Mode, 1 = Bitmap Mode" "    - Bit #6     - 1 = Extended background mode on" "    - Bit #7     - Read: Current raster line, bit #8" "                   Write: Raster line to generate interrupt at, bit #8"))))
    (53266 . ((text . ("Read: Current raster line (bits #0-#7)." "Write: Raster line to generate interrupt at (bits #0-#7)."))))
    (53269 . ((text . ("Sprite enable register. Bits:" "    - Bit #x: 1 = Sprite #x is enabled, drawn onto the screen."))))
    (53270 . ((text . ("Screen control register #2. Bits:" "    - Bits #0-#2: Horizontal raster scroll." "    - Bit #3: Screen width; 0 = 38 columns; 1 = 40 columns." "    - Bit #4: 1 = Multicolor mode on."))))
    (53271 . ((text . ("Sprite double height register. Bits:" "    - Bit #x: 0 = Sprite #x is of regular height; 1 = Sprite #x is stretched to double height"))))
    (53272 . ((text . ("Memory setup register. Bits:" "* Bits #1-#3: In text mode, pointer to character memory (bits #11-#13), relative to VIC bank, memory address $DD00. Values:" "    - %000: 0 - $0000-$07FF, 0-2047." "    - %001: 1 - $0800-$0FFF, 2048-4095." "    - %010: 2 - $1000-$17FF, 4096-6143." "    - %011: 3 - $1800-$1FFF, 6144-8191." "    - %100: 4 - $2000-$27FF, 8192-10239." "    - %101: 5 - $2800-$2FFF, 10240-12287." "    - %110: 6 - $3000-$37FF, 12288-14335." "    - %111: 7 - $3800-$3FFF, 14336-16383." "     Values %010 and %011 in VIC bank #0 and #2 select Character ROM instead." "     In bitmap mode, pointer to bitmap memory (bit #13), relative to VIC bank, memory address $DD00. Values:" "    - %0xx: 0 - $0000-$1FFF, 0-8191." "    - %1xx: 4 - $2000-$3FFF, 8192-16383." "" "* Bits #4-#7: Pointer to screen memory (bits #10-#13), relative to VIC bank, memory address $DD00. Values:" "    - %0000: 0  - $0000-$03FF, 0-1023." "    - %0001: 1  - $0400-$07FF, 1024-2047." "    - %0010: 2  - $0800-$0BFF, 2048-3071." "    - %0011: 3  - $0C00-$0FFF, 3072-4095." "    - %0100: 4  - $1000-$13FF, 4096-5119." "    - %0101: 5  - $1400-$17FF, 5120-6143." "    - %0110: 6  - $1800-$1BFF, 6144-7167." "    - %0111: 7  - $1C00-$1FFF, 7168-8191." "    - %1000: 8  - $2000-$23FF, 8192-9215." "    - %1001: 9  - $2400-$27FF, 9216-10239." "    - %1010: 10 - $2800-$2BFF, 10240-11263." "    - %1011: 11 - $2C00-$2FFF, 11264-12287." "    - %1100: 12 - $3000-$33FF, 12288-13311." "    - %1101: 13 - $3400-$37FF, 13312-14335." "    - %1110: 14 - $3800-$3BFF, 14336-15359." "    - %1111: 15 - $3C00-$3FFF, 15360-16383."))))
    (53273 . ((text . ("Interrupt status register. Read bits:" "    - Bit #0: 1 = Current raster line is equal to the raster line to generate interrupt at." "    - Bit #1: 1 = Sprite-background collision occurred." "    - Bit #2: 1 = Sprite-sprite collision occurred." "    - Bit #3: 1 = Light pen signal arrived." "    - Bit #7: 1 = An event (or more events), that may generate an interrupt, occurred and it has not been (not all of them have been) acknowledged yet." "Write bits:" "    - Bit #0: 1 = Acknowledge raster interrupt." "    - Bit #1: 1 = Acknowledge sprite-background collision interrupt." "    - Bit #2: 1 = Acknowledge sprite-sprite collision interrupt." "    - Bit #3: 1 = Acknowledge light pen interrupt."))))
    (53274 . ((text . ("Interrupt control register. Bits:" "    - Bit #0: 1 = Raster interrupt enabled." "    - Bit #1: 1 = Sprite-background collision interrupt enabled." "    - Bit #2: 1 = Sprite-sprite collision interrupt enabled." "    - Bit #3: 1 = Light pen interrupt enabled."))))
    (53275 . ((text . ("Sprite priority register. Bits:" "    - Bit #x: 0 = Sprite #x is drawn in front of screen contents; 1 = Sprite #x is drawn behind screen contents"))))
    (53276 . ((text . ("Sprite multicolor mode register. Bits:" "    - Bit #x: 0 = Sprite #x is single color; 1 = Sprite #x is multicolor"))))
    (53277 . ((text . ("Sprite double width register. Bits:" "    - Bit #x: 0 = Sprite #x is regular width; 1 = Sprite #x is stretched to double width"))))
    (53280 . ((text . ("Screen Border Color (only bits #0-#3)"))))
    (53281 . ((text . ("Screen Background Color (only bits #0-#3)"))))
    (53282 . ((text . ("Extra Background Color 1 (only bits #0-#3)"))))
    (53283 . ((text . ("Extra Background Color 2 (only bits #0-#3)"))))
    (53284 . ((text . ("Extra Background Color 3 (only bits #0-#3)"))))
    (53285 . ((text . ("Sprite Extra Color 1 (only bits #0-#3)"))))
    (53286 . ((text . ("Sprite Extra Color 2 (only bits #0-#3)"))))
    (53287 . ((text . ("Sprite #0 Color (only bits #0-#3)"))))
    (53288 . ((text . ("Sprite #1 Color (only bits #0-#3)"))))
    (53289 . ((text . ("Sprite #2 Color (only bits #0-#3)"))))
    (53290 . ((text . ("Sprite #3 Color (only bits #0-#3)"))))
    (53291 . ((text . ("Sprite #4 Color (only bits #0-#3)"))))
    (53292 . ((text . ("Sprite #5 Color (only bits #0-#3)"))))
    (53293 . ((text . ("Sprite #6 Color (only bits #0-#3)"))))
    (53294 . ((text . ("Sprite #7 Color (only bits #0-#3)"))))
    (54272 . ((text . ("SID - Voice #1 Frequency, low byte"))))
    (54273 . ((text . ("SID - Voice #1 Frequency, high byte"))))
    (54274 . ((text . ("SID - Voice #1 Pulse Wave Duty Cycle, low byte"))))
    (54275 . ((text . ("SID - Voice #1 Pulse Wave Duty Cycle, high byte" "(Bits #0-3 only)"))))
    (54276 . ((text . ("SID - Voice #1 Control Register" "    - Bit #0: 0 = Voice off, Release cycle; 1 = Voice on, Attack-Decay-Sustain cycle." "    - Bit #1: 1 = Synchronization enabled." "    - Bit #2: 1 = Ring modulation enabled." "    - Bit #3: 1 = Disable voice, reset noise generator." "    - Bit #4: 1 = Triangle waveform enabled." "    - Bit #5: 1 = Saw waveform enabled." "    - Bit #6: 1 = Rectangle waveform enabled." "    - Bit #7: 1 = Noise enabled." " Write-only."))))
    (54277 . ((text . ("SID - Voice #1 Attack and Decay length. Bits:" "" "Bits #0-#3: Decay length. Values:" " - %0000, 0: 6 ms." " - %0001, 1: 24 ms." " - %0010, 2: 48 ms." " - %0011, 3: 72 ms." " - %0100, 4: 114 ms." " - %0101, 5: 168 ms." " - %0110, 6: 204 ms." " - %0111, 7: 240 ms." " - %1000, 8: 300 ms." " - %1001, 9: 750 ms." " - %1010, 10: 1.5 s." " - %1011, 11: 2.4 s." " - %1100, 12: 3 s." " - %1101, 13: 9 s." " - %1110, 14: 15 s." " - %1111, 15: 24 s." "" "Bits #4-#7: Attack length. Values:" " - %0000, 0: 2 ms." " - %0001, 1: 8 ms." " - %0010, 2: 16 ms." " - %0011, 3: 24 ms." " - %0100, 4: 38 ms." " - %0101, 5: 56 ms." " - %0110, 6: 68 ms." " - %0111, 7: 80 ms." " - %1000, 8: 100 ms." " - %1001, 9: 250 ms." " - %1010, 10: 500 ms." " - %1011, 11: 800 ms." " - %1100, 12: 1 s." " - %1101, 13: 3 s." " - %1110, 14: 5 s." " - %1111, 15: 8 s." "" "Write-only."))))
    (54278 . ((text . ("SID - Voice #1 Sustain Volume and Release length. Bits:" "" "Bits #0-#3: Release length. Values:" " - %0000, 0: 6 ms." " - %0001, 1: 24 ms." " - %0010, 2: 48 ms." " - %0011, 3: 72 ms." " - %0100, 4: 114 ms." " - %0101, 5: 168 ms." " - %0110, 6: 204 ms." " - %0111, 7: 240 ms." " - %1000, 8: 300 ms." " - %1001, 9: 750 ms." " - %1010, 10: 1.5 s." " - %1011, 11: 2.4 s." " - %1100, 12: 3 s." " - %1101, 13: 9 s." " - %1110, 14: 15 s." " - %1111, 15: 24 s." "" "Bits #4-#7: Sustain volume." "" "Write-only."))))
    (54283 . ((text . ("SID - Voice #2 Control Register" "    - Bit #0: 0 = Voice off, Release cycle; 1 = Voice on, Attack-Decay-Sustain cycle." "    - Bit #1: 1 = Synchronization enabled." "    - Bit #2: 1 = Ring modulation enabled." "    - Bit #3: 1 = Disable voice, reset noise generator." "    - Bit #4: 1 = Triangle waveform enabled." "    - Bit #5: 1 = Saw waveform enabled." "    - Bit #6: 1 = Rectangle waveform enabled." "    - Bit #7: 1 = Noise enabled." " Write-only."))))
    (54285 . ((text . ("SID - Voice #2 Sustain Volume and Release length. Bits:" "" "Bits #0-#3: Release length. Values:" " - %0000, 0: 6 ms." " - %0001, 1: 24 ms." " - %0010, 2: 48 ms." " - %0011, 3: 72 ms." " - %0100, 4: 114 ms." " - %0101, 5: 168 ms." " - %0110, 6: 204 ms." " - %0111, 7: 240 ms." " - %1000, 8: 300 ms." " - %1001, 9: 750 ms." " - %1010, 10: 1.5 s." " - %1011, 11: 2.4 s." " - %1100, 12: 3 s." " - %1101, 13: 9 s." " - %1110, 14: 15 s." " - %1111, 15: 24 s." "" "Bits #4-#7: Sustain volume." "" "Write-only."))))
    (54286 . ((text . ("SID - Voice #3" "(Low Byte) Voice frequency"))))
    (54287 . ((text . ("SID - Voice #3" "(High Byte) Voice frequency"))))
    (54290 . ((text . ("SID - Voice #3 Control Register" "    - Bit #0: 0 = Voice off, Release cycle; 1 = Voice on, Attack-Decay-Sustain cycle." "    - Bit #1: 1 = Synchronization enabled." "    - Bit #2: 1 = Ring modulation enabled." "    - Bit #3: 1 = Disable voice, reset noise generator." "    - Bit #4: 1 = Triangle waveform enabled." "    - Bit #5: 1 = Saw waveform enabled." "    - Bit #6: 1 = Rectangle waveform enabled." "    - Bit #7: 1 = Noise enabled." " Write-only."))))
    (54292 . ((text . ("SID - Voice #3 Sustain Volume and Release length. Bits:" "" "Bits #0-#3: Release length. Values:" " - %0000, 0: 6 ms." " - %0001, 1: 24 ms." " - %0010, 2: 48 ms." " - %0011, 3: 72 ms." " - %0100, 4: 114 ms." " - %0101, 5: 168 ms." " - %0110, 6: 204 ms." " - %0111, 7: 240 ms." " - %1000, 8: 300 ms." " - %1001, 9: 750 ms." " - %1010, 10: 1.5 s." " - %1011, 11: 2.4 s." " - %1100, 12: 3 s." " - %1101, 13: 9 s." " - %1110, 14: 15 s." " - %1111, 15: 24 s." "" "Bits #4-#7: Sustain volume." "" "Write-only."))))
    (54295 . ((text . ("SID" "Filter control. Bits:" "    - Bit #0: 1 = Voice #1 filtered." "    - Bit #1: 1 = Voice #2 filtered." "    - Bit #2: 1 = Voice #3 filtered." "    - Bit #3: 1 = External voice filtered." "    - Bits #4-#7: Filter resonance." "Write-only."))))
    (54296 . ((text . ("SID" "Volume and filter modes. Bits:" "    - Bits #0-#3: Volume." "    - Bit #4: 1 = Low pass filter enabled." "    - Bit #5: 1 = Band pass filter enabled." "    - Bit #6: 1 = High pass filter enabled." "    - Bit #7: 1 = Voice #3 disabled." "Write-only."))))
    (56320 . ((text . ("CIA #1" "Port A, keyboard matrix columns and joystick #2. Read bits:" "    - Bit #0: 0 = Port 2 joystick up pressed." "    - Bit #1: 0 = Port 2 joystick down pressed." "    - Bit #2: 0 = Port 2 joystick left pressed." "    - Bit #3: 0 = Port 2 joystick right pressed." "    - Bit #4: 0 = Port 2 joystick fire pressed." "Write bits:" "    - Bit #x: 0 = Select keyboard matrix column #x." "    - Bits #6-#7: Paddle selection; %01 = Paddle #1; %10 = Paddle #2."))))
    (56322 . ((text . ("CIA #1" "Port A data direction register." "" "    - Bit #x: 0 = Bit #x in port A can only be read; 1 = Bit #x in port A can be read and written."))))
    (56323 . ((text . ("CIA #1" "Port B data direction register." "" "    - Bit #x: 0 = Bit #x in port B can only be read; 1 = Bit #x in port B can be read and written."))))
    (56324 . ((text . ("CIA #1" "Timer A." "   Read: Current timer value." "   Write: Set timer start value."))))
    (56333 . ((text . ("CIA #1" "Interrupt control and status register. Read bits:" "    - Bit #0: 1 = Timer A underflow occurred." "    - Bit #1: 1 = Timer B underflow occurred." "    - Bit #2: 1 = TOD is equal to alarm time." "    - Bit #3: 1 = A complete byte has been received into or sent from serial shift register." "    - Bit #4: Signal level on FLAG pin, datasette input." "    - Bit #7: An interrupt has been generated." "" "Write bits:" "    - Bit #0: 1 = Enable interrupts generated by timer A underflow." "    - Bit #1: 1 = Enable interrupts generated by timer B underflow." "    - Bit #2: 1 = Enable TOD alarm interrupt." "    - Bit #3: 1 = Enable interrupts generated by a byte having been received/sent via serial shift register." "    - Bit #4: 1 = Enable interrupts generated by positive edge on FLAG pin." "    - Bit #7: Fill bit; bits #0-#6, that are set to 1, get their values from this bit; bits #0-#6, that are set to 0, are left unchanged."))))
    (56334 . ((text . ("CIA #1" "Timer A control register. Bits:" "    - Bit #0: 0 = Stop timer; 1 = Start timer." "    - Bit #1: 1 = Indicate timer underflow on port B bit #6." "    - Bit #2: 0 = Upon timer underflow, invert port B bit #6; 1 = upon timer underflow, generate a positive edge on port B bit #6 for 1 system cycle." "    - Bit #3: 0 = Timer restarts upon underflow; 1 = Timer stops upon underflow." "    - Bit #4: 1 = Load start value into timer." "    - Bit #5: 0 = Timer counts system cycles; 1 = Timer counts positive edges on CNT pin." "    - Bit #6: Serial shift register direction; 0 = Input, read; 1 = Output, write." "    - Bit #7: TOD speed; 0 = 60 Hz; 1 = 50 Hz."))))
    (56335 . ((text . ("CIA #1" "Timer B control register. Bits:" "    - Bit #0: 0 = Stop timer; 1 = Start timer." "    - Bit #1: 1 = Indicate timer underflow on port B bit #7." "    - Bit #2: 0 = Upon timer underflow, invert port B bit #7; 1 = upon timer underflow, generate a positive edge on port B bit #7 for 1 system cycle." "    - Bit #3: 0 = Timer restarts upon underflow; 1 = Timer stops upon underflow." "    - Bit #4: 1 = Load start value into timer." "    - Bits #5-#6: %00 = Timer counts system cycles; %01 = Timer counts positive edges on CNT pin; %10 = Timer counts underflows of timer A; %11 = Timer counts underflows of timer A occurring along with a positive edge on CNT pin." "    - Bit #7: 0 = Writing into TOD registers sets TOD; 1 = Writing into TOD registers sets alarm time."))))
    (56576 . ((text . ("CIA #2" "Port A, serial bus access. Bits:" "    - Bits #0-#1: VIC bank. Values:" "       %00, 0: Bank #3, $C000-$FFFF, 49152-65535." "       %01, 1: Bank #2, $8000-$BFFF, 32768-49151." "       %10, 2: Bank #1, $4000-$7FFF, 16384-32767." "       %11, 3: Bank #0, $0000-$3FFF, 0-16383." "    - Bit #2: RS232 TXD line, output bit." "    - Bit #3: Serial bus ATN OUT; 0 = High; 1 = Low." "    - Bit #4: Serial bus CLOCK OUT; 0 = High; 1 = Low." "    - Bit #5: Serial bus DATA OUT; 0 = High; 1 = Low." "    - Bit #6: Serial bus CLOCK IN; 0 = Low; 1 = High." "    - Bit #7: Serial bus DATA IN; 0 = Low; 1 = High."))))
    (56578 . ((text . ("CIA #2" "Port A data direction register." "" "    - Bit #x: 0 = Bit #x in port A can only be read; 1 = Bit #x in port A can be read and written."))))
    (56579 . ((text . ("CIA #2" "Port B data direction register." "" "    - Bit #x: 0 = Bit #x in port B can only be read; 1 = Bit #x in port B can be read and written."))))
    (56589 . ((text . ("CIA #2" "Interrupt control and status register. Read bits:" "    - Bit #0: 1 = Timer A underflow occurred." "    - Bit #1: 1 = Timer B underflow occurred." "    - Bit #2: 1 = TOD is equal to alarm time." "    - Bit #3: 1 = A complete byte has been received into or sent from serial shift register." "    - Bit #4: Signal level on FLAG pin." "    - Bit #7: A non-maskable interrupt has been generated." "" "Write bits:" "    - Bit #0: 1 = Enable non-maskable interrupts generated by timer A underflow." "    - Bit #1: 1 = Enable non-maskable interrupts generated by timer B underflow." "    - Bit #2: 1 = Enable TOD alarm non-maskable interrupt." "    - Bit #3: 1 = Enable non-maskable interrupts generated by a byte having been received/sent via serial shift register." "    - Bit #4: 1 = Enable non-maskable interrupts generated by positive edge on FLAG pin." "    - Bit #7: Fill bit; bits #0-#6, that are set to 1, get their values from this bit; bits #0-#6, that are set to 0, are left unchanged."))))
    (56590 . ((text . ("CIA #2" "Timer A control register. Bits:" "    - Bit #0: 0 = Stop timer; 1 = Start timer." "    - Bit #1: 1 = Indicate timer underflow on port B bit #6." "    - Bit #2: 0 = Upon timer underflow, invert port B bit #6; 1 = upon timer underflow, generate a positive edge on port B bit #6 for 1 system cycle." "    - Bit #3: 0 = Timer restarts upon underflow; 1 = Timer stops upon underflow." "    - Bit #4: 1 = Load start value into timer." "    - Bit #5: 0 = Timer counts system cycles; 1 = Timer counts positive edges on CNT pin." "    - Bit #6: Serial shift register direction; 0 = Input, read; 1 = Output, write." "    - Bit #7: TOD speed; 0 = 60 Hz; 1 = 50 Hz."))))
    (56591 . ((text . ("CIA #1" "Timer B control register. Bits:" "    - Bit #0: 0 = Stop timer; 1 = Start timer." "    - Bit #1: 1 = Indicate timer underflow on port B bit #7." "    - Bit #2: 0 = Upon timer underflow, invert port B bit #7; 1 = upon timer underflow, generate a positive edge on port B bit #7 for 1 system cycle." "    - Bit #3: 0 = Timer restarts upon underflow; 1 = Timer stops upon underflow." "    - Bit #4: 1 = Load start value into timer." "    - Bits #5-#6: %00 = Timer counts system cycles; %01 = Timer counts positive edges on CNT pin; %10 = Timer counts underflows of timer A; %11 = Timer counts underflows of timer A occurring along with a positive edge on CNT pin." "    - Bit #7: 0 = Writing into TOD registers sets TOD; 1 = Writing into TOD registers sets alarm time."))))
    (58464 . ((text . ("KERNAL ROM" "First byte of \"BASIC BYTES FREE\""))))
    (58483 . ((text . ("KERNAL ROM" "First byte of \"**** COMMODORE 64 BASIC V2 ****\""))))
    (65424 . ((text . ("KERNAL ROM" "Jump Vector for SETMSG (I/O)"))))
    (65466 . ((text . ("KERNAL ROM" "Jump Vector for init_file_p - Initialize File Parameters (I/O)"))))
    (65469 . ((text . ("KERNAL ROM" "Jump Vector for init_file_np - Initialize File Name Parameters (I/O)"))))))

(defun 64tass--to-addr (addr-int)
  "Format address integer ADDR-INT  as $address or $zeropage."
  (if (< addr-int 256)
      (format "$%02x" addr-int)
    (format "$%04x" addr-int)))

(defun 64tass--lookup-memory-doc (address)
  "Return memory documentation entry for ADDRESS.
Looks for exact match, or a range between keys that contains ADDRESS."
  (let ((exact-match (alist-get address c64-memory-map)))
    (cond
     ((> address 65535)
      nil)

     (exact-match
      (list :address (64tass--to-addr address)
            :text (alist-get 'text exact-match)))

     (t
      (let ((index 0)
            (end-entry nil))
        (while (not end-entry)
          (let ((entry (nth index c64-memory-map)))
            (when (> (car entry) address)
              (setq end-entry entry)))
          (setq index (1+ index)))
        (let ((start-entry (nth (- index 2) c64-memory-map)))
          (if end-entry
              (list :address (64tass--to-addr address)
                    :range (cons (64tass--to-addr (car start-entry))
                                 (64tass--to-addr (1- (car end-entry))))
                    :text (alist-get 'text (cdr start-entry)))
            nil)))))))

(provide '64tass-mem)

;;; 64tass-mem.el ends here
