# snoopy
the original snoopy calendar circa 1960s


C     ****************************************************************
C     * IBM 1130 EMULATOR VERSION (KYM FARNIK)			     *
C     * 							     *
C     * COPYRIGHT - THIS IS CONSIDERED PUBLIC DOMAIN FOR THE	     *
C     * FOLLOWING REASON					     *
C     *    DURING THE 60'S AND 70'S THIS CODE (AND VARIATIONS)	     *
C     *    WAS WIDELY SHARED BETWEEN GROUPS SUCH AS SHARE/GUIDE(IBM) *
C     *    AND DECUS(DIGITAL) AND THE ORGINAL AUTHOR IS UNKNOWN AND  *
C     *    HAS NEVER ASSERTED ANY RIGHTS OVER THIS CODE.	     *
C     * 							     *
C     * THIS VERSION WAS MODIFIED FROM CODE SOURCED FROM DECUS	     *
C     * SEE: http://www.ibiblio.org/pub/academic/computer-science    *
C     * 	 /history/pdp-11/rsts/decus/sig87/087018/	     *
C     * WHICH IN TURN WAS SOURCED FROM AN IBM FORTRAN+BAL VERSION    *
C     * 							     *
C     * IBM 1130 MODS: (FORTRAN IV SUBSET)			     *
C     *   ADDED IBM 1130 JCL					     *
C     *   REMOVE OPEN/CLOSE					     *
C     *   CHANGED LOGIGAL IF TO ARITHMETIC IF			     *
C     *   CHANGED TO 5 CHARACTER VARIABLE NAMES 		     *
C     *   CHANGED TO 120 COLUMNS IBM 1132 PRINTER		     *
C     *   EXTENED PRECISION REAL (3*16 BIT WORDS)		     *
C     * 							     *
C     ****************************************************************
C     * PRINTS CALENDAR, ONE MONTH PER PAGE WITH PICTURES OPTIONAL.  *
C     * 							     *
C     * BEGINNING MONTH AND YEAR, ENDING MONTH AND YEAR MUST BE PRO- *
C     * VIDED IN 4(I6)	FORMAT ON A CARD IMMEDIATELY FOLLOWING	     *
C     * CARD 98 OF DECK.					     *
C     * 							     *
C     * IF GRID LINES ARE DESIRED, A 1 MUST APPEAR IN COLUMN 30 OF   *
C     * ABOVE CARD.  A BLANK OR ZERO WILL SUPPRESS GRID LINES.	     *

Modified Nov 2020 :
Added an option to create Postscript output (this looks much nicer on modern printers ...
it's hard to find line printers now days :( ...)
Just put a 1 in column 36 to turn on Postscript output. The psheader.dat file is Postscript
code that will be added to the top of the output file. The header includes a cute little
program that converts line printer formated output to landscape formatted output that fits
nicely on 8.5x11 inch paper. (sorry, you A4 types will just have to deal with it ;-)

To print the postscript output on linux, rename the output.dat file to output.ps. Then from 
the cli, enter : lpr output.ps
This was tested with a (postscript) xerox Workcenter 3225. But, should work on any postcript
printer.
