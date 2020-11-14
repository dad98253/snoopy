      SUBROUTINE SNPIC
C     THIS SUBROUTINE WILL ANALYZE THE INPUT DATA AND PRINT A PICTURE
      implicit real*8 (a-h,o-z)
      DIMENSION ILINE(133),INUM(50),ICHR(50)
      character*1 AFORMFD
      COMMON ISET
      COMMON /CONFIG/ IPSSW
      DATA IBLNK/4H     /
      DATA AFORMFD/'\f'     /
      DATA IONE/4H1     /
      DATA IZERO/4H0     /
      DATA IPLUS/4H+     /
      DO 4 I=1,121
    4 ILINE(I)=IBLNK
      do 3 i=1,50
      INUM(i)=0
      ICHR(i)=0
3     continue
      K=1
   10 READ (2,1000) (INUM(I),ICHR(I),I=1,ISET)
      ijk=0
      DO 40 I = 1, ISET
      ijk=ijk+1
      IF (INUM(ijk)+1)100,11,100
C     HERE WE WRITE A LINE TO THE PRINTER AND GO BUILD ANOTHER
   11 DO 15 L = K, 121
   15 ILINE(L)=ICHR(ijk)
      IF ( IPSSW .NE. 0 ) THEN
       WRITE (3,2000) (ILINE(K),K=1,121)
      ELSE
       if(ILINE(1) .eq. IONE ) write(3,"(A)")AFORMFD
       if(ILINE(1) .eq. IZERO ) write(3,*)" "
       if(ILINE(1) .ne. IPLUS ) WRITE (3,2000) (ILINE(K),K=2,121)
      ENDIF
      ILINE(1)=IBLNK
      DO 20 K = 2, 121
   20 ILINE(K)=ICHR(ijk)
      K=1
c      I=I+1
      ijk=ijk+1
  100 ITST=INUM(ijk)
      IF (ITST + 2) 101,200,101
  101 IF (ITST) 102,40,102
  102 DO 30 J = 1, ITST
      ILINE(K)=ICHR(ijk)
      K=K+1
   30 CONTINUE
   40 CONTINUE
      GOTO 10
C     HERE WE EXIT THE PICTURE AND RETURN TO THE CALLING PROGRAM
C     ADDED A FORM FEED AFTER THE PICTURE (JCK - 11/14/2020)
  200 CONTINUE
      IF ( IPSSW .EQ. 0 ) THEN
       write(3,"(A)")AFORMFD
      ELSE
       WRITE(3,"(A)") "1"
      ENDIF
      RETURN
C     FORMAT STATEMENTS
 1000 FORMAT (1x,25(I2,A1))
 2000 FORMAT (121A1)
      END
c
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
C     * 							     *
C     * ALL PICTURE DATA DECKS MUST BE TERMINATED WITH CODE -2.      *
C     * CONSECUTIVE -2*S WILL RESULT IN NO PICTURE BEING PRINTED     *
C     * FOR THAT MONTH. 					     *
C     * 							     *
C     * PICTURE FORMAT CODES -- 				     *
C     *    -1	 END OF LINE					     *
C     *    -2	 END OF PICTURE 				     *
C     *    -3	 LIST CARDS, ONE PER LINE, FORMAT 13A6		     *
C     *    -4	 LIST CARDS, TWO PER LINE, FORMAT 11A6/11A6	     *
C     *    -5	 LIST CARDS, TWO PER LINE, FORMAT 12A6/10A6	     *
C     ****************************************************************
      implicit real*8 (a-h,o-z)
      DIMENSION AMNTH (12,7,13), ANAM(22), ANUM(2,10,5), NODS(12),
     1 CAL(60,22)
      CHARACTER*80 ALINE
      COMMON ISET
      COMMON /CONFIG/ IPSSW
      open(unit=2,file='input.dat',status='unknown')
      open(unit=3,file='output.dat',status='unknown')
      READ (2,1) (((AMNTH(I,J,K),K=1,13),J=1,7),I=1,12)
      READ (2,2) (ANAM(I),I=1,22)
      READ (2,3) (((ANUM(I,J,K),J=1,10),K=1,5),I=1,2)
      READ (2,4) (NODS(I),I=1,12)
      READ (2,1) BLANK,ONE,ALIN1,ALIN2,ALIN3,ALIN4
      READ (2,4) MF,IYR,MTLST,IYLST,LNSW,IPSSW
      IF ( IPSSW .EQ. 0 ) GO TO 10102
      PRINT *, "PRINTING POSTSCRIPT FORMAT"
      open(unit=4,file='psheader.dat',status='old')
10100 CONTINUE
      READ (4,'(A80)',END=10101) ALINE
      WRITE (3,'(A80)') ALINE
      GO TO 10100      
10101 CONTINUE
      CLOSE (4)
10102 CONTINUE            
      ISET=25
      DO 10 I=1,60
      DO 10 J=1,22
10    CAL(I,J)= BLANK
C      CAL(1,1)= ONE
      CAL(1,1)= BLANK
      DO 20 J=1,22
20    CAL(11,J)=ANAM(J)
      IF (LNSW) 122,142,122
122   DO 125 I=20,60,8
      DO 125 J=1,22
125   CAL(I,J)=ALIN2
      DO 140 J=4,19,3
      I=13
127   DO 130 L=1,7
      CAL(I,J)=ALIN1
130   I=I+1
      IF (I-55) 135,135,140
135   CAL(I,J)=ALIN3
      I=I+1
      GO TO 127
140   CONTINUE
      DO 141 I=20,60,8
141   CAL(I,1)=ALIN4
142   IDOW=(IYR-1751)+(IYR-1753)/4-(IYR-1701)/100+(IYR-1601)/400
      IDOW=IDOW-7*((IDOW-1)/7)
55    IF (IYR-IYLST) 60,65,100
60    ML=12
      GO TO 70
65    ML=MTLST
70    IY1=IYR/1000
      NUMB=IYR-1000*IY1
      IY2=NUMB/100
      NUMB=NUMB-100*IY2
      IY3=NUMB/10
      NUMB=NUMB-10*IY3
      IY4=NUMB
      DO 72 J=1,5
      CAL(J+3,2)=ANUM(2,IY1+1,J)
      CAL(J+1,3)=ANUM(2,IY2+1,J)
      CAL(J+1,19)=ANUM(2,IY3+1,J)
72    CAL(J+3,20)=ANUM(2,IY4+1,J)
      LPYSW=0
      IF (IYR-4*(IYR/4)) 90,75,90
75    IF (IYR-100*(IYR/100)) 85,80,85
80    IF (IYR-400*(IYR/400)) 90,85,90
85    LPYSW=1
90    NODS(2)=NODS(2)+LPYSW
      IF (MF-1) 100,110,95
95    MF=MF-1
      DO 105 MONTH=1,MF
105   IDOW=IDOW+NODS(MONTH)
      IDOW=IDOW-7*((IDOW-1)/7)
      MF=MF+1
110   DO 51 MONTH=MF,ML
      LSTDY=NODS(MONTH)
      DO 115 I=1,7
      DO 115 JM=1,13
      J=JM+4
115   CAL(I,J)=AMNTH(MONTH,I,JM)
      IF (IDOW-1) 160,160,120
120   ID=IDOW-1
      J=2
      DO 155 K=1,ID
      DO 150 I=14,18
      CAL (I,J)= BLANK
150   CAL(I,J+1)= BLANK
      J=J+3
155   CONTINUE
160   IDAY=1
      II=14
25    J=3*IDOW-1
      N=IDAY/10+1
      I=II
      DO 30 K=1,5
      CAL(I,J)=ANUM(1,N,K)
30    I=I+1
      N=IDAY-10*N+11
      J=J+1
      I=II
      DO 35 K=1,5
      CAL(I,J)=ANUM(2,N,K)
35    I=I+1
      IDOW=IDOW+1
      IF (IDOW-7) 45,45,40
40    IDOW=1
      II=II+8
45    IDAY=IDAY+1
      IF (IDAY-LSTDY) 25,25,50
50    ID=IDOW
205   I=II
      J=3*ID-1
      DO 210 K=1,5
      CAL(I,J)= BLANK
      CAL(I,J+1)= BLANK
210   I=I+1
      IF (ID-7) 215,220,220
215   ID=ID+1
      GO TO 205
220   IF (II-54) 225,230,230
225   II=54
      ID=1
      GO TO 205
230   CALL SNPIC
      WRITE (3,5) ((CAL(I,J),J=1,21),I=1,60)
51    CONTINUE
      IF (IYR-IYLST) 235,100,100
235   NODS(2)=NODS(2)-LPYSW
      IYR=IYR+1
      MF=1
      GO TO 55
100   continue
      if ( IPSSW .eq. 0 ) print *,
     +"print using this command: lpr -o cpi=17 -o lpi=12 output.dat"
     +," -- jck 7/1/2017"
      CALL EXIT
1     FORMAT (1x,13A6)
2     FORMAT (1x,11A6)
3     FORMAT (1x,10A6)
4     FORMAT (1x,12I6)
5     FORMAT (A1,20A6)
      END
