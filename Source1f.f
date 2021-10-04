        REAL AR ,GS , NPS ,TI ,LIC ,HRA1,BP,DA,ARP
      WRITE (*,*) ' put your gross salary '
      READ * , AR
      WRITE (*,*) 'put your arrear '
      READ * , GS
      TI = GS + AR
      WRITE (*,*) TI
      WRITE (*,*)  'put your investment in LIC,PLI '
      READ * , LIC , PLI
      write (*,*) ' put your investment in NPS,GIS '
      READ * , NPS , GIS
      WRITE (*,*) 'put your investment in PTAX  '
      READ * , PTAX
      WRITE (*,*) ' put your STDD '
      read * , STDD
      WRITE (*,*) ' put your basic pay, dearness allowance '
      read *, BP, DA
      WRITE (*,*) ' Annual Rent Paid ,House Rent Allowance '
      READ * , ARP , HRA1
      TS = BP + DA
      C = (BP +DA)* 0.1
      HRA2 = ARP - C
      HRA3 = (BP+DA) *0.4
      if  ( HRA1 .LT. HRA2 .AND. HRA1 .LT. HR3) THEN
19    HRA = HRA1
      ELSEIF (HRA2 .LT. HRA3 .AND. HRA2 .LT. HRA1) THEN
20    HRA = 0
      ELSEIF (HRA3 .LT. HRA2 .AND. HRA3 .LT. HRA1) THEN
30    HRA = 0
      ELSE
      HRA=HRA1
      ENDIF
      WRITE (*,*) HRA
      DEPOSIT = LIC+PLI+GIS+NPS
      IF (DEPOSIT .GE. 150000) then
15    DEPOSIT = 150000
      ELSE
16    DEPOSIT=DEPOSIT
      ENDIF
      WRITE (*,*) DEPOSIT
      AGG = (PTAX+STDD+HRA)+DEPOSIT
      WRITE (*,*) AGG
      ANI = TI-AGG
      WRITE (*,*) ANI
      if (ANI .GE. 0 .AND. ANI .LT. 250000) THEN
13    D=ANI*0
      ELSE IF (ANI .GE. 250000 .AND. ANI.LT. 500000) THEN
10    D = (ANI-250000) * 0.05
      ELSE IF (ANI .GE. 500001 .AND. ANI .LT. 1000000) THEN
11    D =  (ANI-750000) * 0.20 +25000
      ELSEIF (ANI .GE. 1000001 .AND. ANI .LT. 3000000) THEN
12    D= (ANI - 1750000)* 0.30 +25000+200000
      ELSE
      D=ANI
      ENDIF
      WRITE (* , *) ANI
      WRITE (* , *) D
      ATP = D * 0.04
      ATTP = ATP + D
      WRITE(*,*) ATTP
      STOP
      END

