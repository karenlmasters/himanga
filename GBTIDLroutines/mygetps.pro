PRO mygetps, StartNumber, Nrepeat

	unzoom
	freeze
   FOR i = 0, Nrepeat-1 DO BEGIN
       n = i*2 + StartNumber
           getps,n,plnum=0, units="Jy"
           accum
           getps,n,plnum=1, units="Jy"
           accum
   ENDFOR	
	unfreeze
	ave,/noclear

	XRange = getxrange()
	rezoom, XRange[0]+10, XRange[1]

END

PRO rfiblank
	setxunit,'Channels'
	default=300
	  Print, "Blanking data from channel=0 to channel=",default
	  replace,0,default,/blank
	  rezoom, default+1, 32768

	default2=32000
      	  Print, "Blanking data to channel=32768 from channel=",default2
	  replace,default2,32768,/blank
	  rezoom, default+1, default2-1


;	XRange = getxrange()
;	rezoom, XRange[0]+10, XRange[1]

; Allows user to mark beginning of spectrum (in channels) to be blanked out.
	rfiOK=0

	WHILE rfiOK EQ 0 DO BEGIN
          usr_input = " "
	  READ, "Want to blank out more beginning channels? (y/n) ", usr_input

          IF STRLOWCASE(usr_input) EQ "n" THEN rfiOK=1
	  IF STRLOWCASE(usr_input) EQ "y" THEN BEGIN
		  PRINT, "Click on channel to blank to: "
		  cb = click()
	  Print, "Blanking data from channel=0 to channel=",cb.x
	  replace,0,cb.x,/blank
	  rezoom, cb.x, 32768
	  ENDIF
       ENDWHILE
       
	WHILE rfiOK EQ 0 DO BEGIN
          usr_input = " "
	  READ, "Want to blank out more end channels? (y/n) ", usr_input

          IF STRLOWCASE(usr_input) EQ "n" THEN rfiOK=1
	  IF STRLOWCASE(usr_input) EQ "y" THEN BEGIN
		  PRINT, "Click on channel to blank from: "
		  cb = click()
	  Print, "Blanking data to end from channel=",cb.x
	  replace,cb.x,default2,/blank
	  rezoom, default,c.x
	  ENDIF
       ENDWHILE
       

;Allows user to mark position of one or more RFI spikes. Interpolates data for 2 channels on either side of marked position.

	rfiOK=0

	WHILE rfiOK EQ 0 DO BEGIN
          usr_input = " "
	  READ, "Do you want to mark the position of RFI spikes? (y/n)", usr_input
	
	  IF STRLOWCASE(usr_input) EQ "n" THEN rfiOK=1

	  IF STRLOWCASE(usr_input) EQ "y" THEN BEGIN
            PRINT, "Click on position of RFI to zoom: "
	    c = click()
	    xmin=c.x-100
	    if xmin LE 300 then xmin=301	
	    xmax = c.x+100
	    if xmax GE 32768 then xmax=32767
	    rezoom, xmin, xmax		
            PRINT, "Click on position of RFI: "
	    c = click()
            Print, "Interpolating data in +/- 10 channels around channel=",c.x
	    rfi=c.freq/1.e6
            Print, "RFI record",rfi
	    replace,c.x-10,c.x+10
	    unzoom
	    ;rezoom, cb.x, 8192
	  ENDIF

	ENDWHILE
	setxunit,'km/s'
END

PRO rfispike
; Allows user to click either side of an RFI spike to remove.

   PRINT, "Click either side of RFI: "
   c1 = click()
   c2 = click()
   r1 = c1.chan
   r2 = c2.chan
   replace, r1, r2
   print,'RFI spike: ',r1, r2

END



PRO ReZoom, xMin, xMax

  stats,/full,ret=mystats,/quiet

   dataX = getxarray()
   dataY = getyarray()

   ind = WHERE(dataX GE xMin AND dataX LE xMax)
   yMax = MAX(dataY(ind))
   yMin = MIN(dataY(ind))

   setxy, xMin, xMax, yMin-3*mystats.rms, yMax+10*mystats.rms
   
END

PRO Getpeak

   print, "Click on either side of profile, low vel side first"
   c1=click()
   c2=click()

   xMin=c1.x
   xMax=c2.x

   print, xMin, xMax

   dataX = getxarray()
   dataY = getyarray()

   ind = WHERE(dataX GE xMin AND dataX LE xMax)
   yMax = MAX(dataY(ind))

   print,"Peak is: ",yMax 
  
END


