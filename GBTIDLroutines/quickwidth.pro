PRO quickwidth, peak, rms

   snr=peak/rms
   print, "S/N is ", snr

   print, "Mark the region of interest with the cursor (any mouse click)"
   c=click()
   xMin = round(c.chan)
   c=click()
   xMax = round(c.chan)
   


   print, "WM50"
   g=mygmeasure(1,0.5,rms=rms,ifirst=xMin,last=xMax)
   print, "WP50"
   g=mygmeasure(2,0.5,rms=rms,ifirst=xMin,last=xMax)
   wp50=g[1]
   print, "WP20"
   g=mygmeasure(2,0.2,rms=rms,ifirst=xMin,last=xMax)
   wp20=g[1]
   slope=0.3*peak/(wp20-wp50) 
   error=sqrt((rms/slope)^2 + 2.3901)
 
   print, "Estimate of error on V is: ", error
   print, "W2P50"
   g=mygmeasure(3,0.5,rms=rms,ifirst=xMin,last=xMax)
   print, "WF50"
   g=mygmeasure(4,0.5,rms=rms,ifirst=xMin,last=xMax)


END
