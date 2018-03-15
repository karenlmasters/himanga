pro autoflag, scan, ntot, x
  
  gtime=0
  getps,scan,intnum=0
  if (getxunits() ne 'km/s') then begin
     print,'Change x-units to km/s'
     stop
  endif	

  for i=0,ntot-1 do begin
  getps,scan,intnum=i
  stats,8000,9000,ret=mystats,/quiet
  print,mystats.rms
    IF mystats.rms gt x THEN BEGIN
       flag,scan,intnum=i,idstring='autoGPS'
       print,'FLAGGED scan ',i
       gtime=gtime+10
    ENDIF

    print,'Time lost: ',gtime, 'sec'
    print,gtime/60., ' min'

 endfor

stop
end 
