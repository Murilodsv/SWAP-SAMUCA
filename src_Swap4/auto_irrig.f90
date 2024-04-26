subroutine auto_irrig(irrmanag,     &
                      nlay,         &
                      swc,          &
                      wpp,          &
                      fcp,          &
                      dep,          &
                      slthickness,  &
                      irr_depth,    &
                      irr_trigger,  &
                      nirr)
        
    implicit none
    include 'Constants.fi'
    
    !--- local variables
    integer irrmanag 
    integer nlay
    integer sl
    real    swc (max_sl)
    real    wpp (max_sl)
    real    fcp (max_sl)
    real    dep (max_sl)
    real    slthickness (max_sl)
    real    irr_depth
    real    irr_trigger
    real    nirr
    real    thold_idepth
    real    taw_idepth
    real    raw_idepth
    
    !--- initialize
    thold_idepth = 0.d0
    taw_idepth   = 0.d0
    raw_idepth   = 0.d0
    nirr         = 0.d0
    
    !--- Total holding capacity and available water up to the irrig depth
    do sl = 1, nlay
        if(dep(sl) .lt. irr_depth)then
            thold_idepth = thold_idepth + (fcp(sl) - wpp(sl)) * slthickness(sl)
            taw_idepth   = taw_idepth   + max(0.d0, (swc(sl) - wpp(sl))) * slthickness(sl)
        else if ((dep(sl) - slthickness(sl)) .lt. irr_depth) then
            thold_idepth = thold_idepth + (fcp(sl) - wpp(sl)) * (slthickness(sl) - (dep(sl) - irr_depth))
            taw_idepth   = taw_idepth   + max(0.d0, (swc(sl) - wpp(sl))) * (slthickness(sl) - (dep(sl) - irr_depth))
        end if
    enddo
    
    !--- Relative available water within the irrigation depth
    raw_idepth = taw_idepth / thold_idepth
    
    !--- net irrigation, if needed
    if(raw_idepth .lt. irr_trigger)then
        nirr = (thold_idepth - taw_idepth) * 10.d0
    end if
    
    return 
    
end subroutine auto_irrig