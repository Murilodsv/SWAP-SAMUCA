    subroutine root_growth_factor(cumdenstb, ddw_rt,          &
                                  upper, bottom, SLTHICKNESS,dep,  & 
                                  rootshape, drld)

    use Variables
    implicit none    
            
    integer   i,cumdenstb, sl, numlay_roots
    real(8)   depth,rootdis(202)
    real(8)   sum,afgen   
    real      upper(maho), bottom(maho), SLTHICKNESS(maho), dep(maho)
    real		ddw_rt
    real		rgf(maho+1,3)                                 !
    real		dw_rt_prof(maho)                              !
    real		lroot(maho)                                   !
    real		drld(maho)                                    !
    real		rdprof                                !
    real		geot(maho)                                    !
    real		rootshape                             !
    real(8)     bot,top
    
        
    !--- Root depth and numlay with roots
    rdprof = 0.d0
    numlay_roots = 0
    do sl = 1, numlay
        if(rd .ge. upper(sl)) then
            rdprof = rdprof + slthickness(sl)
            numlay_roots = sl
        endif
    enddo      
    
    if (cumdenstb .eq. 1) then
        !-----------------------------------------------!
        !--- Calculate RLD based on cumdens function ---!
        !-----------------------------------------------!
        
        !--- Convert dryweight to root length density
        drld = 0.d0        
        rgf  = 0.d0        
        do sl = 1, numlay_roots        
            top = abs(upper(sl) / rd)
            bot = abs(bottom(sl) / rd)            
            rgf(sl,1)       = (afgen(cumdens,202,bot)-afgen(cumdens,202,top))
            !rgf(sl,1)       = geot(sl)/sum(geot)        
            dw_rt_prof(sl)  = rgf(sl,1) * ddw_rt * (1.e6/1.e8)   ! [g cm-2]            
            lroot(sl)       = dw_rt_prof(sl)    * srl * 100.d0  ! [cm cm-2]
            drld(sl)        = lroot(sl) / slthickness(sl)       ! [cm cm-3]
            
        enddo    
          
    else
        
        !--------------------------------------------------!
        !--- Calculate RLD based on geotropism function ---!
        !--------------------------------------------------!
            
        !--- Geotropism function
        do sl = 1, numlay_roots
            geot(sl) = max(0.d0,(1-dep(sl)/rdprof)) ** rootshape
        enddo

        !--- Convert dryweight to root length density
        drld = 0.d0        
        rgf  = 0.d0        
        do sl = 1, numlay_roots        
            rgf(sl,1)       = geot(sl)/sum(geot)        
            dw_rt_prof(sl)  = rgf(sl,1) * ddw_rt * (1.e6/1.e8)   ! [g cm-2]
            lroot(sl)       = dw_rt_prof(sl)    * srl * 100.d0  ! [cm cm-2]
            drld(sl)        = lroot(sl) / slthickness(sl)       ! [cm cm-3]            
        enddo    
    endif
    
    return

    end