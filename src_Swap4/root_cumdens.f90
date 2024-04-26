    subroutine root_cumdens(numlay,rld,upper,bottom,  &
                            cumdens)

    implicit none
    include  'arrays.fi'

    integer     numlay
    integer     sl
    integer     i
    integer     nlay_roots
    real        rld(maho)
    real        bottom(maho)
    real        upper(maho)
    real*8      soma
    real*8      relative_rld(maho*2)
    real*8      rootdis(202)
    real*8      cumdens(202)
    real*8      depth
    real*8      afgen    
    logical     hasvalue
    real ::     rootshape   = 1.d0      ! Exponential (rootshape > 0); Linear (rootshape = 0)

    !--- Initialize 
    relative_rld    = 0.d0
    
    !--- Check if rld has at least one value > zero
    hasvalue    = .false.
    nlay_roots  = 0
    do sl = 1, numlay
        if(rld(sl) .gt. 0.0000001)then
            hasvalue    = .true.    ! Has RLD data
            nlay_roots  = sl        ! Deeper layer with roots
        endif        
    enddo
    
    if(.not. hasvalue)then
        !--- Specify the default value (linear density)
        nlay_roots = numlay 
        i = 1
        do sl = 2, nlay_roots*2,2
            relative_rld(sl-1)    =   bottom(i) / bottom(nlay_roots)
            relative_rld(sl)      =   1.d0 / max(0.01,relative_rld(sl-1))**rootshape
            i = i + 1
        enddo
    else    
        !--- Get Relative depths
        i = 1
        do sl = 2, nlay_roots*2,2                
            relative_rld(sl-1)    =   bottom(i) / bottom(nlay_roots)
            relative_rld(sl)      =   rld(i)
            i = i + 1
        enddo
    endif
    
    !--- specify array ROOTDIS with root density distribution
    do i = 0,100
        depth = 0.01d0 * dble(i)
        rootdis(i*2+1) = depth
        rootdis(i*2+2) = afgen(relative_rld,22,depth)
    enddo
    
    !--- calculate cumulative root density function
    do i = 1,202,2
        !--- relative depths
        cumdens(i) = rootdis(i)
    enddo
    soma = 0.d0
    cumdens(2) = 0.d0
    do i = 4,202,2
        !--- cumulative root density
        soma = soma + (rootdis(i-2)+rootdis(i)) * 0.5d0 * (cumdens(i-1)-cumdens(i-3))
        cumdens(i) = soma
    enddo

    !--- normalize cumulative root density function to one
    do i = 2,202,2
        cumdens(i) = cumdens(i) / soma
    enddo
    
    cumdens = cumdens
    
    
    return

    end