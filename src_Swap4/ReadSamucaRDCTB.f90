    subroutine ReadSamucaRDCTB(pathcrop,  &
                            cropfilename, &
                            cumdens)

    implicit none    

    character(len=200) message,filnam
    character(len=*)   pathcrop, cropfilename
    integer   i,crp,logf,ifnd,swcf,getun2,schedule,swetr
    real(8)   rdctb(22)
    real(8)   depth,rootdis(202),cumdens(202),rooteff
    real(8)   sum,afgen   
    
    !--- open crp file
    filnam = trim(pathcrop)//trim(cropfilename)//'.crp'
    crp = getun2 (10,90,2)
    call rdinit(crp,logf,filnam)
    
    ! --- read rooting table [RDCTB]
    call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)
    
    ! ---   specify array ROOTDIS with root density distribution
    do i = 0,100
        depth = 0.01d0 * dble(i)
        rootdis(i*2+1) = depth
        rootdis(i*2+2) = afgen(rdctb,22,depth)
    enddo

    ! ---   calculate cumulative root density function
    do i = 1,202,2
    ! ---     relative depths
        cumdens(i) = rootdis(i)
    enddo
    sum = 0.d0
    cumdens(2) = 0.d0
    
    do i = 4,202,2
    ! ---     cumulative root density
        sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0                 &
    &               * (cumdens(i-1)-cumdens(i-3))
          cumdens(i) = sum
    enddo

    ! ---   normalize cumulative root density function to one
    do i = 2,202,2
        cumdens(i) = cumdens(i) / sum
    enddo 
    
    close(crp)      
    
    return

    end