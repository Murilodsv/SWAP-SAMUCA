subroutine output_samuca(  task,           &
                    project,        &
                    outp,           &
                    outd,           &                    
                    outdph,         &
                    outdpa,         &
                    outpfac,        &
                    outstres,       &
                    outds,          &
                    outcumd,        &
                    writedetphoto,  &
                    writedcrop,     &
                    writehead,      &
                    detailedsoil,   &
                    writecumdens)
    
    !use Variables
    implicit none
    
    integer task
    integer*4   getcwd
    integer*4   pathstat    
    
    integer     outp
    integer     outd
    integer     outds
    integer     outdph    
    integer     outdpa
    integer     outpfac
    integer     outstres
    integer     outcumd
    
    logical     writedetphoto
    logical     writedcrop
    logical     writehead
    logical     detailedsoil
    logical     writecumdens
    
    character project*80         ! Name of project
    
    save
    
    goto(10,20,30) task
    
10  continue
        
    return
    
20  continue
    
    !--- Open Crop output file
    open (outp,     file='Plant_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
    
    !--- Write header
    if(writehead)then
        
        !--- Crop output header     
        write(outp, 11) 'Plant Growth Simulations for: ', trim(project)
        write(outp, 13) !Warning: Put simulation ID here!
        write(outp, 14)
        write(outp, 15)
        write(outp, 16)
        write(outp, 17)    
        
    endif
    
    !--- Open debugging output file
    open (outd,     file='debug.OUT',status='REPLACE', action='WRITE')
    
    !--- Open detailed photosynthesis file
    if(writedetphoto)then
        open (outdph, file='Photos_Actual_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
        if(writehead)then
            !--- Canopy photosynthesis header 
            write(outdph, 21) 'Photosynthesis Simulations for: ', trim(project)
            
            write(outdph, 24)
            write(outdph, 25)
            write(outdph, 26)
            write(outdph, 27)
        endif    
    endif
    
    !--- Open detailed crop file (phytomer level)
    if(writedcrop)then
        open (outdpa,     file='Detailed_Crop_Actual_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
        if(writehead)then
            !--- to be developed...
        endif    
    endif
    
    !--- Partitioning Factors (SAMUCA)
    open (outpfac,     file='Partitioning_factor_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
    if(writehead)then
        !--- to be developed...
    endif    
    
    !--- Stress factors (SAMUCA)
    open (outstres,     file='Stress_factors_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
    if(writehead)then
        !--- to be developed...
    endif
    
    !--- Detailed Soil
    open (outds,     file='Detailed_Soil_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
    if(writehead)then
        !--- to be developed...
    endif
    
    open (outcumd,   file='Root_cumdens_'//trim(project)//'.OUT',status='REPLACE', action='WRITE')
     if(writehead)then
        !--- to be developed...
    endif
    
        !--------------------!
        !--- File Headers ---!
        !--------------------!
    
      !--- Crop file Header
11    format(2A,/) 		
12    format(2A,/)                                                  
13    format('Results of plant growth simulation in daily output:')				
14    format('     PlCane           Day   Days   Days  Accum    Plant   Stalk  Leaves    Root   Stalk  Sucros',&
'                           Plant   Green   Stress  Stress                ')
15    format('       or              of  after  after  D.Day   Weight  Weight  Weight  Weight  Weight  Weight',&
'     POL     LAI  Tiller  Height  Leaves   Factor  Factor   Crop    Crop ')
16    format('Seq. Ratoon   Year   Year  Simul  Plant   oC.d   DMt/ha  DMt/ha  DMt/ha  DMt/ha  FMt/ha  DMt/ha',&
'       %   m2/m2    #/m2       m   #/Stk   Expans  Photos  Status  DStage')

17    format('---- ------   ----  -----  -----  -----  ------ -------  ------  ------  ------  ------  ------',&
'  ------  ------  ------  -------  ------  ------  ------  ------  ------')       
        

      !--- Detailed Photos Header
21    format(2A,/) 		
22    format(2A,/)
24    format('     PlCane           Day   Days   Days               Amax                 LAI      Qleaf      A   ',&
'    PAR        PAR       PAR  ')
25    format('       or              of  after  after      LAI     kg(CO2)     Hour     Accum      μmol     μmol ',&
'   Direct     Difuse    Total ')      
26    format('Seq. Ratoon   Year   Year  Simul  Plant      M/M     ha-1h-1      hr       M/M      m-2s-1   m-2s-1 ',&
'   W m-2     W m-2     W m-2 ')
27    format('---- ------   ----  -----  -----  -----    -------   -------   -------   -------   -------   -------',&
'  -------    ------   -------')
    
    
    return

30  continue
    
    close(outp)    
    close(outd)    
    close(outpfac)
    close(outstres)    
    
    !--- Detailed outputs    
    if(writedetphoto)   close(outdph)
    if(writedcrop)      close(outdpa)    
    if(detailedsoil)    close(outds)
    if(writecumdens)    close(outcumd)
    
    return
    
end subroutine output_samuca
    
    