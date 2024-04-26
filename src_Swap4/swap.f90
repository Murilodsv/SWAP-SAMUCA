! File VersionID:
!   $Id: swap.f90 325 2017-03-16 15:59:06Z kroes006 $
! ----------------------------------------------------------------------
      program SWAP
! ----------------------------------------------------------------------

!     Swap modules for data communication
      use variables
      implicit none

!     Initialization of all variables in Module Variables
      call Initialize

!     iteration and timing statistics
      call IterTime(1)

! --- read time independent input .swp file
      call ReadSwap

! --- shared simulation
!cD     if(flSwapShared) call SharedSimulation(1)

! --- initialize time variables and switches/flags
      call TimeControl(1)

! --- calculate grid parameters
      call CalcGrid

! --- initialize SoilWater rate/state variables 
      call SoilWater(1)

! --- initialize SurfaceWater management variables
      if (flSurfaceWater) call SurfaceWater(1)

! --- initialize MacroPore rate/state variables
      if (flMacroPore) call MacroPore(1)

! --- initialize SoilTemperature rate/state variables
      if (flTemperature) call Temperature(1)

! --- initialize Snow rate/state variables
      if (flSnow) call Snow(1)

! --- initialize Solute rate/state variables
      if (flSolute) call Solute(1)

! --- initialize Ageing rate/state variables
      if (flAgeTracer) call AgeTracer(1)

! --- Read and Initialize Soil Management Event
      if (flCropNut) call Soilmanagement(1)

! --- open Output files and write headers
      call SwapOutput(1)
      call SoilWaterOutput(1)
      if (flIrrigate)    call IrrigationOutput(1)
      if (flTemperature) call TemperatureOutput(1)
      if (flSolute)      call SoluteOutput(1)
      if (flAgeTracer)   call AgeTracerOutput(1)
      if (flSnow)        call SnowOutput(1)
      if (flMacroPore)   call MacroPoreOutput(1)
      if (flSurfaceWater)    call SurfaceWaterOutput(1)

! --- loop with soil water time step during entire simulation period
      do while (.not.flrunend)

! ---   initialize CropGrowth rate/state variables
        if (flDayStart) call CropGrowth(1)

! ---   open crop output file and write header
!        if (flCropCalendar .and. flDayStart) call CropOutput(1)

! ---   initialize Irrigation rate/state variables
        if (schedule.eq.1) flIrrigate = .true.
        if (flIrrigate)  then
          if (t1900 - cropstart(icrop) .gt. -1.d-3 .and.                &
     &                t1900 - cropstart(icrop) .lt. 1.d-3) then
             call Irrigation(1)
             if (schedule.eq.1 .and. flIrg1Start) then
                call IrrigationOutput(1)
             endif
           endif
        endif

! ---   calculate Irrigation rate/state variables
        if (flIrrigate .and. flDayStart) call Irrigation(2)

! ---   read and process Meteo data
        if (flYearStart) call ReadMeteo
        if (flDayStart)  call MeteoDay
        if (flMeteoDt .or. flETSine)   call MeteoDt

! ---   shared simulation
!cD        if(flSwapShared .and. flDayStart) call SharedSimulation(2)

! ---   calculate Snow
        if (flSnow .and. flDayStart) call Snow(2)

! ---   calculate reduction for conductivities for frozen conditions
        if (SwFrost.eq.1) call FrozenCond
 
! ---   calculate potential and actual root water extraction profile
        call RootExtraction

! ---   determine SoilWater bottom boundary conditions
        call BoundBottom

        fldtreduce = .true.
        do while(fldtreduce)
           fldtreduce = .false.

! ---   calculate drainage fluxes
           if (fldrain)                           call Drainage
           if (.not.fldecdt .and. flSurfaceWater) call SurfaceWater(2)
           if (SwFrost.eq.1)                      call FrozenBounds
      
! ---   calculate SoilWater 
           if (.not.fldecdt) call SoilWater(2)

! ---   calculate surface water balace
           if (.not.fldecdt .and. flSurfaceWater) call SurfaceWater(3)

! ---   update time variables and switches/flags
           if(fldecdt .or. (flMacroPore .and. FlDecMpRat))then
              call SoilWaterStateVar(2)
              call TimeControl(3)
              fldtreduce = .true.
           end if

        end do

! ---   calculate SoilWater rate/state variables
        call SoilWater(3)

! ---   calculate SoilTemperature rate/state variables
        if (flTemperature) call Temperature(2)
           
! ---   calculate Solute rate/state variables
        if (flSolute) call Solute(2)

! ---   calculate Ageing rate/state variables
        if (flAgeTracer) call AgeTracer(2)

! ---   update time variables and switches/flags
        call TimeControl(2)

! ---   at the end of a day, 
        if (flDayEnd) then

! ---   update Soil nutrient status variables
           if (flCropNut) call Soilmanagement(2)

! ---      calculate potential crop growth 
           if (flCropCalendar) call CropGrowth(2)

! ---      amendent of crop residues from previous day
           if (flCropNut) call Soilmanagement(5)

! ---      amendent of fertilizers of current day
           if (flCropNut) call Soilmanagement(3)

! ---      calculate actual crop growth 
           if (flCropCalendar) call CropGrowth(3)

! ---      Simulate Soil Nutrient processes
           if (flCropNut) call Soilmanagement(4)

           if (flCropCalendar) call CropGrowth(4)

! ---      timing statistics : prevent (near) endless simulations
           if (flMaxIterTime) call IterTime(2)

        end if

! ---   output section (write to standard files and to optional files)
        if (flOutput) then
           call SwapOutput(2)
           call SoilWaterOutput(2)
           if (flTemperature)   call TemperatureOutput(2)
           if (flSolute)        call SoluteOutput(2)
           if (flAgeTracer)     call AgeTracerOutput(2)
           if (flSnow)          call SnowOutput(2)
           if (flMacroPore)     call MacroPoreOutput(2)
           if (flSurfaceWater)  call SurfaceWaterOutput(2)
        else
           if (flOutputShort)   call SoilWaterOutput(2)
        end if
        if (flDayEnd .and. (flOutput .or. flHarvestDay)) then
          if (flCropCalendar .and. flCropOutput) then
            call CropOutput(2)
          endif
        endif
        if (flIrrigationOutput)            call IrrigationOutput(2)
        if (flDayEnd .and. flCropNut)      call Soilmanagement(6)
       

! --- shared simulation
!cD       if(flSwapShared .and. flDayEnd) call SharedSimulation(3)

      enddo


! --- terminate simulation

! --- iteration and timing statistics
      call IterTime(3)

! --- close output files
!cD     if(flSwapShared) call SharedSimulation(4)
      call SwapOutput(3)
      call SoilWaterOutput(3)
      if (flCropCalendar .and. .not. flCropHarvest)  call CropOutput(3)
      if (flTemperature)        call TemperatureOutput(3)
      if (flSolute)             call SoluteOutput(3)
      if (flAgeTracer)          call AgeTracerOutput(3)
      if (flIrrigate)           call IrrigationOutput(2)
      if (flSnow)               call SnowOutput(3)
      if (flMacroPore)          call MacroPoreOutput(3)
      if (flSurfaceWater)       call SurfaceWaterOutput(3)
      if (flCropNut)            call Soilmanagement(7)

      call CloseTempFil
     

! --- write okay file for external use
      call WriteSwapOk(Project)

! --- write message on screen
      write(*,'(a)')' Swap normal completion!'
      close(logf)
      Call Exit(100)
      End Program SWAP
