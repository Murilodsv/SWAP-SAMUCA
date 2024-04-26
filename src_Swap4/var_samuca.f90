    !-------------------------------!
    !--- SAMUCA global variables ---!
    !-------------------------------!
    
      module  var_samuca
      implicit none
      save      
      
      !--- Soil Temperature Coefficients
      real(8)    sinld,dso,dsinbe,dsinb,dayl,cosld,qo,srad,croph
      real(8)    hrnc,dhrlai,alb_surface,rh_p
      real(8)    tbot_mean,tbot_imref,tbot_ddamp,tbot_ampli
      
      !--- Management 
      real      plantdepth, rowsp
      real      co2
      integer   tillermet, metpg      
      logical   mulcheffect, usetsoil, potential_growth              
      
      !--- i/o arrays
      integer     n_inte_host                             ! Number of integer crop parameter to be read in Samuca files   called from ReadFile_samuca.f90
      integer     n_real_host                             ! Number of real crop parameter to be read in Samuca files      called from ReadFile_samuca.f90
      integer     inte_host(50)                           ! Array with integer crop parameters
      real        real_host(200)                          ! Array with real crop parameters

      end module var_samuca
