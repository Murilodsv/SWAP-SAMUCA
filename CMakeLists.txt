##########################################################################################
# CMake project file for SWAP-SAMUCA
##########################################################################################
# Define the project and the dependencies that it has
##########################################################################################
# IMPORTANT:
# The release version of SWAP is compiled using Intel(R) Visual Fortran Compiler [check 
# SWAP file compiler_settings_4.0.txt]. This compilation tries to be as close as possible
# as SWAP's release, so the below configurations are done before running cmake:
# 
# #--- loading Intel(R) Visual Fortran Compiler (ifort) and cmake
# module load iccifort/2020.1.217
# module load CMake
#
# #--- setting ifort as the default compiler
# export FC=ifort
##########################################################################################
# CAVEATS:
# In some linux systems, users should use lowercase filenames for SWAP input:
# e.g. swap.swp instead of Swap.swp
# This apparently has to do with fopengstandard.f90 and underlying openfile subs of ttutil
##########################################################################################
# Murilo Vianna. Feb,2023
##########################################################################################

CMAKE_MINIMUM_REQUIRED(VERSION 3.5.0)

PROJECT(SWAP-SAMUCA Fortran)

# Compiler options [not really working because of version incompatibility, commented for now]
# trying to be as close as J.G. (Joop) Kroes set for SWAP [check SWAP file compiler_settings_4.0.txt]
# add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-O0;-fpscomp;general;-fpe0;-traceback;-check;all;-threads;-static-intel>")

##########################################################################################
# SWAP-SAMUCA VERSION
##########################################################################################

SET(MAJOR 1)
SET(MINOR 0)
SET(MODEL 0)
SET(BUILD 0)

# Get the current working branch
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%H
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH_ABBR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

MESSAGE("##########################################################################")
MESSAGE("Building ${CMAKE_PROJECT_NAME}")
MESSAGE(STATUS "MAJOR: ${MAJOR} MINOR: ${MINOR} MODEL: ${MODEL} BUILD: ${BUILD} COMMIT: ${GIT_COMMIT_HASH_ABBR}")
MESSAGE(STATUS "BRANCH: ${BRANCH}")
MESSAGE(STATUS "COMMIT: ${GIT_COMMIT_HASH}")
# Set the version
SET(VERSION ${MAJOR}.${MINOR}.${MODEL})

IF(APPLE)
    SET(ENV{MACOSX_DEPLOYMENT_TARGET} 10.10)
ENDIF(APPLE)

# Add our local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Organize output files.  On Windows this also keeps .dll files next
# to the .exe files that need them, making tests easy to run.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Keep Uncomment if Fortran 90 support is required
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Sets the compilation flags for DEBUG
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# Define the executable name and OSDefinitions file
SET(EXECUTABLE_NAME swap_samuca_v${MAJOR})

# Add the source files [first ttutil then swap-samuca]
set(
    source_list
		"src_ttutil427PC/addinf.for"
		"src_ttutil427PC/addint.for"
		"src_ttutil427PC/addrea.for"
		"src_ttutil427PC/addref.for"
		"src_ttutil427PC/addstf.for"
		"src_ttutil427PC/addstr.for"
		"src_ttutil427PC/ambusy.for"
		"src_ttutil427PC/copfl2.for"
		"src_ttutil427PC/decchk.for"
		"src_ttutil427PC/decdou.for"
		"src_ttutil427PC/decint.for"
		"src_ttutil427PC/decrea.for"
		"src_ttutil427PC/decrec.for"
		"src_ttutil427PC/dectim.for"
		"src_ttutil427PC/delfil.for"
		"src_ttutil427PC/dtardp.for"
		"src_ttutil427PC/dtdpar.for"
		"src_ttutil427PC/dtdpst.for"
		"src_ttutil427PC/dtfsecmp.for"
		"src_ttutil427PC/dtfsedp.for"
		"src_ttutil427PC/dtleap.for"
		"src_ttutil427PC/dtnow.f90"
		"src_ttutil427PC/dtsys.for"
		"src_ttutil427PC/entcha.for"
		"src_ttutil427PC/entdch.for"
		"src_ttutil427PC/entddo.for"
		"src_ttutil427PC/entdin.for"
		"src_ttutil427PC/entdou.for"
		"src_ttutil427PC/entdre.for"
		"src_ttutil427PC/entdti.for"
		"src_ttutil427PC/entdyn.for"
		"src_ttutil427PC/enthlp.for"
		"src_ttutil427PC/entint.for"
		"src_ttutil427PC/entrea.for"
		"src_ttutil427PC/enttim.for"
		"src_ttutil427PC/entyno.for"
		"src_ttutil427PC/extens.for"
		"src_ttutil427PC/fatalerr.f90"
		"src_ttutil427PC/flexist.for"
		"src_ttutil427PC/flname.for"
		"src_ttutil427PC/fopengstandard.f90"
		"src_ttutil427PC/fopens.for"
		"src_ttutil427PC/getrec.for"
		"src_ttutil427PC/getun.for"
		"src_ttutil427PC/getun2.for"
		"src_ttutil427PC/ifindc.f90"
		"src_ttutil427PC/ifindi.for"
		"src_ttutil427PC/istart.for"
		"src_ttutil427PC/iunifl.for"
		"src_ttutil427PC/lextokin.inc"
		"src_ttutil427PC/lint.for"
		"src_ttutil427PC/lowerc.for"
		"src_ttutil427PC/messini.f90"
		"src_ttutil427PC/messinq.f90"
		"src_ttutil427PC/messwrt.for"
		"src_ttutil427PC/movavr.for"
		"src_ttutil427PC/newlinestandard.f90"
		"src_ttutil427PC/notnul.for"
		"src_ttutil427PC/openlogf.f90"
		"src_ttutil427PC/outar2.f90"
		"src_ttutil427PC/outcom.for"
		"src_ttutil427PC/outdat.f90"
		"src_ttutil427PC/outplt.for"
		"src_ttutil427PC/outsel.f90"
		"src_ttutil427PC/parsword.for"
		"src_ttutil427PC/rchrsrc.for"
		"src_ttutil427PC/rdacha.for"
		"src_ttutil427PC/rdador.for"
		"src_ttutil427PC/rdadou.for"
		"src_ttutil427PC/rdainr.for"
		"src_ttutil427PC/rdaint.for"
		"src_ttutil427PC/rdalog.for"
		"src_ttutil427PC/rdarea.for"
		"src_ttutil427PC/rdarer.for"
		"src_ttutil427PC/rdatim.for"
		"src_ttutil427PC/rddata.for"
		"src_ttutil427PC/rddata.inc"
		"src_ttutil427PC/rddecinf.inc"
		"src_ttutil427PC/rddtmp.for"
		"src_ttutil427PC/rderr.for"
		"src_ttutil427PC/rderri.for"
		"src_ttutil427PC/rderrinf.inc"
		"src_ttutil427PC/rdfcha.for"
		"src_ttutil427PC/rdfdor.for"
		"src_ttutil427PC/rdfdou.for"
		"src_ttutil427PC/rdfilinf.inc"
		"src_ttutil427PC/rdfinr.for"
		"src_ttutil427PC/rdfint.for"
		"src_ttutil427PC/rdflog.for"
		"src_ttutil427PC/rdfrea.for"
		"src_ttutil427PC/rdfrer.for"
		"src_ttutil427PC/rdfrom.for"
		"src_ttutil427PC/rdftim.for"
		"src_ttutil427PC/rdinar.for"
		"src_ttutil427PC/rdindt.for"
		"src_ttutil427PC/rdindx.for"
		"src_ttutil427PC/rdinit.for"
		"src_ttutil427PC/rdinlv.for"
		"src_ttutil427PC/rdinne.for"
		"src_ttutil427PC/rdinqr.for"
		"src_ttutil427PC/rdinqr2.for"
		"src_ttutil427PC/rdinqr3.for"
		"src_ttutil427PC/rdjdat.gin"
		"src_ttutil427PC/rdjdec.gin"
		"src_ttutil427PC/rdlex.for"
		"src_ttutil427PC/rdmachin.inc"
		"src_ttutil427PC/rdmcha.for"
		"src_ttutil427PC/rdmdef.for"
		"src_ttutil427PC/rdmdou.for"
		"src_ttutil427PC/rdmint.for"
		"src_ttutil427PC/rdmlog.for"
		"src_ttutil427PC/rdmodulettutil.f90"
		"src_ttutil427PC/rdmrea.for"
		"src_ttutil427PC/rdmtim.for"
		"src_ttutil427PC/rdndat.gin"
		"src_ttutil427PC/rdndec.gin"
		"src_ttutil427PC/rdpars.for"
		"src_ttutil427PC/rdrecinf.inc"
		"src_ttutil427PC/rdscha.for"
		"src_ttutil427PC/rdsctb.for"
		"src_ttutil427PC/rdsdor.for"
		"src_ttutil427PC/rdsdou.for"
		"src_ttutil427PC/rdsets.for"
		"src_ttutil427PC/rdsinr.for"
		"src_ttutil427PC/rdsint.for"
		"src_ttutil427PC/rdslog.for"
		"src_ttutil427PC/rdsrea.for"
		"src_ttutil427PC/rdsrer.for"
		"src_ttutil427PC/rdstainf.inc"
		"src_ttutil427PC/rdstim.for"
		"src_ttutil427PC/rdtblinf.inc"
		"src_ttutil427PC/rdtmp1.for"
		"src_ttutil427PC/rdtmp2.for"
		"src_ttutil427PC/reaand.for"
		"src_ttutil427PC/reanor.for"
		"src_ttutil427PC/recread.f90"
		"src_ttutil427PC/recread.inc"
		"src_ttutil427PC/recreadi.f90"
		"src_ttutil427PC/recreadt.f90"
		"src_ttutil427PC/remove.for"
		"src_ttutil427PC/sfindg.for"
		"src_ttutil427PC/sortch.for"
		"src_ttutil427PC/sortin.for"
		"src_ttutil427PC/str_copy.for"
		"src_ttutil427PC/swpi4.for"
		"src_ttutil427PC/ttutil.f90"
		"src_ttutil427PC/ttutilprefs.f90"
		"src_ttutil427PC/ttuver.for"
		"src_ttutil427PC/unifl.for"
		"src_ttutil427PC/upperc.f90"
		"src_ttutil427PC/usedun.for"
		"src_ttutil427PC/ver4_27.for"
		"src_ttutil427PC/warning.for"
		"src_ttutil427PC/words.for"
		"src_ttutil427PC/wracha.for"
		"src_ttutil427PC/wradou.for"
		"src_ttutil427PC/wraint.for"
		"src_ttutil427PC/wralog.for"
		"src_ttutil427PC/wrarea.for"
		"src_ttutil427PC/wratim.for"
		"src_ttutil427PC/wrinit.for"
		"src_ttutil427PC/wrscha.for"
		"src_ttutil427PC/wrsdou.for"
		"src_ttutil427PC/wrsint.for"
		"src_ttutil427PC/wrslog.for"
		"src_ttutil427PC/wrsrea.for"
		"src_ttutil427PC/wrstim.for"
		"src_ttutil427PC/wr_sys.inc"
		"src_Swap4/arrays.fi"
		"src_Swap4/boundbottom.f90"
		"src_Swap4/boundtop.f90"
		"src_Swap4/calcgrid.f90"
		"src_Swap4/calcgwl.f90"
		"src_Swap4/checkmassbal.f90"		
		"src_Swap4/convertdiscrvert.f90"
		"src_Swap4/cropgrowth.f90"
		"src_Swap4/description.fi"
		"src_Swap4/divdra.f90"
		"src_Swap4/drainage.f90"
		"src_Swap4/fluxes.f90"		
		"src_Swap4/frozencond.f90"
		"src_Swap4/functions.f90"
		"src_Swap4/Functions_SAMUCA.f90"
		"src_Swap4/headcalc.f90"
		"src_Swap4/hysteresis.f90"
		"src_Swap4/initialize.f90"
		"src_Swap4/integral.f90"
		"src_Swap4/irrigation.f90"
		"src_Swap4/macropore.f90"
		"src_Swap4/macroporeoutput.f90"
		"src_Swap4/macrorate.f90"
		"src_Swap4/management_soil.f90"
		"src_Swap4/meteoday.f90"
		"src_Swap4/meteodt.f90"
		"src_Swap4/Output_samuca.f90"
		"src_Swap4/oxygenstress.f90"
		"src_Swap4/params.fi"
		"src_Swap4/penmon.f90"		
		"src_Swap4/ReadFile_samuca.f90"
		"src_Swap4/readmeteo.f90"
		"src_Swap4/readswap.f90"
		"src_Swap4/rld.f90"
		"src_Swap4/rootextraction.f90"
		"src_Swap4/root_cumdens.f90"
        "src_Swap4/root_growth_factor.f90"
        "src_Swap4/ReadSamucaRDCTB.f90"
		"src_Swap4/root_profile.f90"
		"src_Swap4/SAMUCA.f90"
		"src_Swap4/TempHour_samuca.f90"
		"src_Swap4/radiat.f90"
		"src_Swap4/astro_sam.f90"
		"src_Swap4/sharedexchange.f90"
		"src_Swap4/sharedsimulation.f90"
		"src_Swap4/snow.f90"
		"src_Swap4/soilwater.f90"
		"src_Swap4/solute.f90"
		"src_Swap4/sptabulated.f90"
		"src_Swap4/substrates_balance.f90"
		"src_Swap4/sucrose_content.f90"
		"src_Swap4/surfacewater.f90"
		"src_Swap4/swap.f90"
		"src_Swap4/swapoutput.f90"
		"src_Swap4/temperature.f90"
		"src_Swap4/timecontrol.f90"
		"src_Swap4/totass_samuca.f90"
		"src_Swap4/tridag.f90"		
		"src_Swap4/variables.f90"
		"src_Swap4/var_samuca.f90"
		"src_Swap4/water_stress_sugarcane.f90"
		"src_Swap4/watstor.f90"
		"src_Swap4/wofostnut.f90"
		"src_Swap4/wofost_soil_amendments.f90"
		"src_Swap4/wofost_soil_balancecheck.f90"
		"src_Swap4/wofost_soil_cropresidues.f90"
		"src_Swap4/wofost_soil_declarations.f90"
		"src_Swap4/wofost_soil_interface.f90"
		"src_Swap4/wofost_soil_orgmatn.f90"
		"src_Swap4/wofost_soil_parameters.f90"
		"src_Swap4/wofost_soil_rateconstants.f90"
		"src_Swap4/wofost_soil_watern.f90"
)

ADD_EXECUTABLE(${EXECUTABLE_NAME} ${source_list})

##################################################
# Resume information about flags and option used #
##################################################

message( "-- Flags" )
message( "   FFLAGS       ${CMAKE_Fortran_FLAGS}" )
message( "   RELEASE      ${CMAKE_Fortran_FLAGS_RELEASE}" )
message( "   DEBUG        ${CMAKE_Fortran_FLAGS_DEBUG}")
message( "   LINKER       ${CMAKE_EXE_LINKER_FLAGS}")
message( "-- Build Info" )
message( "   BUILD TYPE   ${CMAKE_BUILD_TYPE}" )
message( "   VERSION      ${MAJOR}.${MINOR}.${MODEL}.${BUILD}" )
message( "   I. PREFIX    ${CMAKE_INSTALL_PREFIX}" )
message( "   Executable   ${EXECUTABLE_NAME}" )

MESSAGE("##########################################################################")


