=== README.TXT =============
============================
This file is part of:

program    :  SWAP
version    :  4.0.1
releasedate:  12-June-2017
platform   :  MS-windows7, 64-bit operating system


This file READ.ME contains the following information:
1. Short product description
2. Installation procedure
3. How to get started
4. Disclaimer
5. References
============================
1. Short product description

SWAP (Soil-Water-Atmosphere-Plant) simulates transport of water, solutes 
and heat in variably saturated soils. The program is designed to simulate the
transport processes at field scale level and during whole growing seasons. 
The program is developed by Wageningen University and Research.

============================
2. Installation procedure

This version of SWAP is offered to you in a zip-file (Swap4.x.x.zip)
Unzip this file to a folder where you have write-access. 
Leave the folder-structure intact to enable an easy start.


============================
3. How to get started

A quick start of a simulation for case 1 '1.HupselBrook' can be carried out as follows:

  - Change to the sub-directory   \cases\1.HupselBrook\;
  - On this directory you can simulate the reference situation by starting the batch-file runswap.cmd;
  - Verify results by using an ASCII-editor. 
    The water balance of the first year 2002, as given in the file Result.bal, should read:

                  Water balance components (cm)
       In                           Out
       =========================    ============================
       Rain + snow    :    84.18    Interception      :     3.74
       Runon          :     0.00    Runoff            :     0.00
       Irrigation     :     0.50    Transpiration     :    30.86
       Bottom flux    :     0.00    Soil evaporation  :    16.09
                                    Crack flux        :     0.00
                                    Drainage level 1  :    29.55
       =========================    ============================
       Sum            :    84.68    Sum               :    80.24

The standard manual is supplied in the folder \doc\
Kroes et al. - 2017 - SWAP version 4, Theory description and user manual. 
Wageningen Environmental Research, ESG Report 2780.pdf

An additional manual for simulations focusing on C and N in crop and soil simulation is in the folder \doc\:
Groenendijk et al. - 2016 - Simulation of nitrogen-limited crop growth with SWAP WOFOST. Report 2721.PDF

The program has been tested on MS Windows7 using different cases. 
Test-reports are supplied in the folder \doc\

============================
4. Disclaimer

The author(s) and the Alterra Institute disclaim all liability for 
direct, incidental or consequential damage resulting from use of the program.


============================
5. References:

An explanation of theory and ins and outs of this version of the model is given in the 
manual (Kroes et al, 2017). 
A general reference to the Swap model is Van Dam et al (2000) who also supplies an overview of 
applications. References may also be found on the internet www.swap.alterra.nl.
SoilPhysical data in the sub-folders of \xdata\soils\ are based on Wosten (1987), Wosten et al. (1994), 
Wosten et al. (2001) and Wosten et al. (2013) 

Kroes, J. G., Dam, J. C. van, Bartholomeus, R. P., Groenendijk, P., Heinen, M., 
	Hendriks, R. F. A., … Walsum, P. E. V. van. (2017). SWAP version 4, Theory description and 
	user manual. Wageningen Environmental Research, ESG Report 2780.
Van Dam, J.C, 2000. Field scale water flow and solute transport. SWAP model concepts, 
    parameter estimation and case studies. PhD thesis, Wageningen Universiteit, 167 p.
Groenendijk, P., Boogaard, H., Heinen, M., Kroes, J., Supit, I., & Wit, A. De. (2016). 
	Simulation of nitrogen-limited crop growth with SWAP / WOFOST. Report 2721. 
	Alterra Rapport, 2721. Retrieved from http://edepot.wur.nl/400458
Wösten, J. H. M. (1987). Beschrijving van de Waterretentie- en doorlatendheidskarakteristieken 
	uit de Staringreeks met analytische functies. Stiboka rapport nr 2019.
Wösten, J. H. ., Veerman, G. J., & Stolte, J. (1994). Waterretentie- en doorlatendsheidskarakteristieken 
	van boven- en ondergronden in Nederland : de Staringreeks 1994. Technisch Document 18.
Wösten, J. H. M., Pachepsky, Y. A., & Rawls, W. J. (2001). Pedotransfer functions: bridging the gap 
	between available basic soil data and missing soil hydraulic characteristics. 
	Journal of Hydrology, 251(3–4), 123–150. http://doi.org/10.1016/S0022-1694(01)00464-4
Wösten, H., Vries, F. De, Hoogland, T., Massop, H., Veldhuizen, A., Vroon, H., … Bolman, A. (2013). 
	BOFEK2012, de nieuwe, bodemfysische schematisatie van Nederland; Alterra-rapport 2387. Wageningen

For information please contact:
J.G. (Joop) Kroes or other persons from the Development group (www.swap.alterra.nl)
Wageningen UR, Centre for Water and climate
Adresses:
Post:     PO.Box 47, 6700 AA Wageningen / Netherlands
Email:    swap.alterra@wur.nl  or  joop.kroes@wur.nl 
Phone:    + 31 317 486433
Internet: www.swap.alterra.nl
----------------------------

Copyright ALTERRA 2017
============================

=== end of README.TXT  =====
