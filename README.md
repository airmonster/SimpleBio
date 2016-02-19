# SimpleBio
##SimpleBio is a simplified biogenic voc emission model for CMAQ modelers.  
Currently there are so many biogenic voc emission models including BEIS2, BEIS3, Megan and Globeis, so why should I develop a bvoc model?
###SimpleBio is *simple*
![Calculated ISOP of SCBasin in April](https://github.com/airmonster/SimpleBio/blob/master/ISOP.png)  


>The SimpleBio Model needs no extra data on landuse or emission factors or LAI or balabala, the only data needed is WRFOUT.
>WRF output netCDF data was processed with MCIP, with which the file format was converted to IOAPI, and the grid matches perfectly with CMAQ.

###SimpleBio is *portable*
>SimpleBio was written in R and can be ported to Windows, Mac or Linux, even for Linux on ARM.

###SimpleBio is *easy to go*
>SimpleBio runs under both IDE(RStudio) and CLI, parameters can be set by editing the code or passed from CLI.
>To run SimpleBio under CLI, type ./SimpleBio.R APPL IF_MERGE_TO_SMOKE

###SimpleBio is *free and opensource*
>Source code of SimpleBio will be released right the moment of my essay publishes.

*Contact me via lcw@airmonster.org*
