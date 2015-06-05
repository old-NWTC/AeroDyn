BEMT Validation Study
----------------------

1) These folders contain files necessary to reproduce the results found in:
     Development and Validation of a New Blade Element Momentum Skewed-Wake Model within AeroDyn: Preprint
     by Ning, S. A., Hayman, G., Damiani, R., Jonkman, J., 2014 
	
     http://www.nrel.gov/docs/fy15osti/63217.pdf

2) Files are also included for validation of the stand-alone version of AeroDyn v15 as well as the version of FAST v8 which includes AD v15


Explanation of Folders and Files
-------------------------------

BEMT Validation                                      :  Top-level folder for this study
   codes                                             :  This folder contains all files necessary to generate simulation results using the various Simulation packages (i.e., FAST, AD v15, stand-alone, etc.)
      AD_v15                                         :  All files necessary to generate simulation results using the stand-alone AeroDyn v15
      FAST8                                          :  All files necessary to generate simulation results using the FAST v8 and AD v14
      FAST8_ADv15                                    :  All files necessary to generate simulation results using the FAST v8 and AD v15
      WT_PERF                                        :  All files necessary to generate simulation results using the prototype BEMT module and a stand-alone driver called WT_Perf
      runcodes.bat                                   :  This file batch executes all the simulation runs for this study
   images                                            :  This folder contains the plotted results in pdf format as well as images needed to generate the LaTex version of the above referenced paper
   data                                              :  This folder contains the simulation results, the experimental results, and the files necessary to post-process these results
   
   
   
Generating Simulation Results
-------------------------------



Plotting the Study Results
-------------------------------