# PoritesSpawn
Physiological trait analysis for P. astreoides larvae derived from different adult colonies

Files in this repository:

- Graphs.R: Annotated R script for generating plots of daily release and mild stress traits by family

    Input files: 
    - LarvalVolume.csv
    - ZooxDailyRls.csv
    - ChlDailyRls.csv
    - PrtDailyRls.csv
    - ZooxExp.csv
    - ChlExp.csv
    - PrtExp.csv

- CoreStats.R: Annotated R script for generating ANOVA models for daily release traits and mixed models for mild stress data

    Input files: 
    - LarvalVolume.csv
    - ZooxDailyRls.csv
    - ChlDailyRls.csv
    - PrtDailyRls.csv
    - ZooxExp.csv
    - ChlExp.csv
    - PrtExp.csv
                 
- FecCor.R: Annotated R script for performing correlations between fecundity and physiological traits as well as survival

    Input files: 
    - DailyReleaseData.csv
    - LarvalVolume.csv
    - ZooxDailyRls.csv
    - ChlDailyRls.csv
    - PrtDailyRls.csv
    - CFec.csv
    - ZooxExp.csv
    - ChlExp.csv
    - PrtExp.csv
    - SurvivalRiskCorrelations.csv
                 
- SymChlaCor.R: Annotated R script for performing correlations between symbiont density and chlorophyll a concentration for daily release and mild stress experiment

    Input files:
    - ZooxDailyRls.csv
    - ChlDailyRls.csv
    - ZooxExp.csv
    - ChlExp.csv

- netwells_heats.R: Annotated R script for survival analysis in acute stress experiment

    Input files:
    - NetwellSurvivalExptData_Rep2_14Apr18_ModelSurvFit.csv
    - NetweelSurvivalExptData_Rep1_14Apr18_ModelSurvFit.csv

- Temperature logger data for different treatment conditions

    Acute stress:
    - AmbientBinExtremeTempExpt_run2.csv
    - ExtremeTempExp_HeatBin_run2.csv

    Mild stress:
    - TempExp-Ctrl-BinA.csv
    - TempExp-Ctrl-BinB.csv
    - TempExp-Ctrl-BinC.csv
    - TempExp-Heat-BinD.csv
    - TempExp-Heat-BinE.csv
    - TempExp-Heat-BinF.csv
