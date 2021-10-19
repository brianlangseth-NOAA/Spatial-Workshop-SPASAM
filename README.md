# Spatially-Integrated-Life-Cycle-SILC-Model

This is the main repository for the Spatially Integrated Life Cycle (SILC) model. SILC is a fully generalized spatially stratified simulation-estimation framework incorporating a multistage stock-recruit relationship and the ability to include larval IBM outputs to inform larval dispersal. 

This is an extension to the Spatial Processes and Stock Assessment Methods (SPASAM) framework (see https://github.com/dgoethel/Spatial-Assessment-Simulator https://github.com/KateBoz/SPASAM  https://github.com/dgoethel/tag-integrated-model). The SILC model maintains all of the spatial functionality of the SPASAM model (i.e., natal homing, metapopulation, spatial heterogeneity, or panmictic population structure along with fully generalizable, including time- and age-varying, movement simulation/estimation), but with added capability of accounting for spatial processes within pre-recruit dynamics. The multistage stock-recruit relationship is also fully generalizable with each stage (egg, larval drift, settlement, post-settlement, recruitment) being modeled explicitly or various stages being compressed (e.g., using a stock-recruit function). Spatial processes are accounted for throughout the life cycle from spawning to recruitment along with juvenile movement, first spawning migrations, adult movement, and adult spawning migrations. The estimation model can fit to larval IBM outputs (i.e., connectivity matrices) with uncertainty in a similar manner to fitting tag-recovery data for juveniles and adults. The simulation model can similarly use larval IBM data to simulate larval dispersal dynamics and population connectivity. A pre-recruit index can also be simulated or fit to help estimate pre-recruit parameters.

This repository contains the main SILC framework files, including those used for the XX paper:

Completing the Life Cycle: Accounting for Pre-Recruit Dynamics in Spatial Population Models by Incorporating Larval Agent-Based Model Outputs
Daniel R. Goethel Aaron M. Berger Katelyn Bosley, et al.


UPDATE Oct, 2021

During the Spatial Assessment Modeling Workshop simulations (see https://github.com/carenbarcelo-NOAA/Spatial-Assessment-Modeling-Workshop) we noticed that the Estimation model .dat file does not aligh with the .tpl nor the operating model output. We updated the .tpl and the .exe to match the output from the operation model.
