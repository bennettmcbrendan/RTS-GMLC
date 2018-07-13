**The workflow for running a decomposed RTS-GMLC model is as follows:**

1. go into the PLEXOS/ directory in your cloned repo
2. double click the batch file 'run_models_1.bat' (assumes you have PLEXOS installed, licensed, etc.)
3. wait until the first model (DAY_AHEAD_A) has finished
4. source 'Intermediate_scripts/intermediate_reserves.R'
5. source 'Intermediate_scripts/intermediate_look_ahead.R'
6. wait until other runs finish