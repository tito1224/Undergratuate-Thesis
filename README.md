# Undergratuate-Thesis
Code for final year thesis project. The folders necessary are the "DigitalAllianceOutput", "Functions", "Huggins".

# Data 
There are multiple .rds files where capture histories were stored. For each .rds there are 1000 runs or "capture histories". To be more specific, each .rds returns three datasets:
  1. Dataset of summary statistics (this can be ignored because we want summary stats on the combined data from all replicates and scenarios).
  2. A dataframe of estimates of N and p. 
  3. A dataframe which contains the capture histories used as input into program MARK
  
Each dataframe contains the estimates and capture histories of 1000 runs or "simulations". So since there are 100 scenarios (and .rds files) and 1000 runs, we end up with 100 x 1000 estimates of N and 100 x 1000 estimates of p. 

At first I found estimates by maximizing the full likelihood, but it led to some instability in estimates. So I generated data again and found estimates by using the Huggins model. Estimates with the "full likelihood" approach can be found in the folder "./DigitalAllianceOutput/Final" with the subfolders Final, Final2 and Final3. The .rds are in these three folders because at the time I generated the data in batches. They have to be kept in those three files because the combination/simulation numbers overlap. Code in the the .Rmd that reads that data and analyzes it is called "Final.Rmd" and it loads the data properly and is in the main folder.

The .Rmd file for analyzing estimates made via maximizing the conditional likelihood (Huggins model) can be found in the folder "./Huggins" and all the .rds files are located there. These are the estimates that are preferred, and the .Rmd that analyzes this is called "Final Huggins.Rmd". 

# .Rmd
The process for generating capture histories was done in steps, and the various .Rmd files reflect those steps. To see how the functions used to generate capture histories and make estimates, and eventually run simulations works, the .Rmd files are a good way to do so. In order from earliest to latest they are: 
  1. Process to Create Point Count Data.Rmd
  2. Process to Create Errors in Point Count Data.Rmd
  3. Simulation Intro.Rmd
  4. Final.Rmd
  5. Final Huggins.Rmd

# Functions

The functions used to generate capture histories and run simulations can be found in the ./Functions folder. The shell script used for generating simulations is also present there and can be modified if necessary to run on the HPC. 

# To Add
- process of running scripts on HPC? need to mention how to get Rmark working on there?
- detail how simulations work/walk through script?
