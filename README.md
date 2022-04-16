# Prototype-Footwear-Testing

### PrototypeShoeAnalysis.m
MATLAB code to analysis biomechanical data.  Code loads in joint angle data from squats and run, and parses out each squat/run.  Ensemble curves are then created by taking the average of all squats/runs for each condition.  Variables of interest are then calculated.

Section 1A: One subject at a time - parses out each squat from the squat trial, and each right stance phase for each shoe (10 stance phases per shoe condition)
Section 1B: One subject at a time - continuation of 1A to plot joint angles across all squats and across all stance phases for each shoe (10 lines per plot)
Section 2: Multiple subjects - loops through data structure to create ensemble curves for each joint in each shoe condition
Section 3: Multiple subjects - finds max/min/footstrike of joint angles, and calculates other variables of interest

### PrototypeShoeAnalysis_timeseries.m
MATLAB code to create an Excel file with each subjects time-normalized data for a given variable (GRFz is default but any variable can be used). Excel file is long form of data where columns are: Subject ID, Group (if applicable), % Stance Phase, and a colunmn for each shoe condition containing variable data.

### getTrials.m
Helper function to PrototypeShoeAnalysis.m that parses out each squat from the squat trial waveform.  GUI pops up with knee angle waveform plot, and user selects the "peaks" in the plot (since waveform has negative values, the peaks are actually where the leg is straight and knee angle is close to 0).  Selection does not need to be exact.

### plot4lvlstruct.m
Helper function to PrototypeShoeAnalysis.m that loops through the data structure to create ensemble curves for each joint angle in each shoe condition.  Also highlights max, min, and values at footstrike.

### Study4Figures.R
This code generates figures from biomechanics and survey data, according to questions asked by industry partner. Some terms have been redacted or modified so as to maintain classified nature of some of the material.
