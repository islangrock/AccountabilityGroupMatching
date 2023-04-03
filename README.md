# Accountability Group Matching Script for the UPENN Grad Center Writing Fellows
## Prepared: Fall 2022 

This R script uses kmeans to build groups based off of shared preferences and availability of grad students who indicate interest via the Qualtrics Accountability Group Form. Students with more shared availability are grouped together. The script should be run in RStudio or RStudio Cloud and will need the tidyverse library installed. For the basics of R/RStudio set-up visit: http://www.sthda.com/english/wiki/r-basics-quick-and-easy

**Please note, if you change the survey this script might not work correctly.**

The script will produce one csv file of the survey responses and the group variable. Writing Fellows *should manually review this file and make any necessary changes to the groups*. Often there is a large drop-out rate in the accountability groups, so I recommend erring on the side of setting up larger groups (5-6) in the hopes that a small subset (2-3) will continue to meet. Groups are introduced via email (Templates in the Writing Fellows Box Folder) with their names, departments, and the most overlapping meeting times (requires review of the survey responses). We have also had some success with an optional in-person launch event at the beginning of the semester. 
