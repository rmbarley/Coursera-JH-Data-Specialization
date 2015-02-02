Final Project: Getting and Cleaning Data
=========================
This repository contains my final project for the Coursera Data Science Specialization course Getting and Cleaning Data. 

It contains:

1. run_analysis.r : this R script takes a collection of messy data from collected from the accelerometers from the Samsung Galaxy S smartphone (see http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones for details)

2. tidy.txt : The output from run_analysis.r

3. codebook.md: A codebook outlining the variable and techniques used in run_analysis.r

4. README.md: This readme file

Using this script:
-------------------
* Load run_analysis.r in your favorite text editor
* run_analysis.r assumes that you are able to connect to the internet. It will automatically download and unzip the files from the web into your working directory. 
* After it is run, it will output a text file named tidy.txt to your working directory.

The data for this project was retrieved from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
