# Mana Tūāpapa Future Leader Fellowship selection algorithm

## Description

Simple R function to randomly draw fellows from an applicant pool while satisfying, or exceeding, a given set of diversity targets.
Within the actual ballot, the Mana Tūāpapa Future Leader Fellowship Selection Panel both generates a random seed and chooses the excellence threshold. 

## Usage

mana_tuapapa_selection_excellence(dataframe, ma = 4, pa = 2, ff = 10, fe = 20, threshold = 3.5)

## Arguments

*dataframe* a dataframe of applicants structured as id, Gender coded as c("M", "F", "GD", "NR"), Ethnicity coded as c("ma", "pa", "ma/pa", "pool")  
*ma*        the target minimum draw for Māori fellows  
*pa*        the target minimum draw for Pacific Nation fellows  
*ff*        the target minimum draw for female fellows  
*fe*        the total number of fellows to draw  
*threshold* the minimum excellence score to be considered for the ballot.

## Details

The *mana_tuapapa_selection_excellence* function makes extensive use the dplyr library's slice and filter functions to draw fellows from the different applicant groups.  
Some degree of sense is required of inputs; under plausible real-world applicant populations, it is guaranteed to satisfy Māori and Pacific Nation targets, and unless ma+pa is set high and/or GD population large and a very very unlucky draw occurs, the female fellow target.  

## Value

A dataframe of *fe* selected Fellows having columns: Gender, Ethnicity, id, and ballot (i.e. group in which the fellow was selected).
As a side-effect this function creates 'unchosen_applicant_list', an ordered dataframe of excellent applicants that were not selected

## Example

    set.seed(0)
	require(tidyr)
	require(dplyr)
	require(stringr)

	# Create synthetic population
	demo_population <-uncount(data.frame(Gender = rep(c("M", "F", "GD", "NR"), times=4), Ethnicity = rep(c("Ma", "Pa", "Ma/Pa", "Pool"), each=4), 
                                     Count = c(9,15,3,0,3,3,0,3,3,3,3,0,411,408,3,24)), weights = Count, .id = "id")  %>% mutate(Score = runif(n=891, min = 3, max = 5))

	# Simulate selection

	example_selection <- mana_tuapapa_selection_excellence(demo_population)
	print(addmargins(xtabs(~Gender + Ethnicity, example_selection)))

	#       Ethnicity
	# Gender Ma Ma/Pa Pa Pool Sum
	# F    1     2  0    7  10
	# GD   0     1  0    0   1
	# M    0     1  0    7   8
	# NR   0     0  1    0   1
	# Sum  1     4  1   14  20

	summary(example_selection$Score)

	# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
	# 3.544   3.880   4.345   4.266   4.650   4.903 
