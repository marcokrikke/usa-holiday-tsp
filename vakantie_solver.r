#!/usr/bin/Rscript

# Travelling Salesmen Problem library
library(TSP)

# --- SETTINGS ---
# Distance matrix input file
distance_matrix_input <- "~/Desktop/cities 3.tsv"

# Set the method (e.g. nearest_insertion or cheapest_insertion) of the TSP library
global_method <- "nearest_insertion"

# Check if the distance matrix is symmetric
check_distance_matrix_symmetric <- FALSE

# Read cities data (distance matrix) from TSV file
data<-read.csv(distance_matrix_input, sep = "\t", header=TRUE, row.names = 1)

# Check if distance matrix is symmetric
if (check_distance_matrix_symmetric) {
	isSymmetric(t(data))
}

do_tsp <- function(data, start, used_method) {
	d<-dist(data)
	tsp <- TSP(d)
	initial_tour <- solve_TSP(tsp, method = used_method, control=list(start=start))

	# Refine
	tour <- solve_TSP(tsp, method="two_opt", control=list(tour=initial_tour))

	return(tour)
}

# Sum all distances between cities in the tour
calculate_tour_length <- function(data, tour) {
	tourlength <- 0

	for (i in 1:(length(names(tour))-1)) {
		tourlength <- tourlength + as.integer(data[names(tour)[i],names(tour)[i+1]])
	}

	# Last city back to the first one
	tourlength <- tourlength + as.integer(data[names(tour)[i+1],names(tour)[1]])

	return(tourlength)
}

# Print all stages of the tour
print_stages <- function(data, tour) {

	for (i in 1:(length(names(tour))-1)) {
		print(paste(names(tour)[i], "->", names(tour)[i+1], ":", as.integer(data[names(tour)[i],names(tour)[i+1]])))
	}

	# Last city back to the first one
	print(paste(names(tour)[i+1], "->", names(tour)[1], ":", as.integer(data[names(tour)[i+1],names(tour)[1]])))
}

# Calculate tours

print("Distance matrix: ")
print(data)

print("")
print("TSP method used: ")
print(global_method)


# All cities
print("")
print("All cities")
tour <- do_tsp(data, 7L, global_method)

labels(tour)
print_stages(data, tour)

tourlength <- calculate_tour_length(data, tour)
print(paste("Total tour length:", tourlength))


# All cities east of LV (inclusive)
print("")
print("All cities east of LV, start and end in SF")
tour <- do_tsp(data[-(2:6),-(2:6)], 2L, global_method)

labels(tour)
print_stages(data, tour)

tourlength <- calculate_tour_length(data, tour)
print(paste("Total tour length:", tourlength))


# All cities west of LV (inclusive)
print("")
print("All cities west of LV, start and end in LV")
tour <- do_tsp(data[1:6,1:6], 1L, global_method)

labels(tour)
print_stages(data, tour)

tourlength <- calculate_tour_length(data, tour)
print(paste("Total tour length:", tourlength))
