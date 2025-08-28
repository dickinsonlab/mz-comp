#New approach
#I pooled all of the B73 final peaks from my replicates into one column
B73<-read.csv("B73peaks.csv", stringsAsFactors = FALSE)

cluster_values <- function(values, tolerance = 0.001) {
  values <- sort(values)
  clusters <- list()
  current_cluster <- c(values[1])
  
  for (i in 2:length(values)) {
    if (abs(values[i] - tail(current_cluster, 1)) <= tolerance) {
      current_cluster <- c(current_cluster, values[i])
    } else {
      clusters[[length(clusters) + 1]] <- current_cluster
      current_cluster <- c(values[i])
    }
  }
  clusters[[length(clusters) + 1]] <- current_cluster
  
  # Return the mean of each cluster
  sapply(clusters, mean)
}

unique_vals_B73 <- cluster_values(B73$Rep1, tolerance = 0.001)
write.csv(data.frame(unique_vals_B73), "unique_vals_B73.csv", row.names = FALSE)

OG <-read.csv("OGpeaks.csv", stringsAsFactors = FALSE)


unique_val_OG <- cluster_values(OG$Rep1, tolerance = 0.001)
write.csv(data.frame(unique_val_OG), "unique_vals_OG.csv", row.names = FALSE)

___________________

#Comparing between B73 and OG 

B73comp <- read.csv("unique_vals_B73.csv")
OGcomp <- read.csv("unique_vals_OG.csv")

B73vals <- as.numeric(B73comp$unique_vals_B73)
OGvals <- as.numeric(OGcomp$unique_val_OG)

find_matches <- function(source, target, tolerance = 0.001) {
  sapply(source, function(x) any(abs(x - target) <= tolerance))
}

shared_1 <- B73vals[find_matches(B73vals, OGvals)]
shared_2 <- OGvals[find_matches(OGvals, B73vals)]
shared <- unique(round(c(shared_1, shared_2), 6))

unique_to_B73 <- B73vals[!find_matches(B73vals, OGvals)]
unique_to_OG <- OGvals[!find_matches(OGvals, B73vals)]

write.csv(data.frame(shared), "shared_values.csv", row.names = FALSE)
write.csv(data.frame(unique_to_B73), "unique_to_B73.csv", row.names = FALSE)
write.csv(data.frame(unique_to_OG), "unique_to_OG.csv", row.names = FALSE)


#had issue with more values identified than were in lists to fix use this code

tolerance <- 0.001

# Get matches from B73 to OG and record which B73 indices matched
b73_matched <- logical(length(B73vals))
og_matched  <- logical(length(OGvals))

for (i in seq_along(B73vals)) {
  diffs <- abs(B73vals[i] - OGvals)
  if (any(diffs <= tolerance)) {
    b73_matched[i] <- TRUE
    og_matched[which.min(diffs)] <- TRUE  # match the closest OG value only
  }
}

# Shared values from B73 (could just use OG side too)
shared_from_B73 <- B73vals[b73_matched]
shared_both <- unique(round(shared_from_B73, 6))  # rounding for final output
write.csv(data.frame(shared_both), "final_shared_values.csv", row.names = FALSE)
# Unique to B73
unique_to_B73 <- B73vals[!b73_matched]
write.csv(data.frame(unique_to_B73), "final_unique_B73.csv", row.names = FALSE)
#Unique to OG
unique_to_OG <- OGvals[!og_matched]
write.csv(data.frame(unique_to_OG), "final_unique_OG.csv", row.names = FALSE)
