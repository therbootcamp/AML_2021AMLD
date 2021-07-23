# R Studio Settings ----- 
# 1.  Panel icon -> switch to Console on right
# 2.  Go to preferences and switch of workspace storing

# Create Project -----
# 1.  Create project named BaselRBootcamp
# 2.  Create appropriate folder structure from within RStudio

# R as a calculator ----- 

# 1.  In console

# Calculate 2 + 2
2 + 2

# Create objects via assignment
result <- 2 + 2

# Print result
result

# Use result
result + 2

# Reevalute result
result

# Overwrite result
result <- result + 2

# 1.  In script
# 1.1 Open and save new script
# 1.2 Repeat stuff by sending it to console

# Calculate 2 + 2
2 + 2

# Create results objects via assignment
result <- 2 + 2

# Print result
result

# Use result: add 2
result + 2

# Reevalute result
result

# Overwrite result
result <- result + 2

# Functions in R ----- 

# Create a numeric vector called age
age <- c(21, 34, 12, 76)

# Ccalculate mean age (auto-complete)
mean(age)

# Store result
mean_age <- mean(age)

# Add NA to age
age <- c(21, 34, 12, 76, NA)

# Compute mean
mean(age)

# Check help
?mean

# Use n.rm argument
mean(age, na.rm=T)

# Close RStudio & Reopen ----



