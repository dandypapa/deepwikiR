# Sample R code for testing

# A simple function to add two numbers
add_numbers <- function(a, b) {
  return(a + b)
}

# A helper function
process_greeting <- function(name_str) {
  return(paste("Processed:", name_str))
}

# Another simple function that calls a user-defined helper
greet_person <- function(name) {
  processed_name <- process_greeting(name) # Call to user-defined function
  message <- paste("Hello,", processed_name, "!")
  print(message) # Call to base function
  add_numbers(1,2) # Call to another user-defined function
  return(message)
}

# A top-level call to ensure functions are invoked
result <- greet_person("World")
sum_example <- add_numbers(5, 3)

