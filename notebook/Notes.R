# To learn R packaging in github
# see https://ourcodingclub.github.io/tutorials/writing-r-package/
install.packages("devtools")
install.packages("roxygen2")

# Load the package directly without compiling!
library(devtools)
load_all(".")# Working directory should be in the package 

# To build help files if coded correctly in the function (@param, @return, @examples)
library(roxygen2) # Read in the roxygen2 R package
roxygenise()      # Builds the help files


# Create .Rbuildignore
usethis::use_build_ignore("placeholder")

## Configure git in console

# Name associated to git interactions
# git config --global user.name "Deepak"

# Set email address
# git config --global user.email "deepakgeorgep@gmail.com"

# Set default name branch for new repositories to main
# git config --global init.defaultBranch main

# Set so git will ignore filemode permission changes
# git config --global core.filemode false

# Configure how git handles line endings in files
# git config --global core.autocrlf input

# Set up git in R Studio
# Tools> Global options> Git/SVN > Git executable is /usr/bin/git
# Create SSH key
# Copy the SSH key and add to github account under settings
# return to R-Studio console and run ssh -T git@github.com to see whether it authenticate
# You should get "Hi deepak-pazhayamadom! You've successfully authenticated, but GitHub does not provide shell access."


