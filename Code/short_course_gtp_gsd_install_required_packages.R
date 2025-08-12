##### README ###################################################################
# Filename:      short_course_gtp_gsd_install_required_packages.R              #
# Author(s):     Michael Grayling (mgraylin@its.jnj.com)                       #
#                Yevgen Tymofyeyev (ytymofye@its.jnj.com)                      #
# Description:   Installs the packages required for the practicals in the      #
#                Short Course 'An introduction to graphical testing procedures #
#                for group-sequential designs', given at MCP2025.              #
# Last modified: 2025/08/11                                                    #
################################################################################

##### INSTALL REQUIRED PACKAGES ################################################

### Graphical testing ###

# For Practical 1, you can choose to use {gMCPLite} and {gMCP}, or use
# {graphicalMCP}. Comment out below as needed
install.packages("gMCP")
install.packages("gMCPLite")
install.packages("graphicalMCP")

### Group-sequential design ###

# For Practical 1, you can choose to use {gsDesign} or {rpact}. Comment out
# below as needed
install.packages("gsDesign")
install.packages("rpact")

### Other ###

# The 'GTP-GSD Template' package discussed in the Short Course. This will make
# sure all other required packages for Practical 2 are installed
install.packages("devtools")
devtools::install_github("ytymofyeyev/appendMCP")
