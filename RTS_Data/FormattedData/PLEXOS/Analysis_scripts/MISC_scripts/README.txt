
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Folder contains scripts which are no longer used but which
# I didn't quite want to delete
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -----------------------------------------------|
# alternative_interface_definitions/
# -----------------------------------------------|

Folder with price-flow plotting scripts that test other ways to define interfaces
between ISOs. See tags:

EDGE - interface price is the average price in all PLEXOS regions which border another ISO
ex. NYISO's interface price with ISO-NE would be the average of prices in North, Capital, Hudson Valley, etc. but not West.

REGION - interface price is the average price in all PLEXOS regions in an ISO

LINE - each line between two ISOs is treated as a seperate interface with its own price difference based on the nodal prices
at its end buses

# -----------------------------------------------|
# alternative_regression/
# -----------------------------------------------|

unfinished attempt to regress against price differences between ISOs at the line rather than interface level

# -----------------------------------------------|
# autocorrelation/ 
# -----------------------------------------------|

Scripts which start with the price-flow plot outputs and plot the price autocorrelation for the ISO data 
and single and multi-operator models

# -----------------------------------------------|
# database_analysis/
# -----------------------------------------------|

Scripts which analyze aspects of the Seams database, mainly interface transmission. Scripts are old and may need
a few changes to get working

extract_interface_lines.R - tables of interfaces lines
flow_by_voltage.R - characterizes the amount of interface flow by the voltage at which it occurs
node_analysis.R - interface nodes by voltage
