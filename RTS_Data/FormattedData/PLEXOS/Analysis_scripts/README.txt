
# -----------------------------------------------|
# RTS_geodecomp_analysis_DA.Rmd
# -----------------------------------------------|

Compares decomposed stage B and non-decomposed RTS-GMLC solutions:

1) basic metrics tables (DA and RT)
2) warnings if sanity checks on fixed generation, reserves, etc. do not check out
3) unit commitment plot (fig 2)


# -----------------------------------------------|
# RTS_geodecomp_analysis_RT.Rmd
# -----------------------------------------------|

Compares decomposed stage C and non-decomposed RTS-GMLC solutions:

1) metrics on starts, generation and congestion

# -----------------------------------------------|
# RTS_price_flow_plots.R
# -----------------------------------------------|

Script which makes price-flow plots for the RTS-GMLC and provides related metrics
1) quadrants - number of points in each quadrant
2) int.stat.center - average price difference
3) outlier.data - number of outliers

Note: the 'Inputs' section is currently set up to run for the interface between region 1 and region 3

# -----------------------------------------------|
# RTS_regression.Rmd
# -----------------------------------------------|

Runs regression analysis on RTS-GMLC solutions - not used in paper

# -----------------------------------------------|
# run_paper_plots.R
# -----------------------------------------------|

Script which calls other scripts in source_scripts/ folder to do the majority of the Eastern Interconnection analysis

# -----------------------------------------------|
# (1) source_scripts/REAL-ISO_price_flow_plots.R
# (2) source_scripts/SINGLE-OPT-BORDER_price_flow_plots.R
# (3) source_scripts/MULTI-OPT-BORDER_price_flow_plots.R
# -----------------------------------------------|

Called by run_paper_plots.R

Make price-flow plots and provide metrics on how many points in each quadrant, average price differences, etc. for:
(1) ISO data
(2) single-operator EI model
(3) multi-operator EI model

Note: The 'BORDER' label in the title means that the script calcuates interface price by averaging all nodes which connect
to inter-ISO transmission with voltage at or above the input parameter 'voltage.threshold'

# -----------------------------------------------|
# source_scripts/ALL_price_flow_plots-fold.R
# -----------------------------------------------|

Additional sourced script which takes the data for an unfolded price-flow plot and outputs a folded price-flow plot

not used in paper, probably will not need

# -----------------------------------------------|
# (1) source_scripts/plot_parameters.R
# (2) source_scripts/plot_theme.R
# -----------------------------------------------|

Sourced scripts with ggplot() theme objects and a couple functions used in SQLite queries

# -----------------------------------------------|
# Seams_regression_INTERFACE.R
# -----------------------------------------------|

Runs linear regression (Equation 26) on Seams results

# -----------------------------------------------|
# regression_bar_plot.R
# -----------------------------------------------|

Makes Figure 6 in the geo decomp paper. Note that it is necessary to hard-code in the number of significant coefficients from
Seams_regression_INTERFACE.R each time it is re-run