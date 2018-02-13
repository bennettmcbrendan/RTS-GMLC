
# -----------------------------------------------|
# RTS_geodecomp_analysis_DA.Rmd
# -----------------------------------------------|

Compares decomposed and non-decomposed RTS-GMLC solutions and outputs

1) basic metrics tables (DA and RT)
2) warnings if sanity checks on fixed generation, reserves, etc. do not check out
2) commitment plot
3) interface time series
4) KSI plot (for interface flows)
5) miscellaneous statistics including RMS calculations


# -----------------------------------------------|
# RTS_geodecomp_analysis_RT.Rmd
# -----------------------------------------------|

Outdated script from when we used to run a full RTS RT as stage C rather than power flow

# -----------------------------------------------|
# SINGLE-OPT_injections.R
# -----------------------------------------------|

Writes nondecomposed (single-operator) SEAMS nodal injections and withdrawals as csvs in the directory below:
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\SEAMS-data-nondecomposed

# -----------------------------------------------|
# MULTI-OPT_injections.R
# -----------------------------------------------|

Writes decomposed (multi-operator) SEAMS nodal injections and withdrawals as csvs in the directory below:
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\SEAMS-data-decomposed
requires a few hours to run

# -----------------------------------------------|
# MULTI-OPT_price_flow_plots.R
# -----------------------------------------------|

Makes multi-operator price flow plots from MATPOWER outputs located in the directory below
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\SEAMS-data-decomposed-matpower

# -----------------------------------------------|
# ERGIS-ISO_price_flow_comparison.R
# -----------------------------------------------|

Script which compares price-flow plots made with 5-minute RT ERGIS results and real ISO data
relies on inputs from data/real-world-iso-efficiency-data.csv

# -----------------------------------------------|
# ST_time_boxplots.R
# -----------------------------------------------|

Script which makes the two computation time boxplots for the journal paper
relies on inputs from data/

# -----------------------------------------------|
# RTS_injections.R
# -----------------------------------------------|

Writes decomposed RTS-GMLC nodal injections and withdrawals (single or multi-operator) as csvs in the directories below:
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\geo-decomp-data-decomposed
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\geo-decomp-data-nondecomposed

# -----------------------------------------------|
# RTS_price_flow_plots.R
# -----------------------------------------------|

Makes price-flow plots from single and multi-operator RTS power flow ouptuts
relies on inputs from:
\\nrelqnap01d\PLEXOS\Projects\GMLC-MSPCM\geo-decomp-matpower-solns


