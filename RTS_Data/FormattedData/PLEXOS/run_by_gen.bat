cd "C:\Program Files (x86)\Energy Exemplar\PLEXOS 7.3\"
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "DAY_AHEAD_A" \n
start "" "C:\Program Files\R\R-3.3.2\bin\Rscript.exe" "\\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\Intermediate_scripts\intermediate_reserves.R"
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "DAY_AHEAD" \n
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "DAY_AHEAD_B1" \n
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "DAY_AHEAD_B2" \n
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "DAY_AHEAD_B3" \n
.\PLEXOS64.EXE \\plexossql\Data\bmcbenne\RTS-GMLC-geodecomp\RTS-GMLC\RTS_Data\FormattedData\PLEXOS\\RTS-GMLC-GeoDecomp_bygen2.xml \m "REAL_TIME_C" \n
