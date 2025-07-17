# User Manual: Downward Continuation and redatuming to the seafloor of Wide Angle Seismic (WAS) data

Welcome to the user manual for the Downward Continuation (DC) software designed specifically for marine Ocean Bottom Seismometer (OBS) field data. The DC process involves a virtual transformation of OBS gathers from datum 1 (shots at the sea surface) to datum 2 (shots at the seafloor).

As inputs:
- Each OBS gather must be provided in a file with SU format.
- The bathymetry data of the shots should be provided in an ASCII file.
- Additionally, a series of parameters need to be provided through an input file, which is read during execution.

## Installation
- To install and use this software effectively, please refer to the instructions provided in the PDF manual located in this directory:  [docs/UserGuide_DC_WAS.pdf](docs/UserGuide_DC_WAS.pdf)

## Testing with WAS data
For testing the code, please download the sample data test at Zenodo [2] and follow the readme.

## Development
- Development is hosted on GitHub repository:
[github/ejimeneztejero/DC_WAS](https://github.com/ejimeneztejero/DC_WAS).

## Author
- Clara Estela Jiménez Tejero is the author of this software.
- The software was developed at the Barcelona Center for Subsurface Imaging, ICM-CSIC.
Feel free to customize this README as needed based on your specific requirements and preferences.

## References
- The software was used in [1] and a dataset for testing and validation is available on Zenodo [2]:
[1] C E Jimenez-Tejero, M Prada, L Gomez de la Peña, C R Ranero, V Sallares, Downward continuation of wide-angle seismic data: implications for traveltime tomography uncertainty, Geophysical Journal International, Volume 241, Issue 3, June 2025, Pages 1868–1880,  \url{https://doi.org/10.1093/gji/ggaf137}.
[2] Jimenez Tejero, C. E. (2025). Data sample for testing the code: Downward Continuation for WAS data (DC_WAS) [Data set]. In Geophysical Journal International (Vol. 241, Number 3, pp. 1868–1880). Zenodo. https://doi.org/10.5281/zenodo.16031283.
