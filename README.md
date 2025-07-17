# User Manual: Downward Continuation and redatuming to the seafloor of Wide Angle Seismic (WAS) data

Welcome to the user manual for the Downward Continuation (DC) software designed specifically for marine Ocean Bottom Seismometer (OBS) field data. The DC process involves a virtual transformation of OBS gathers from datum 1 (shots at the sea surface) to datum 2 (shots at the seafloor).

As inputs:
- Each OBS gather must be provided in a file with SU format.
- The bathymetry data of the shots should be provided in an ASCII file.
- Additionally, a series of parameters need to be provided through an input file, which is read during execution.
- By default, the software assumes a constant p-wave velocity model for the water column. However, you can also provide XBT data to construct a specific Vp model.

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
- The software was used in \cite{estela} and is available on GitHub: https://github.com/ejimeneztejero/DC_WAS.
- A dataset for testing and validation is available on Zenodo \cite{estela2} (DOI: https://doi.org/10.5281/zenodo.16031283).
