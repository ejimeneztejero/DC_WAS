# User Manual: Downward Continuation and redatuming to the seafloor of WAS (OBS) data

Welcome to the user manual for the Downward Continuation (DC) software designed specifically for marine OBS (Ocean Bottom Seismometer) field data. The DC process involves a virtual transformation of OBS gathers from datum 1 (shots at the sea surface) to datum 2 (shots at the seafloor).

As inputs:
- Each OBS gather must be provided in a file with SU format.
- The bathymetry data of the shots should be provided in an ASCII file.
- Additionally, a series of parameters need to be provided through an input file, which is read during execution.
- By default, the software assumes a constant p-wave velocity model for the water column. However, you can also provide XBT data to construct a specific Vp model.

## Installation
- To install and use this software effectively, please refer to the instructions provided in the PDF manual located in this directory:  [UserGuide_DC_WAS.pdf](UserGuide_DC_WAS.pdf)

## Development
- Development is hosted on GitHub repository:
[github/ejimeneztejero/DC_WAS](https://github.com/ejimeneztejero/DC_WAS).

## Author
- Clara Estela Jiménez Tejero is the author of this software.
- The software was developed at the Barcelona Center for Subsurface Imaging, ICM-CSIC.
Feel free to customize this README as needed based on your specific requirements and preferences.
