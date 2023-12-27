# hydroDCindex
R package for compute Duration Curve Hydrological Model Indexes based on Yilmaz (Yilmaz et al., 2008) methodology

# Requirements
Dependencies: stats, ie2misc

# Installation
You can install the development version of **hydroDCindex** from GitHub with this R command:
```
# install.packages("remotes")
remotes::install_github("Alobondo/hydroDCindex")
library(hydroDCindex)
```

# Usage
Functions | Description |
--- | --- |
```hydroDC_Index(Q_obs, Q_sim, c_opt)``` | Compute Duration Curve Hydrological Model Indexes. |
```Q_obs``` | Column with daily observed flows. |
```Q_sim``` | Column with daily simulates flows. |
```c_opt``` | Results, default 1 for indexes, 2 for duration curve values, 3 for DC plot and 4 for scatter plot. |

```hydroDCindex::Q_obs``` Example containing observed flows.

```hydroDCindex::Q_sim``` Example containing simulated flows.

# Reporting bugs
If you find an error in some function, want to report a typo in the documentation or submit a recommendation, you can do it [here](https://github.com/Alobondo/hydroDC_Index/issues)

# Keywords
Hydrology, R package, Duration Curve, Goodness of Fit

# References
Yilmaz, K. K., H. V. Gupta, and T. Wagener (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res.,44, W09417, doi:10.1029/2007WR006716
