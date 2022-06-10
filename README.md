# hydroDCindex
R package for compute Duration Curve Hydrological Model Indexes based on Yilmaz (Yilmaz et al., 2008) methodology

# Installation
You can install the development version of **hydroCHISQR** from GitHub with this R command:
```
# install.packages("remotes")
remotes::install_github("Alobondo/hydroDCindex")
library(hydroDCindex)
```

# Usage
Functions | Description |
--- | --- |
```hydroDC_Index(Q_obs, Q_sim, c_res)``` | Compute Duration Curve Hydrological Model Indexes. |
```Q_obs``` | Column with daily observed flows. |
```Q_sim``` | Column with daily simulates flows. |
```c_res``` | Results, default 1 for indexes, 2 for duration curve values and 3 for plot. |

```hydroDC_Index::Q_obs``` Example containing observed flows.

```hydroDC_Index::Q_sim``` Example containing simulated flows.

# Reporting bugs
If you find an error in some function, want to report a typo in the documentation or submit a recommendation, you can do it [here](https://github.com/Alobondo/hydroDC_Index/issues)

# Keywords
Hydrology, R package, Duration Curve, Goodness of Fit
