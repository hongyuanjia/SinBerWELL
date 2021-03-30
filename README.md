# Building energy simulation on radiant cooling + ventilation

[![Launch RStudio Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/2326226)

## How to access

You can try out this project as long as you have a browser and an
internet connection. [Click here](https://rstudio.cloud/project/2326226) to
navigate your browser to an RStudio Cloud instance. Alternatively, you can clone
or download this code repository and install the required R packages by calling
`renv::restore()`.

## How to run

* In the R console, run `targets::tar_make()`

## File structure

The main files are:

```
.
├── renv.lock
├── R
│   └── functions.R
├── data-raw
├── data
│   ├── idf
│   │   └── ...
│   ├── results
│   │   └── ...
│   └── sim
│       └── ...
└── _targets.R
```

| File                                                                                  | Purpose                                                                                                                                                                                                                                      |
| ---                                                                                   | ---                                                                                                                                                                                                                                          |
| [`renv.lock`](https://github.com/hongyuanjia/SinBerWELL/blob/main/renv.lock)          | The [renv](https://rstudio.github.io/renv/index.html) lockfile, describing the state of libraries used in this projected.                                                                                                                    |
| [`_targets.R`](https://github.com/hongyuanjia/SinBerWELL/blob/main/_targets.R)        | The R script that declares the targets pipeline which shows the core workflow, including model creation, data analytics and visualization. The full workflow can be rerun using [`targets::tar_make()`](https://docs.ropensci.org/targets/). |
| [`R/functions.R`](https://github.com/hongyuanjia/SinBerWELL/blob/main/R/functions.R)  | An R script with helper functions to modify EnergyPlus and extract results.                                                                                                                                                                  |
| [`data-raw/*.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/main/data)          | The Singapore large office reference building [EnergyPlus](https://energyplus.net/) model developed by [SinBerBEST](http://doi.wiley.com/10.1002/ente.201700564).                                                                            |
| [`data-raw/*.epw`](https://github.com/hongyuanjia/SinBerWELL/blob/main/data)          | The [IWEC](https://energyplus.net/weather/sources#IWEC) EnergyPlus weather file.                                                                                                                                                             |
| [`data/idf/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf)         | The processed model generated during the calibration workflow.                                                                                                                                                                               |
| [`data/sim/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/sim)         | The individual simulation results including input IDF and EPW, ERR and table report.                                                                                                                                                         |
| [`data/results/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/results) | The simulation result summary including EUI and its breakdowns, together with the energy saving compared with the baseline.                                                                                                                  |
| [`figures/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/figures)           | Plots about the energy savings compared to the baseline and indoor air temperature distribution.                                                                                                                                             |

## Scenarios investigated

This project investigated 8 different scenarios using the Singapore large office
reference building, including:

* Baseline (data/idf/baseline.idf): Business-as-usual scenario with indoor air temperature setpoint at
  23C.
  See [`data/idf/baseline.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/baseline.idf).

* Raised indoor setpoints: Increase indoor air setpoint temperatures to 26C,
  together with increased setpoints in the HVAC cooling equipments and ceiling
  fan for thermal comfort compensation.
  See [`data/idf/fans.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/fans.idf).

For radiant cooling systems, 6 different scenarios are considered:

| # | Name                                                                                                                        | Setpoint   | Raidant chilled water source | Ventilation Type               | Auxiliary cooling |
|---|-----------------------------------------------------------------------------------------------------------------------------|------------|------------------------------|--------------------------------|-------------------|
| 1 | [`radiant_natvent_28C.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant_natvent_28C.idf)         | 28°C ± 2°C | Constant COP chiller         | Natural ventilation with 5 ACH | No                |
| 2 | [`radiant_natvent_29C.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant_natvent_28C.idf)         | 29°C ± 1°C | Constant COP chiller         | Natural ventilation with 5 ACH | No                |
| 3 | [`radiant_natvent_vav_28C.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant_natvent_vav_29C.idf) | 28°C ± 2°C | Constant COP chiller         | Natural ventilation with 5 ACH | VAV               |
| 4 | [`radiant_natvent_vav_28C.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant_natvent_vav_29C.idf) | 29°C ± 1°C | Constant COP Chiller         | Natural ventilation with 5 ACH | VAV               |
