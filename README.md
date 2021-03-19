# Building energy simulation on radiant cooling floor + natural ventilation

[![Launch RStudio Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/2306579)

## How to access

You can try out this project as long as you have a browser and an
internet connection. [Click here](https://rstudio.cloud/project/2306579) to
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

| File                                                                                  | Purpose                                                                                                                                                           |
| ---                                                                                   | ---                                                                                                                                                               |
| [`renv.lock`](https://github.com/hongyuanjia/SinBerWELL/blob/main/renv.lock)          | The [renv](https://rstudio.github.io/renv/index.html) lockfile, describing the state of libraries used in this projected.                                         |
| [`R/functions.R`](https://github.com/hongyuanjia/SinBerWELL/blob/main/R/functions.R)  | An R script with helper functions to midfy EnergyPlus and extract results.                                                                                        |
| [`data-raw/*.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/main/data)          | The Singapore large office reference building [EnergyPlus](https://energyplus.net/) model developed by [SinBerBEST](http://doi.wiley.com/10.1002/ente.201700564). |
| [`data-raw/*.epw`](https://github.com/hongyuanjia/SinBerWELL/blob/main/data)          | The [IWEC](https://energyplus.net/weather/sources#IWEC) EnergyPlus weather file.                                                                                  |
| [`data/idf/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf)         | The processed model generated during the calibration workflow.                                                                                                    |
| [`data/sim/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/sim)         | The individual simulation results including input IDF and EPW, ERR and table report.                                                                              |
| [`data/results/`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/results) | The simulation result summary including EUI and its breakdowns, together with the energy saving compared with the baseline.                                       |

## Scenarios investigated

This project investigated 4 different scenarios using the Singapore large office
reference building, including:

* Baseline (data/idf/baseline.idf): Business-as-usual scenario with indoor air temperature setpoint at
  23C.
  See [`data/idf/baseline.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/baseline.idf).

* Raised indoor setpoints: Increase indoor air setpoint temperatures to 26C,
  together with increased setpoints in the HVAC cooling equipments and ceiling
  fan for thermal comfort compensation.
  See [`data/idf/fans.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/fans.idf).

* Radiant floors: Use radiant cooling floors together with natural ventilation
  to further increase the indoor setpoint temperature to 28C -- 30C.
  See [`data/idf/radiant1.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant1.idf).

* Radiant floors 2: Same as above, but with a narrower and more aggressive setpoint temperature of 29C -- 30C.
  See [`data/idf/radiant2.idf`](https://github.com/hongyuanjia/SinBerWELL/blob/master/data/idf/radiant2.idf).
