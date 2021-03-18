library(targets)
library(eplusr)
eplusr_option(verbose_info = FALSE)
source("R/functions.R")

tar_option_set(format = "qs")

targets <- list(
    tar_target(path_idf, "data-raw/model.idf", format = "file"),
    tar_target(path_epw, "data-raw/SGP_Singapore.486980_IWEC.epw", format = "file"),

    targets::tar_target(path_baseline, format = "file", {
        # read the original benchmark model
        idf <- eplusr::read_idf(path_idf)

        # change setpoint temperature to 23C
        set_thermostat_singlecooling(idf, core = 23, perimeter = 23)

        # save
        idf$save("data/idf/baseline.idf", overwrite = TRUE)
        "data/idf/baseline.idf"
    }),

    tar_target(path_sat, format = "file", {
        # read the original benchmark model
        idf <- eplusr::read_idf(path_idf)

        # change setpoint temperature to 23C
        set_thermostat_singlecooling(idf, core = 26, perimeter = 26)

        # add ceiling fans
        add_ceiling_fan(idf, 0.5)

        # set air velocity around people
        set_air_velocity(idf, 0.3)

        # save
        idf$save("data/idf/sat.idf", overwrite = TRUE)
        "data/idf/sat.idf"
    }),

    tar_target(path_radiant1, format = "file", {
        # read the SBB2 model
        idf <- eplusr::read_idf(path_sat)

        # turn off air system
        turn_off_air_system(idf)

        # add natural ventilation
        add_natural_ventilation(idf, ach = 5, max_oa_temp = 30)

        # add radiant cooling floors and set setpoint temperature to 28-29C
        add_radiant_floor(idf, core = c(28, 30), perimeter = c(28, 30))

        # save
        idf$save("data/idf/radiant1.idf", overwrite = TRUE)
        "data/idf/radiant1.idf"
    }),

    tar_target(path_radiant2, format = "file", {
        # read the SBB2 model
        idf <- eplusr::read_idf(path_sat)

        # turn off air system
        turn_off_air_system(idf)

        # add natural ventilation
        add_natural_ventilation(idf, ach = 5, max_oa_temp = 30)

        # add radiant cooling floors and set setpoint temperature to 28-29C
        add_radiant_floor(idf, core = c(29, 30), perimeter = c(29, 30))

        # save
        idf$save("data/idf/radiant2.idf", overwrite = TRUE)
        "data/idf/radiant2.idf"
    }),

    tar_target(path_sim, format = "file", {
        grp <- eplusr::group_job(
            c(path_baseline, path_sat, path_radiant1, path_radiant2),
            "data-raw/SGP_Singapore.486980_IWEC.epw"
        )
        grp$run("data/sim")

        qs::qsave(grp, "data/sim/sim.qs")
        "data/sim/sim.qs"
    }),

    tar_target(eui, {
        sim <- eplusr::reload(qs::qread(path_sim))
        eui <- read_eui(sim)[type == "Total Site", .(case, eui = energy_per_total_area)]
        eui[, savings := round((eui[1] - eui)/eui[1], 3)]
    }),

    tar_target(utility, {
        sim <- eplusr::reload(qs::qread(path_sim))
        utility <- sim$tabular_data(
            table_name = "Utility Use Per Total Floor Area",
            column_name = "Electricity Intensity"
        )[, .(case, category = row_name, electricity = as.numeric(value))]
        utility[, savings := (electricity[1] - electricity)/electricity[1],
            by = .(category)]
        utility[, savings := round(savings, 3)]
    }),

    tar_target(end_uses, {
        sim <- eplusr::reload(qs::qread(path_sim))
        end_uses <- read_end_uses(sim)[, .(case, category, subcategory, electricity)]
        end_uses <- end_uses[!category %in% c("Humidification", "Heat
            Recovery", "Refrigeration", "Water Systems", "Generators",
            "Heating", "Exterior Lights", "Exterior Equipment")]
        end_uses[, electricity := round(electricity, 2)]
        end_uses[, savings := (electricity[1] - electricity)/electricity[1],
            by = .(category, subcategory)]
        end_uses[, savings := round(savings, 3)]
    }),

    tar_target(path_results, format = "file", {
        path <- c(
            eui      = "data/results/eui.csv",
            utility  = "data/results/utility.csv",
            end_uses = "data/results/end_uses.csv"
        )
        data.table::fwrite(eui, path["eui"])
        data.table::fwrite(utility, path["utility"])
        data.table::fwrite(end_uses, path["end_uses"])
        path
    })
)
