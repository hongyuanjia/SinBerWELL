library(targets)
library(eplusr)
source("R/functions.R")

tar_option_set(format = "qs")

targets <- list(
    tar_target(path_idf, "data-raw/model.idf", format = "file"),
    tar_target(path_epw, "data-raw/SGP_Singapore.486980_IWEC.epw", format = "file"),

    tar_target(
        param,
        {

            # baseline
            baseline <- eplusr::read_idf(path_idf)
            baseline$load(
                "
                Output:Variable,*,Zone Ventilation Air Change Rate,hourly; !- HVAC Average [ach]
                Output:Variable,*,Zone Ventilation Fan Electricity Energy,hourly; !- HVAC Sum [J]
                Output:Variable,*,Zone Ventilation Air Inlet Temperature,hourly; !- HVAC Average [C]
                Output:Variable,*,Chiller Part Load Ratio,hourly; !- HVAC Average []
                Output:Variable,*,Chiller Evaporator Cooling Rate,hourly; !- HVAC Average [W]
                Output:Variable,*,Chiller Electricity Rate,hourly; !- HVAC Average [W]
                Output:Variable,*,Fan Electricity Rate,hourly; !- HVAC Average [W]
                "
            )

            # 2020s --> current SinBerBEST solution
            # Setpoint temperature increase (3ºC) + Fans
            # Supply air temperature increase (3ºC)
            # Chilled water temperature increase (2ºC)
            # LED Lighting
            # Daylighting control
            # Occupancy localization
            # - Occupancy schedule reduced by 60%
            # - Plug load schedule reduced by 30%
            # - Lighting load schedule reduced by 60%
            # CO2 based demand ventilation control
            p2020s <- apply_combined_strategies(baseline$clone(),
                indoor = 3, coil = 2, economizer = 2, evaporator = 3,
                sch_frac_occu = 0.4, sch_frac_equip = 0.7, sch_frac_light = 0.4,
                CO2_ctrl = TRUE, dayl_ctrl = TRUE, LED = TRUE, lowe = TRUE
            )

            # 2030s --> toward natural ventilation
            p2030s <- apply_combined_strategies(baseline$clone(),
                indoor = 4, coil = 2, economizer = 2, evaporator = 3,
                sch_frac_occu = 0.4, sch_frac_equip = 0.7, sch_frac_light = 0.4,
                CO2_ctrl = TRUE, dayl_ctrl = TRUE, LED = TRUE, lowe = TRUE
            )
            p2030s <- add_hybrid_ventilation(p2030s$clone())

            baseline$save("data/baseline.idf", overwrite = TRUE)
            p2020s$save("data/p2020s.idf", overwrite = TRUE)
            p2030s$save("data/p2030s.idf", overwrite = TRUE)

            group <- eplusr::group_job(
                list(baseline = baseline, `2020s` = p2020s, `2030s` = p2030s),
                path_epw
            )

            group$run("data/sim")

            group
        }
    ),

    # extract results
    tar_target(
        end_uses,
        {
            param <- reload(tar_read(param))

            end_uses <- read_end_uses(param)[, .(case, category, subcategory, electricity)]
            end_uses <- end_uses[!category %in% c("Humidification", "Heat
                Recovery", "Refrigeration", "Water Systems", "Generators",
                "Heating", "Exterior Lights", "Exterior Equipment")]
            end_uses <- units::drop_units(end_uses)
            end_uses[, savings := (electricity[1] - electricity)/electricity[1],
                by = .(category, subcategory)]

        }, cue = tar_cue("always")
    ),

    tar_target(
        utility,
        {
            param <- reload(tar_read(param))
            utility <- param$tabular_data(
                table_name = "Utility Use Per Total Floor Area",
                column_name = "Electricity Intensity"
            )[, .(case, category = row_name, electricity = as.numeric(value))]
            utility[, savings := (electricity[1] - electricity)/electricity[1],
                by = .(category)]
        }, cue = tar_cue("always")
    ),

    # save results
    tar_target(
        path_results,
        {
            path <- c(end_uses = "data/end_uses.csv", utility = "data/utility.csv")
            data.table::fwrite(end_uses, path["end_uses"])
            data.table::fwrite(utility, path["utility"])
            path
        },
        format = "file", cue = tar_cue("always")
    )
)

tar_pipeline(targets)
