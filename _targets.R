library(targets)
library(eplusr)
library(ggplot2)
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

    tar_target(path_fans, format = "file", {
        # read the original benchmark model
        idf <- eplusr::read_idf(path_idf)

        # change setpoint temperature to 26C
        set_thermostat_singlecooling(idf, core = 26, perimeter = 26)

        # change SAT to from 12.8C to 16C
        set_coil_setpoint(idf, 16)

        # change chilled water temperature from 6.7C to 10C
        set_evaporator_setpoint(idf, 10)

        # add ceiling fans
        add_ceiling_fan(idf, 0.5)

        # set air velocity around people
        set_air_velocity(idf, 0.3)

        # save
        idf$save("data/idf/fans.idf", overwrite = TRUE)
        "data/idf/fans.idf"
    }),

    tar_target(path_radiant1, format = "file", {
        # read the SBB2 model
        idf <- eplusr::read_idf(path_fans)

        # remove existing airloop
        remove_airloop(idf$AirLoopHVAC$VAV_Bot)
        remove_airloop(idf$AirLoopHVAC$VAV_Mid)
        remove_airloop(idf$AirLoopHVAC$VAV_Top)

        # remove existing plant loop
        remove_waterloop(idf$PlantLoop$ChW)
        # remove existing condenser loop
        remove_waterloop(idf$CondenserLoop$CndW)

        # add natural ventilation
        add_natural_ventilation(idf, ach = 5, max_oa_temp = 30)

        # add deadband thermostat for autosizing radiant system
        set_thermostat_singlecooling(idf, 28, 28)

        # add radiant cooling floors and set setpoint temperature to 28C
        add_radiant_floor(idf, core = c(20, 28), perimeter = c(20, 28))

        # set cooling throttling range to 1C to achieve control temperature
        # range of 28C-30C
        idf$set("ZoneHVAC:LowTemperatureRadiant:VariableFlow" := list(
            cooling_control_throttling_range = 2
        ))

        # remove VAV box
        idf$ZoneHVAC_AirDistributionUnit <- NULL
        idf$AirTerminal_SingleDuct_VAV_NoReheat <- NULL
        zones <- idf$to_table(class = "ZoneControl:Thermostat")[
            field == "Zone or ZoneList Name", value]
        conn <- idf$to_table(class = "ZoneHVAC:EquipmentConnections", wide = TRUE)[`Zone Name` %in% zones]
        conn[, `:=`(`Zone Air Inlet Node or NodeList Name` = NA_character_)]
        idf$update(eplusr::dt_to_load(conn))

        # save
        idf$save("data/idf/radiant1.idf", overwrite = TRUE)
        "data/idf/radiant1.idf"
    }),

    tar_target(path_radiant2, format = "file", {
        # read the radiant model
        idf <- eplusr::read_idf(path_radiant1)

        # update setpoint
        idf$set(
            "Sch_Zone_Cooling_Setpoint_Wo_Solar" = list(field_4 = "29"),
            "Sch_Zone_Cooling_Setpoint_Solar" = list(field_4 = "29"),
            "Sch_Zone_RadiantCooling_Setpoint_Core" = list(field_4 = "29"),
            "Sch_Zone_RadiantCooling_Setpoint_Perimeter" = list(field_4 = "29")
        )

        # set cooling throttling range to 1C to achieve control temperature
        # range of 29C-30C
        idf$set("ZoneHVAC:LowTemperatureRadiant:VariableFlow" := list(
            cooling_control_throttling_range = 1
        ))

        # save
        idf$save("data/idf/radiant2.idf", overwrite = TRUE)
        "data/idf/radiant2.idf"
    }),

    tar_target(path_sim, format = "file", {
        grp <- eplusr::group_job(
            c(path_baseline, path_fans, path_radiant1, path_radiant2),
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
    }),

    tar_target(plot_utility, {
        ggplot(utility[category %in% c("HVAC", "Total")]) +
            geom_col(aes(category, electricity, fill = case), position = "dodge") +
            geom_text(aes(category, electricity, group = case,
                    label = scales::percent({savings[savings < 0.005] <- NA;savings}, 0.1)),
                position = position_dodge(width = 0.9), vjust = -0.5, na.rm = TRUE,
                color = "darkgreen", size = 4) +
            scale_x_discrete(NULL) +
            scale_y_continuous(expression("Electricity [ " * "kWh" / (m^2 ~ yr) * " ]")) +
            scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(utility$case)) + 1, "Greens")[-1]) +
            guides(fill = guide_legend("Case")) +
            theme_minimal() +
            theme(legend.position = "top")
    }),

    tar_target(plot_temp, {
        idf <- eplusr::read_idf(path_baseline)
        zones <- idf$to_table(class = "ZoneControl:Thermostat")[
            field == "Zone or ZoneList Name", toupper(value)]

        sim <- eplusr::reload(qs::qread(path_sim))
        res <- sim$report_data(NULL, zones, name = "zone mean air temperature", all = TRUE,
            environment_name = "runperiod 1", hour = 8:18)

        ggplot(res, aes(case, value, color = case)) +
            geom_boxplot() +
            scale_x_discrete(NULL) +
            scale_y_continuous(expression("Indoor Temperature [ "~ degree * C *" ]")) +
            scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(res$case)) + 2, "Greens")[-c(1, 2)]) +
            guides(color = guide_legend("Case")) +
            theme_minimal() +
            theme(legend.position = "top")
    }),

    tar_target(path_plots, format = "file", {
        path <- c(
            utility = "figures/utility.png",
            temp    = "figures/temp.png"
        )
        ggsave(path["utility"], plot_utility, width = 8, height = 4, dpi = 600)
        ggsave(path["temp"], plot_temp, width = 8, height = 4, dpi = 600)
        path
    })
)
