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

    tar_target(path_param_radiant, format = "file", {
        # read the SBB2 model
        idf <- eplusr::read_idf(tar_read(path_fans))

        # remove all existing HVAC
        remove_all_vavbox(idf)
        remove_all_airloop(idf)
        remove_all_waterloop(idf)

        # remove toilet fans
        # otherwise DOAS cannot be autosized
        for (fan in idf$Fan_ZoneExhaust) {
            ref <- fan$value_relation(NULL, depth = NULL, "ref_by")$ref_by
            del_if_exist(idf, c(ref$object_id, ref$src_object_id))
        }

        idf$save(tempfile(fileext = ".idf"))

        param <- eplusr::param_job(idf, path_epw)

        # explore all possible solutions for the radiant system
        create_radiant <- function(idf, setpoint, type, throttling_range,
                                   chilled_water_type, loop_setpoint,
                                   natvent = TRUE, doas = FALSE) {

            # add deadband thermostat for autosizing radiant system
            set_thermostat_singlecooling(idf, setpoint, setpoint)

            if (natvent) add_natural_ventilation(idf, ach = 5, max_oa_temp = setpoint)

            add_radiant_cooling(idf,
                core = c(20, setpoint), perimeter = c(20, setpoint),
                type = type[[1]], control_throttling_range = throttling_range,
                chilled_water_type = chilled_water_type, loop_setpoint = loop_setpoint
            )

            if (doas) add_doas(idf, sat = 20)

            idf
        }

        values <- data.table::rbindlist(list(
            # 1. radiant cooling cooling supplied by a chiller without ventilation
            list(.names = "radiant_28C_chiller_nonv", setpoint = 28, chilled_water_type = "chiller", loop_setpoint = 15, natvent = FALSE, doas = FALSE),

            # 2. radiant cooling cooling supplied by a chiller with natural ventilation
            list(.names = "radiant_28C_chiller_nv", setpoint = 28, chilled_water_type = "chiller", loop_setpoint = 15, natvent = TRUE, doas = FALSE),

            # 3. radiant cooling cooling supplied by a chiller with DOAS
            list(.names = "radiant_28C_chiller_doas", setpoint = 28, chilled_water_type = "chiller", loop_setpoint = 15, natvent = FALSE, doas = TRUE),

            # 4. radiant cooling cooling supplied by a cooling tower without ventilation
            list(.names = "radiant_28C_tower_nonv", setpoint = 28, chilled_water_type = "cooling_tower", loop_setpoint = -5, natvent = FALSE, doas = FALSE),

            # 5. radiant cooling cooling supplied by a chiller with natural ventilation
            list(.names = "radiant_28C_tower_nv", setpoint = 28, chilled_water_type = "cooling_tower", loop_setpoint = -5, natvent = TRUE, doas = FALSE),

            # 6. radiant cooling cooling supplied by a chiller with DOAS
            list(.names = "radiant_28C_tower_doas", setpoint = 28, chilled_water_type = "cooling_tower", loop_setpoint = -5, natvent = FALSE, doas = TRUE)
        ))
        values[, `:=`(type = list(c("floor", "ceiling")), throttling_range = 2)]

        # create parametric models
        do.call(param$apply_measure, c(create_radiant, values))

        # save models
        paths <- param$save("data/idf", separate = FALSE)
        # remove copied weather file
        unlink(unique(paths$weather))

        paths$model
    }),

    tar_target(path_sim, format = "file", {
        grp <- eplusr::group_job(
            c(path_baseline, path_fans, path_param_radiant),
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
        cols <- paletteer::paletteer_dynamic("cartography::green.pal", length(unique(utility$case)))

        ggplot(utility[category %in% c("HVAC", "Total")]) +
            geom_col(aes(category, electricity, fill = case), position = "dodge") +
            geom_text(aes(category, electricity, group = case,
                    label = scales::percent({savings[savings < 0.005] <- NA;savings}, 0.1)),
                position = position_dodge(width = 0.9), vjust = -0.5, na.rm = TRUE,
                color = "darkgreen", size = 3) +
            scale_x_discrete(NULL) +
            scale_y_continuous(expression("Electricity [ " * "kWh" / (m^2 ~ yr) * " ]")) +
            scale_fill_manual(values = cols) +
            guides(fill = guide_legend("Case", ncol = 4)) +
            theme_minimal() +
            theme(legend.position = "top")
    }),

    tar_target(plot_temp, {
        idf <- eplusr::read_idf(path_baseline)
        zones <- idf$to_table(class = "ZoneControl:Thermostat")[
            field == "Zone or ZoneList Name", toupper(value)]

        sim <- eplusr::reload(qs::qread(path_sim))
        res <- sim$report_data(NULL, zones, name = "zone mean air temperature",
            environment_name = "runperiod 1", hour = 8:17, day_type = "weekdays")

        cols <- paletteer::paletteer_dynamic("cartography::green.pal", length(unique(res$case)))

        ggplot(res, aes(case, value, color = case)) +
            geom_boxplot() +
            scale_x_discrete(NULL) +
            scale_y_continuous(expression("Indoor Temperature [ "~ degree * C *" ]")) +
            scale_color_manual(values = cols) +
            guides(color = guide_legend("Case", ncol = 4)) +
            theme_minimal() +
            theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1))

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
