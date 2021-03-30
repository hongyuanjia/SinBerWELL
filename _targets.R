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

    tar_target(path_radiant_natvent_28C, format = "file", {
        # read the SBB2 model
        idf <- eplusr::read_idf(path_fans)

        # turn off the VAV system
        idf$add(
            Schedule_Constant = list("Sch_Always_Off", "On/Off", 0),
            AvailabilityManager_Scheduled = list("Availability_Always_Off", "Sch_Always_Off"),
            AvailabilityManagerAssignmentList = list(
                "Availability_List_Always_Off",
                "AvailabilityManager:Scheduled",
                "Availability_Always_Off"
            )
        )
        idf$set(
            AirLoopHVAC := list(availability_manager_list_name = "Availability_List_Always_Off")
        )

        # add radiant floor and ceiling
        idf$add(
            Material = list(
                "SGP_Floor_Finishing", "Smooth",
                0.0016, 0.17, 1922, 1250, 0.9, 0.5, 0.5),

            Construction_InternalSource = list(
                "SGP_Floor_Radiant",
                source_present_after_layer_number = 2,
                temperature_calculation_requested_after_layer_number = 2,
                dimensions_for_the_ctf_calculation = 1,
                tube_spacing = 0.1524,
                two_dimensional_temperature_calculation_position = 0,
                outside_layer = "SGP_Concrete_100mm",
                layer_2 = "SGP_Gypsum_Board_15mm",
                layer_3 = "SGP_Floor_Finishing")
        )
        idf$load("
            Material,
                Metal_Panel,             !- Name
                MediumSmooth,            !- Roughness
                0.005,                   !- Thickness {m}
                62,                      !- Conductivity {W/m-K}
                7580,                    !- Density {kg/m3}
                485,                     !- Specific Heat {J/kg-K}
                0.9,                     !- Thermal Absorptance
                0.5,                     !- Solar Absorptance
                0.5;                     !- Visible Absorptance
        ")
        idf$add(Construction_InternalSource = list(
            "SGP_Drop_Ceiling_Radiant",
            source_present_after_layer_number = 1,
            temperature_calculation_requested_after_layer_number = 1,
            dimensions_for_the_ctf_calculation = 1,
            tube_spacing = 0.1524,
            two_dimensional_temperature_calculation_position = 0,
            outside_layer = "SGP_Semi_Rigid_Insulation_75mm",
            layer_2 = "Metal_Panel"
        ))

        # add low temperature radiant cooling and heating
        radiant_floor <- new_radiant_cooling(idf,
            get_conditioned_zones(idf), idf$object("SGP_Floor_Radiant")$id(),
            type = "floor", setpoint = c(20, 28), throttling_range = 2
        )
        radiant_ceiling <- new_radiant_cooling(idf,
            get_conditioned_zones(idf), idf$object("SGP_Drop_Ceiling_Radiant")$id(),
            type = "ceiling", setpoint = c(20, 28), throttling_range = 2
        )
        idf$set(c(radiant_floor, radiant_ceiling) := list(availability_schedule_name = "Sch_ACMV"))

        # add radiant cooling/heating as the 1st zone hvac
        add_equipment_to_zone(idf,
            radiant_floor,
            get_conditioned_zones(idf),
            seq_cooling = 1, seq_heating = 1
        )
        add_equipment_to_zone(idf,
            radiant_ceiling,
            get_conditioned_zones(idf),
            seq_cooling = 2, seq_heating = 2
        )

        # create radiant equipment branches
        node_dict <- get_pair_node_dict(idf)
        ## cooling branch
        bran_radiant_floor_cooling <- new_branch(idf, radiant_floor, "Cooling", "water", node_dict[grepl("Cooling", inlet)])
        bran_radiant_ceiling_cooling <- new_branch(idf, radiant_ceiling, "Cooling", "water", node_dict[grepl("Cooling", inlet)])
        ## heating branch
        bran_radiant_floor_heating <- new_branch(idf, radiant_floor, "Heating", "water", node_dict[grepl("Heating", inlet)])
        bran_radiant_ceiling_heating <- new_branch(idf, radiant_ceiling, "Heating", "water", node_dict[grepl("Heating", inlet)])

        # create a chiller for radiant cooling
        # NOTE: Here I use a constant COP. We need a quite high chilled water
        #       temperature and I did not find specs of high temperature chiller
        radiant_chiller <- get_named_id(idf$add(Chiller_ConstantCOP := list(
            name = "Radiant_Chiller",
            nominal_capacity = "Autosize",
            nominal_cop = 6.2,
            design_chilled_water_flow_rate = "Autosize",
            design_condenser_water_flow_rate = "Autosize",
            chilled_water_inlet_node_name = "Radiant_Cooling_Tower_Inlet_Node",
            chilled_water_outlet_node_name = "Radiant_Cooling_Tower_Outlet_Node",
            condenser_type = "AirCooled",
            sizing_factor = 1.15
        )))

        # clone a variable chiller pump
        radiant_pump <- get_named_id(idf$dup("Radiant_Cooling_Pump" = "ChW_Pump"))
        # rename the nodes and capacity
        idf$set(c(radiant_pump) := list(
            inlet_node_name = "Radiant_Cooling_Supply_Inlet_Node",
            outlet_node_name = "Radiant_Cooling_Pump_Outlet_Node"
        ))

        # create a new chilled water loop for the radiant cooling system
        radcooling_loop <- new_chilled_water_loop(idf, "RadCooling",
            supply_setpoint = 25,
            id_chiller = radiant_chiller,
            id_pump = radiant_pump,
            ids_equip = c(bran_radiant_floor_cooling, bran_radiant_ceiling_cooling),
            use_equip_branch = TRUE
        )

        # have to create a heating water loop
        heater <- get_named_id(idf$add(DistrictHeating = list("Purchased_Heating", "Purchased_Heating_Inlet_Node", "Purchased_Heating_Outlet_Node", 1E6)))
        heat_pump <- get_named_id(idf$add(Pump_VariableSpeed = list("HW_Pump", "HW_Supply_Inlet_Node", "HW_Pump_Outlet_Node",
            "Autosize", 2E5, 30000, 0.9, 0, 0, 1, 0, 0, 0, "Intermittent")))
        radheating_loop <- new_hot_water_loop(idf, "HW",
            supply_setpoint = 60,
            id_boiler = heater,
            id_pump = heat_pump,
            ids_equip = c(bran_radiant_floor_heating, bran_radiant_ceiling_heating),
            use_equip_branch = TRUE
        )

        # add single cooling thermostat for autosizing radiant system
        set_thermostat_singlecooling(idf, core = 28, perimeter = 28)

        # add natural ventilation at 5ACH with maximum outdoor air temperature
        # at 30C
        add_natural_ventilation(idf, ach = 5, max_oa_temp = 30)

        # add outputs
        idf$add(Output_Variable = list("*", "Cooling Tower Outlet Temperature", "Hourly"))

        # save model
        idf$save("data/idf/radiant_natvent_28C.idf", overwrite = TRUE)
        "data/idf/radiant_natvent_28C.idf"
    }),

    tar_target(path_radiant_natvent_vav_28C, format = "file", {
        # read the radiant model
        idf <- eplusr::read_idf(targets::tar_read(path_radiant_natvent_28C))

        # turn on VAV system
        idf$set(AirLoopHVAC := list(availability_manager_list_name = NULL))

        # add hybrid ventilation
        zones <- get_conditioned_zones(idf)
        idf$add(
            Schedule_Constant = list("Sch_Mode_HybridVent", "Control Type", 1),
            Schedule_Constant = list("Sch_Min_OutAirVent", "On/Off", 0),
            AvailabilityManager_HybridVentilation := list(
                name = sprintf("%s_HybridVent", names(zones)),
                control_zone_name = names(zones),
                ventilation_control_mode_schedule_name = "Sch_Mode_HybridVent",
                zoneventilation_object_name = sprintf("NatVent_%s", names(zones)),
                maximum_outdoor_temperature = 30,
                maximum_outdoor_enthalpy = 290000,
                minimum_outdoor_ventilation_air_schedule_name = "Sch_Min_OutAirVent",
                simple_airflow_control_type_schedule_name = "Sch_Always_On"
            )
        )

        # save model
        idf$save("data/idf/radiant_natvent_vav_28C.idf", overwrite = TRUE)
        "data/idf/radiant_natvent_vav_28C.idf"
    }),

    # change to higher setpoint
    tar_target(path_radiant_natvent_29C, format = "file", {
        # read the radiant model
        idf <- eplusr::read_idf(targets::tar_read(path_radiant_natvent_28C))

        # change setpoint and throttling range
        idf$set(
            Sch_Zone_Radiant_Cooling_Setpoint = list(field_4 = "29"),
            Sch_Zone_Cooling_Setpoint_Wo_Solar = list(field_4 = "29"),
            Sch_Zone_Cooling_Setpoint_Solar = list(field_4 = "29"),
            ZoneHVAC_LowTemperatureRadiant_VariableFlow := list(cooling_control_throttling_range = 1)
        )

        # save model
        idf$save("data/idf/radiant_natvent_29C.idf", overwrite = TRUE)
        "data/idf/radiant_natvent_29C.idf"
    }),

    # change to higher setpoint
    tar_target(path_radiant_natvent_vav_29C, format = "file", {
        # read the radiant with VAV model
        idf <- eplusr::read_idf(targets::tar_read(path_radiant_natvent_vav_28C))

        # change setpoint and throttling range
        idf$set(
            Sch_Zone_Radiant_Cooling_Setpoint = list(field_4 = "29"),
            Sch_Zone_Cooling_Setpoint_Wo_Solar = list(field_4 = "29"),
            Sch_Zone_Cooling_Setpoint_Solar = list(field_4 = "29"),
            ZoneHVAC_LowTemperatureRadiant_VariableFlow := list(cooling_control_throttling_range = 1)
        )

        # save model
        idf$save("data/idf/radiant_natvent_vav_29C.idf", overwrite = TRUE)
        "data/idf/radiant_natvent_vav_29C.idf"
    }),

    tar_target(path_sim, format = "file", {
        grp <- eplusr::group_job(
            c(path_baseline, path_fans,
              path_radiant_natvent_28C,
              path_radiant_natvent_vav_28C,
              path_radiant_natvent_29C,
              path_radiant_natvent_vav_29C
            ),
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
