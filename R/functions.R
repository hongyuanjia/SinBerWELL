# set_thermostat_singlcooling {{{
set_thermostat_singlecooling <- function(idf, core, perimeter) {
    checkmate::assert_number(core, lower = 16, upper = 30)
    checkmate::assert_number(perimeter, lower = 16, upper = 30)

    idf$set("Sch_Zone_Thermostat_Control_Type" = list(field_4 = "2"))

    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    eplusr::without_checking({
        idf$set("ZoneControl:Thermostat" := list(
            control_1_object_type = "ThermostatSetpoint:SingleCooling"
        ))

        nm_sch <- idf$object_name("Schedule:Compact")[[1]]

        if ("Sch_Zone_Cooling_Setpoint_Wo_Solar" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Wo_Solar" = list(
                field_4 = as.character(core)
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Wo_Solar",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(core)
            ))
        }

        if ("Sch_Zone_Cooling_Setpoint_Solar" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Solar" = list(
                field_4 = as.character(perimeter)
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Solar",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(perimeter)
            ))
        }

        # remove existing
        idf$ThermostatSetpoint_SingleCooling <- NULL
        idf$ThermostatSetpoint_SingleHeating <- NULL
        idf$ThermostatSetpoint_SingleHeatingOrCooling <- NULL
        idf$ThermostatSetpoint_DualSetpoint <- NULL

        sgl <- idf$to_table(class = rep("ThermostatSetpoint:SingleCooling", length(zones)),
            init = TRUE, all = TRUE, wide = TRUE)

        sgl[, Name := sprintf("%s_SPSched", zones)]

        sgl[grepl("^Core", Name), `:=`(
            `Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Wo_Solar"
        )]
        sgl[!grepl("^Core", Name), `:=`(
            `Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Solar"
        )]

        idf$load(eplusr::dt_to_load(sgl))
    })

    idf
}
# }}}

# set_thermostat_deadband {{{
set_thermostat_deadband <- function(idf, core, perimeter) {
    checkmate::assert_double(core, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)
    checkmate::assert_double(perimeter, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)

    idf$set("Sch_Zone_Thermostat_Control_Type" = list(field_4 = "4"))

    eplusr::without_checking({
        idf$set("ZoneControl:Thermostat" := list(
            control_1_object_type = "ThermostatSetpoint:DualSetpoint"
        ))

        nm_sch <- idf$object_name("Schedule:Compact")[[1]]

        if ("Sch_Zone_Cooling_Setpoint_Lower_Core" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Lower_Core" = list(
                field_4 = as.character(core[1])
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Lower_Core",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(core[1])
            ))
        }

        if ("Sch_Zone_Cooling_Setpoint_Lower_Perimeter" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Lower_Perimeter" = list(
                field_4 = as.character(perimeter[1])
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Lower_Perimeter",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(perimeter[1])
            ))
        }

        if ("Sch_Zone_Cooling_Setpoint_Upper_Core" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Upper_Core" = list(
                field_4 = as.character(core[2])
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Upper_Core",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(core[2])
            ))
        }

        if ("Sch_Zone_Cooling_Setpoint_Upper_Perimeter" %in% nm_sch) {
            idf$set("Sch_Zone_Cooling_Setpoint_Upper_Perimeter" = list(
                field_4 = as.character(perimeter[2])
            ))
        } else {
            idf$add(Schedule_Compact = list(
                "Sch_Zone_Cooling_Setpoint_Upper_Perimeter",
                "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
                as.character(perimeter[2])
            ))
        }

        # remove existing
        idf$ThermostatSetpoint_SingleHeating <- NULL
        idf$ThermostatSetpoint_SingleCooling <- NULL
        idf$ThermostatSetpoint_SingleHeatingOrCooling <- NULL
        idf$ThermostatSetpoint_DualSetpoint <- NULL

        dual <- idf$to_table(class = rep("ThermostatSetpoint:DualSetpoint", length(zones)),
            init = TRUE, all = TRUE, wide = TRUE)

        dual[, Name := sprintf("%s_SPSched", zones)]

        dual[grepl("^Core", Name), `:=`(
            `Cooling Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Upper_Core",
            `Heating Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Lower_Core"
        )]
        dual[!grepl("^Core", Name), `:=`(
            `Cooling Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Upper_Perimeter",
            `Heating Setpoint Temperature Schedule Name` = "Sch_Zone_Cooling_Setpoint_Lower_Perimeter"
        )]

        idf$load(eplusr::dt_to_load(dual))
    })

    idf
}
# }}}

# add_radiant_cooling {{{
add_radiant_cooling <- function(idf, core, perimeter, type = c("floor", "ceiling"),
                                control_throttling_range = 2,
                                chilled_water_type = "chiller", loop_setpoint = 20) {
    checkmate::assert_double(core, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)
    checkmate::assert_double(perimeter, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)
    checkmate::assert_subset(type, c("floor", "ceiling"), empty.ok = FALSE)
    checkmate::assert_choice(chilled_water_type, c("chiller", "cooling_tower"))

    # add control temperature schedules {{{
    nm_sch <- idf$object_name("Schedule:Compact")[[1]]
    if ("Sch_Zone_Radiant_Heating_Setpoint_Core" %in% nm_sch) {
        idf$set("Sch_Zone_Radiant_Heating_Setpoint_Core" = list(
            field_4 = as.character(core[1])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_Radiant_Heating_Setpoint_Core",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(core[1])
        ))
    }
    if ("Sch_Zone_Radiant_Heating_Setpoint_Perimeter" %in% nm_sch) {
        idf$set("Sch_Zone_Radiant_Heating_Setpoint_Perimeter" = list(
            field_4 = as.character(perimeter[1])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_Radiant_Heating_Setpoint_Perimeter",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(perimeter[1])
        ))
    }
    if ("Sch_Zone_Raidant_Cooling_Setpoint_Core" %in% nm_sch) {
        idf$set("Sch_Zone_Radiant_Cooling_Setpoint_Core" = list(
            field_4 = as.character(core[2])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_Radiant_Cooling_Setpoint_Core",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(core[2])
        ))
    }
    if ("Sch_Zone_Radiant_Cooling_Setpoint_Perimeter" %in% nm_sch) {
        idf$set("Sch_Zone_Radiant_Cooling_Setpoint_Perimeter" = list(
            field_4 = as.character(perimeter[2])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_Radiant_Cooling_Setpoint_Perimeter",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(perimeter[2])
        ))
    }
    # }}}

    # add radiant surfaces {{{
    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    type[type == "floor"] <- "Floor"
    type[type == "ceiling"] <- "Ceiling"
    # get surf
    surf <- idf$to_table(class = "BuildingSurface:Detailed", wide = TRUE)[
        `Surface Type` %in% type & `Zone Name` %in% zones,
        .(id, name, zone = `Zone Name`, type = `Surface Type`)]

    # get surface areas in order to calculate flow fraction for each surface
    surf[, area := idf$geometry()$area(object = surf$name)$area]
    surf[, fraction := round(area / sum(area), 3), by = "zone"]
    surf[, area := NULL]

    # create radiant floor construction
    if ("Floor" %in% type) {
        idf$add(
            Material = list(
                "SGP_Floor_Finishing", "Smooth",
                0.0016, 0.17, 1922, 1250, 0.9, 0.5, 0.5
            ),

            Construction_InternalSource = list(
                "SGP_Floor_Radiant",
                source_present_after_layer_number = 2,
                temperature_calculation_requested_after_layer_number = 2,
                dimensions_for_the_ctf_calculation = 1,
                tube_spacing = 0.1524,
                two_dimensional_temperature_calculation_position = 0,
                outside_layer = "SGP_Concrete_100mm",
                layer_2 = "SGP_Gypsum_Board_15mm",
                layer_3 = "SGP_Floor_Finishing"
            )
        )
        idf$set(.(surf[type == "Floor", id]) := list(construction_name = "SGP_Floor_Radiant"))
    }
    if ("Ceiling" %in% type) {
        # add metal panel
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
        idf$set(.(surf[type == "Ceiling", id]) := list(construction_name = "SGP_Drop_Ceiling_Radiant"))
    }

    # group surfaces by zone and type to create radiant surface groups
    rad_surf <- surf[, by = c("zone", "type"), {
        val <- mapply(function(surf, frac) c(surf, frac), name, fraction, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        val <- c(sprintf("%s_Radiant_%s_Surfaces", .BY$zone, .BY$type), unlist(val))
        list(class = "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup", index = seq_along(val), value = val)
    }]
    rad_surf[, `:=`(id = data.table::rleid(zone, type))]
    idf$load(rad_surf)
    # }}}

    # create low temperature radiant {{{
    rad_meta <- rad_surf[, list(surface = value[1L]), by = c("id", "zone", "type")]
    rad_meta[, name := sprintf("%s_Radiant_%s", zone, type)]
    rad <- idf$to_table(class = rep("ZoneHVAC:LowTemperatureRadiant:VariableFlow", nrow(rad_meta)),
        init = TRUE, wide = TRUE)
    rad[, `:=`(
        Name = rad_meta$name,
        `Availability Schedule Name` = "Sch_ACMV",
        `Zone Name` = rad_meta$zone,
        `Surface Name or Radiant Surface Group Name` = rad_meta$surface,
        `Heating Design Capacity` = "Autosize",
        `Heating Water Inlet Node Name` = sprintf("%s_Heating_Water_Inlet_Node", rad_meta$name),
        `Heating Water Outlet Node Name` = sprintf("%s_Heating_Water_Outlet_Node", rad_meta$name),
        `Heating Control Temperature Schedule Name` = sprintf("Sch_Zone_Radiant_Heating_Setpoint_%s",
            ifelse(grepl("^Core", rad_meta$zone), "Core", "Perimeter")),
        `Maximum Hot Water Flow` = "Autosize",
        `Cooling Design Capacity` = "Autosize",
        `Cooling Water Inlet Node Name` = sprintf("%s_Cooling_Water_Inlet_Node", rad_meta$name),
        `Cooling Water Outlet Node Name` = sprintf("%s_Cooling_Water_Outlet_Node", rad_meta$name),
        `Cooling Control Temperature Schedule Name` = sprintf("Sch_Zone_Radiant_Cooling_Setpoint_%s",
            ifelse(grepl("^Core", rad_meta$zone), "Core", "Perimeter")),
        `Condensation Control Type` = "Off",
        `Condensation Control Dewpoint Offset` = 1,
        `Maximum Cold Water Flow` = "Autosize",
        `Cooling Control Throttling Range` = control_throttling_range
    )]
    idf$load(eplusr::dt_to_load(rad))
    rad_meta[rad, on = c(name = "Name"), `:=`(
        heating_inlet = `Heating Water Inlet Node Name`,
        heating_outlet = `Heating Water Outlet Node Name`,
        cooling_inlet = `Cooling Water Inlet Node Name`,
        cooling_outlet = `Cooling Water Outlet Node Name`
    )]
    # }}}

    # update zone equipment list {{{
    equiplist <- rad_meta[, by = "zone", {
        val <- mapply(function(name, sequence) {
            c("ZoneHVAC:LowTemperatureRadiant:VariableFlow", name, sequence, sequence, NA, NA)
        }, name, seq_len(.N))
        val <- c(sprintf("%s_Equipment", .BY$zone), "SequentialLoad", unlist(val))
        list(name = val[1], class = "ZoneHVAC:EquipmentList", index = seq_along(val), value = val)
    }]
    equiplist[unique(idf$to_table(class = "ZoneHVAC:EquipmentList"), by = "id"),
        on = "name", id := i.id]
    eplusr::without_checking(idf$update(equiplist))
    # }}}

    # update zone equipment connections {{{
    equipconn <- idf$to_table(class = "ZoneHVAC:EquipmentConnections", wide = TRUE)
    equipconn <- equipconn[match(unique(rad_meta$zone), `Zone Name`)]
    equipconn[, `:=`(
        `Zone Conditioning Equipment List Name` = paste(unique(rad_meta$zone), "Equipment", sep = "_")
    )]
    idf$update(eplusr::dt_to_load(equipconn))
    # }}}

    # create one branch for each radiant cooling floor {{{
    branch_clg <- idf$to_table(class = rep("Branch", nrow(rad_meta)), wide = TRUE, init = TRUE)
    branch_htg <- idf$to_table(class = rep("Branch", nrow(rad_meta)), wide = TRUE, init = TRUE)
    branch_clg[, `:=`(
        Name = sprintf("%s_Cooling_Branch", rad_meta$name),
        `Component 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Component 1 Name` = rad_meta$name,
        `Component 1 Inlet Node Name` = rad_meta$cooling_inlet,
        `Component 1 Outlet Node Name` = rad_meta$cooling_outlet
    )]
    branch_htg[, `:=`(
        Name = sprintf("%s_Heating_Branch", rad_meta$name),
        `Component 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Component 1 Name` = rad_meta$name,
        `Component 1 Inlet Node Name` = rad_meta$heating_inlet,
        `Component 1 Outlet Node Name` = rad_meta$heating_outlet
    )]
    idf$load(eplusr::dt_to_load(branch_clg), eplusr::dt_to_load(branch_htg))
    rad_meta[, cooling_branch := sprintf("%s_Cooling_Branch", name)]
    rad_meta[, heating_branch := sprintf("%s_Heating_Branch", name)]
    # }}}

    # add water loops {{{
    if (chilled_water_type == "chiller") {
        create_chilled_water_loop(idf, loop_setpoint = loop_setpoint, rad_meta$cooling_branch)
        create_condensed_water_loop(idf, loop_setpoint = 30, "CndW_Demand_Equipment_Branch")
    # use a cooling tower for radiant system
    } else {
        create_condensed_water_loop(idf, loop_setpoint = loop_setpoint, rad_meta$cooling_branch)
        idf$object("CndW_Loop")$ref_by_object(class = "Sizing:Plant")[[1]]$set(
            loop_type = "Cooling", design_loop_exit_temperature = loop_setpoint
        )
    }

    # in order to make a full HVAC typology, should add heating loop
    create_hot_water_loop(idf, loop_setpoint = 40, rad_meta$heating_branch)
    # }}}

    # add outputs {{{
    idf$load("
        Output:Variable,*,Zone Radiant HVAC Cooling Rate,hourly; !- HVAC Average [W]
        Output:Variable,*,Zone Radiant HVAC Cooling Energy,hourly; !- HVAC Sum [J]
        Output:Variable,*,Zone Radiant HVAC Mass Flow Rate,hourly; !- HVAC Average [kg/s]
        Output:Variable,*,Zone Radiant HVAC Inlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Zone Radiant HVAC Outlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Zone Radiant HVAC Moisture Condensation Time,hourly; !- HVAC Sum [s]
        Output:Variable,*,Zone Radiant HVAC Operation Mode,hourly; !- HVAC Average []

        Output:Variable,*,Chiller Part Load Ratio,hourly; !- HVAC Average []
        Output:Variable,*,Chiller COP,hourly; !- HVAC Average [W/W]
        Output:Variable,*,Chiller Electricity Rate,hourly; !- HVAC Average [W]
        Output:Variable,*,Chiller Evaporator Cooling Rate,hourly; !- HVAC Average [W]
        Output:Variable,*,Chiller Evaporator Inlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Chiller Evaporator Outlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Chiller Evaporator Mass Flow Rate,hourly; !- HVAC Average [kg/s]

        Output:Variable,*,Cooling Tower Inlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Cooling Tower Outlet Temperature,hourly; !- HVAC Average [C]
        Output:Variable,*,Cooling Tower Mass Flow Rate,hourly; !- HVAC Average [kg/s]
        Output:Variable,*,Cooling Tower Heat Transfer Rate,hourly; !- HVAC Average [W]
    ")
    # }}}

    idf
}
# }}}

# create_chilled_water_loop {{{
create_chilled_water_loop <- function(idf, loop_setpoint = 6.7, demand_branches = NULL) {
    checkmate::assert_number(loop_setpoint)
    checkmate::assert_character(demand_branches, any.missing = FALSE, unique = TRUE,
        null.ok = TRUE)

    idf$add(
        # demand inlet pipe
        Pipe_Adiabatic = list("ChW_Demand_Inlet_Pipe",
            "ChW_Demand_Inlet_Node", "ChW_Demand_Inlet_Pipe_Outlet_Node"),
        Branch = list("ChW_Demand_Inlet_Branch", NULL, "Pipe:Adiabatic", "ChW_Demand_Inlet_Pipe",
            "ChW_Demand_Inlet_Node", "ChW_Demand_Inlet_Pipe_Outlet_Node"
        ),

        # demand bypass pipe
        Pipe_Adiabatic = list("ChW_Demand_Bypass_Pipe",
            "ChW_Demand_Bypass_Pipe_Inlet_Node", "ChW_Demand_Bypass_Pipe_Outlet_Node"),
        Branch = list("ChW_Demand_Bypass_Branch", NULL, "Pipe:Adiabatic", "ChW_Demand_Bypass_Pipe",
            "ChW_Demand_Bypass_Pipe_Inlet_Node", "ChW_Demand_Bypass_Pipe_Outlet_Node"
        ),

        # demand outlet pipe
        Pipe_Adiabatic = list("ChW_Demand_Outlet_Pipe",
            "ChW_Demand_Outlet_Pipe_Inlet_Node", "ChW_Demand_Outlet_Node"),
        Branch = list("ChW_Demand_Outlet_Branch", NULL, "Pipe:Adiabatic", "ChW_Demand_Outlet_Pipe",
            "ChW_Demand_Outlet_Pipe_Inlet_Node", "ChW_Demand_Outlet_Node"
        ),

        # branch list
        BranchList = as.list(c("ChW_Demand_Branches",
            "ChW_Demand_Inlet_Branch",
            demand_branches,
            "ChW_Demand_Bypass_Branch",
            "ChW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("ChW_Demand_Splitter",
            "ChW_Demand_Inlet_Branch",
            demand_branches,
            "ChW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("ChW_Demand_Mixer",
            "ChW_Demand_Outlet_Branch",
            demand_branches,
            "ChW_Demand_Bypass_Branch"
        )),
        ConnectorList = list("ChW_Demand_Connectors",
            "Connector:Splitter", "ChW_Demand_Splitter",
            "Connector:Mixer", "ChW_Demand_Mixer"
        ),

        # supply inlet pump
        Pump_VariableSpeed = list("ChW_Pump",
            "ChW_Supply_Inlet_Node", "ChW_Pump_Outlet_Node",
            "Autosize", 3.5E5, "Autosize", 0.9, 0,
            0, 1, 0, 0, 0, "Intermittent"
        ),
        Branch = list("ChW_Supply_Inlet_Branch", NULL, "Pump:VariableSpeed", "ChW_Pump",
            "ChW_Supply_Inlet_Node", "ChW_Pump_Outlet_Node"
        ),

        # chiller
        Curve_Biquadratic = list(
            "WC_Cent_2010_Pb_CapFT",
            0.536234, 0.005581, -0.0013654, 0.0488966, -0.0012018, 0.0010523,
            5, 10, 17.78, 46.11
        ),
        Curve_Biquadratic = list(
            "WC_Cent_2010_Pb_EIRFT",
            1.0821822, 0.0042977, -0.0013182, -0.0260781, 0.0008256, -0.0006013,
            5, 10, 17.78, 46.11
        ),
        Curve_Bicubic = list(
            "WC_Cent_Gt600_2010_Pa_EIRFPLR",
            -0.1360532, 0.008642703, 0.000003855583, 1.024034, 0.06047444, -0.00894786,
            0, 0.05706602, 0, 0,
            31.13, 36.52, 0.19, 1.03
        ),
        Chiller_Electric_ReformulatedEIR = list("SGP_Chiller",
            reference_capacity = "Autosize",
            reference_cop = 6.286,
            reference_leaving_chilled_water_temperature = 6.7,
            reference_leaving_condenser_water_temperature = 32,
            reference_chilled_water_flow_rate = "Autosize",
            reference_condenser_water_flow_rate = "Autosize",
            "WC_Cent_2010_Pb_CapFT", "WC_Cent_2010_Pb_EIRFT",
            "LeavingCondenserWaterTemperature", "WC_Cent_Gt600_2010_Pa_EIRFPLR",
            0.1, 1, 1, 0.1,
            "ChW_Chiller_Inlet_Node", "ChW_Chiller_Outlet_Node",
            "CndW_Chiller_Inlet_Node", "CndW_Chiller_Outlet_Node"
        ),
        Branch = list("ChW_Supply_Equipment_Branch", NULL, "Chiller:Electric:ReformulatedEIR",
            "SGP_Chiller",
            "ChW_Chiller_Inlet_Node", "ChW_Chiller_Outlet_Node"
        ),
        Branch = list("CndW_Demand_Equipment_Branch", NULL, "Chiller:Electric:ReformulatedEIR",
            "SGP_Chiller",
            "CndW_Chiller_Inlet_Node", "CndW_Chiller_Outlet_Node"
        ),

        # supply bypass pipe
        Pipe_Adiabatic = list("ChW_Supply_Bypass_Pipe",
            "ChW_Supply_Bypass_Pipe_Inlet_Node", "ChW_Supply_Bypass_Pipe_Outlet_Node"),
        Branch = list("ChW_Supply_Bypass_Branch", NULL, "Pipe:Adiabatic", "ChW_Supply_Bypass_Pipe",
            "ChW_Supply_Bypass_Pipe_Inlet_Node", "ChW_Supply_Bypass_Pipe_Outlet_Node"
        ),

        # supply outlet pipe
        Pipe_Adiabatic = list("ChW_Supply_Outlet_Pipe",
            "ChW_Supply_Outlet_Pipe_Inlet_Node", "ChW_Supply_Outlet_Node"),
        Branch = list("ChW_Supply_Outlet_Branch", NULL, "Pipe:Adiabatic", "ChW_Supply_Outlet_Pipe",
            "ChW_Supply_Outlet_Pipe_Inlet_Node", "ChW_Supply_Outlet_Node"
        ),

        # branch list
        BranchList = list("ChW_Supply_Branches",
            "ChW_Supply_Inlet_Branch",
            "ChW_Supply_Equipment_Branch", "ChW_Supply_Bypass_Branch",
            "ChW_Supply_Outlet_Branch"
        ),

        # splitter and mixer
        Connector_Splitter = list("ChW_Supply_Splitter",
            "ChW_Supply_Inlet_Branch",
            "ChW_Supply_Equipment_Branch", "ChW_Supply_Bypass_Branch"
        ),
        Connector_Mixer = list("ChW_Supply_Mixer",
            "ChW_Supply_Outlet_Branch",
            "ChW_Supply_Equipment_Branch", "ChW_Supply_Bypass_Branch"
        ),
        ConnectorList = list("ChW_Supply_Connectors",
            "Connector:Splitter", "ChW_Supply_Splitter",
            "Connector:Mixer", "ChW_Supply_Mixer"
        ),

        # plant equipment list
        PlantEquipmentList = list("ChW_Equipment_List", "Chiller:Electric:ReformulatedEIR", "SGP_Chiller"),

        # plant equipment operation
        PlantEquipmentOperation_CoolingLoad = list("ChW_Operation_Scheme", 0, 1E9, "ChW_Equipment_List"),
        PlantEquipmentOperationSchemes = list(
            "ChW_Loop_Operation_Scheme_List",
            "PlantEquipmentOperation:CoolingLoad",
            "ChW_Operation_Scheme", "Sch_Always_On"
        ),

        # plant loop
        PlantLoop = list("ChW_Loop", "Water", NULL,
            "ChW_Loop_Operation_Scheme_List", "ChW_Supply_Outlet_Node",
            98, 1, "Autosize", 0, "Autocalculate",
            "ChW_Supply_Inlet_Node", "ChW_Supply_Outlet_Node",
            "ChW_Supply_Branches", "ChW_Supply_Connectors",
            "ChW_Demand_Inlet_Node", "ChW_Demand_Outlet_Node",
            "ChW_Demand_Branches", "ChW_Demand_Connectors"
        ),

        SetpointManager_Scheduled = list(
            "ChW_Loop_Setpoint_Manager", "Temperature",
            "Sch_ChW_Loop_Temp", "ChW_Supply_Outlet_Node"
        ),

        Sizing_Plant = list("ChW_Loop", "Cooling", loop_setpoint, 5)
    )

    idf$set(Sch_ChW_Loop_Temp = list(field_4 = as.character(loop_setpoint)))

    idf
}
# }}}

# create_condensed_water_loop {{{
create_condensed_water_loop <- function(idf, loop_setpoint = 30, demand_branches = NULL) {
    checkmate::assert_number(loop_setpoint)
    checkmate::assert_character(demand_branches, any.missing = FALSE, unique = TRUE,
        null.ok = TRUE)

    idf$add(
        # demand inlet pipe
        Pipe_Adiabatic = list("CndW_Demand_Inlet_Pipe",
            "CndW_Demand_Inlet_Node", "CndW_Demand_Inlet_Pipe_Outlet_Node"),
        Branch = list("CndW_Demand_Inlet_Branch", NULL, "Pipe:Adiabatic", "CndW_Demand_Inlet_Pipe",
            "CndW_Demand_Inlet_Node", "CndW_Demand_Inlet_Pipe_Outlet_Node"
        ),

        # demand bypass pipe
        Pipe_Adiabatic = list("CndW_Demand_Bypass_Pipe",
            "CndW_Demand_Bypass_Pipe_Inlet_Node", "CndW_Demand_Bypass_Pipe_Outlet_Node"),
        Branch = list("CndW_Demand_Bypass_Branch", NULL, "Pipe:Adiabatic", "CndW_Demand_Bypass_Pipe",
            "CndW_Demand_Bypass_Pipe_Inlet_Node", "CndW_Demand_Bypass_Pipe_Outlet_Node"
        ),

        # demand outlet pipe
        Pipe_Adiabatic = list("CndW_Demand_Outlet_Pipe",
            "CndW_Demand_Outlet_Pipe_Inlet_Node", "CndW_Demand_Outlet_Node"),
        Branch = list("CndW_Demand_Outlet_Branch", NULL, "Pipe:Adiabatic", "CndW_Demand_Outlet_Pipe",
            "CndW_Demand_Outlet_Pipe_Inlet_Node", "CndW_Demand_Outlet_Node"
        ),

        # branch list
        BranchList = as.list(c("CndW_Demand_Branches",
            "CndW_Demand_Inlet_Branch",
            demand_branches,
            "CndW_Demand_Bypass_Branch",
            "CndW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("CndW_Demand_Splitter",
            "CndW_Demand_Inlet_Branch",
            demand_branches,
            "CndW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("CndW_Demand_Mixer",
            "CndW_Demand_Outlet_Branch",
            demand_branches,
            "CndW_Demand_Bypass_Branch"
        )),
        ConnectorList = list("CndW_Demand_Connectors",
            "Connector:Splitter", "CndW_Demand_Splitter",
            "Connector:Mixer", "CndW_Demand_Mixer"
        ),

        # supply inlet pump
        Pump_VariableSpeed = list("CndW_Pump",
            "CndW_Supply_Inlet_Node", "CndW_Pump_Outlet_Node",
            "Autosize", 3.5E5, "Autosize", 0.9, 0,
            0, 1, 0, 0, 0, "Intermittent"
        ),
        Branch = list("CndW_Supply_Inlet_Branch", NULL, "Pump:VariableSpeed", "CndW_Pump",
            "CndW_Supply_Inlet_Node", "CndW_Pump_Outlet_Node"
        ),

        # cooling tower
        CoolingTower_SingleSpeed = list("SGP_CoolingTower",
            "CndW_Tower_Inlet_Node", "CndW_Tower_Outlet_Node",
            design_water_flow_rate = "Autosize",
            design_air_flow_rate = "Autosize",
            design_fan_power = "Autosize",
            design_u_factor_times_area_value = 114900,
            free_convection_regime_air_flow_rate = "Autocalculate",
            free_convection_regime_air_flow_rate_sizing_factor = 0.125,
            free_convection_regime_u_factor_times_area_value = 11480,
            free_convection_u_factor_times_area_value_sizing_factor = 0.1,
            "UFactorTimesAreaAndDesignWaterFlowRate", 1.25,
            NULL, NULL, 0.1, NULL, NULL, NULL, NULL, 0, 2,
            NULL, NULL, 0.2, 0.008, NULL, 3, NULL, NULL, NULL, "FanCycling",
            3, "MinimalCell", 0.33, 2.5, 1
        ),
        Branch = list("CndW_Supply_Equipment_Branch", NULL, "CoolingTower:SingleSpeed",
            "SGP_CoolingTower",
            "CndW_Tower_Inlet_Node", "CndW_Tower_Outlet_Node"
        ),

        # supply bypass pipe
        Pipe_Adiabatic = list("CndW_Supply_Bypass_Pipe",
            "CndW_Supply_Bypass_Pipe_Inlet_Node", "CndW_Supply_Bypass_Pipe_Outlet_Node"),
        Branch = list("CndW_Supply_Bypass_Branch", NULL, "Pipe:Adiabatic", "CndW_Supply_Bypass_Pipe",
            "CndW_Supply_Bypass_Pipe_Inlet_Node", "CndW_Supply_Bypass_Pipe_Outlet_Node"
        ),

        # supply outlet pipe
        Pipe_Adiabatic = list("CndW_Supply_Outlet_Pipe",
            "CndW_Supply_Outlet_Pipe_Inlet_Node", "CndW_Supply_Outlet_Node"),
        Branch = list("CndW_Supply_Outlet_Branch", NULL, "Pipe:Adiabatic", "CndW_Supply_Outlet_Pipe",
            "CndW_Supply_Outlet_Pipe_Inlet_Node", "CndW_Supply_Outlet_Node"
        ),

        # branch list
        BranchList = list("CndW_Supply_Branches",
            "CndW_Supply_Inlet_Branch",
            "CndW_Supply_Equipment_Branch", "CndW_Supply_Bypass_Branch",
            "CndW_Supply_Outlet_Branch"
        ),

        # splitter and mixer
        Connector_Splitter = list("CndW_Supply_Splitter",
            "CndW_Supply_Inlet_Branch",
            "CndW_Supply_Equipment_Branch", "CndW_Supply_Bypass_Branch"
        ),
        Connector_Mixer = list("CndW_Supply_Mixer",
            "CndW_Supply_Outlet_Branch",
            "CndW_Supply_Equipment_Branch", "CndW_Supply_Bypass_Branch"
        ),
        ConnectorList = list("CndW_Supply_Connectors",
            "Connector:Splitter", "CndW_Supply_Splitter",
            "Connector:Mixer", "CndW_Supply_Mixer"
        ),

        # plant equipment list
        CondenserEquipmentList = list("CndW_Equipment_List", "CoolingTower:SingleSpeed", "SGP_CoolingTower"),

        # plant equipment operation
        PlantEquipmentOperation_CoolingLoad = list("CndW_Operation_Scheme", 0, 1E9, "CndW_Equipment_List"),
        CondenserEquipmentOperationSchemes = list("CndW_Loop_Operation_Scheme_List", "PlantEquipmentOperation:CoolingLoad",
            "CndW_Operation_Scheme", "Sch_Always_On"
        ),

        # condenser loop
        CondenserLoop = list("CndW_Loop", "Water", NULL,
            "CndW_Loop_Operation_Scheme_List", "CndW_Supply_Outlet_Node",
            80, 5, "Autosize", 0, "Autocalculate",
            "CndW_Supply_Inlet_Node", "CndW_Supply_Outlet_Node",
            "CndW_Supply_Branches", "CndW_Supply_Connectors",
            "CndW_Demand_Inlet_Node", "CndW_Demand_Outlet_Node",
            "CndW_Demand_Branches", "CndW_Demand_Connectors"
        )
    )

    checkmate::assert_vector(loop_setpoint, any.missing = FALSE, len = 1L)
    if (loop_setpoint > 0) {
        idf$add(
            SetpointManager_Scheduled = list(
                "CndW_Loop_Setpoint_Manager", "Temperature",
                "Sch_CndW_Loop_Temp", "CndW_Supply_Outlet_Node"
            )
        )

        idf$set(Sch_CndW_Loop_Temp = list(field_4 = as.character(loop_setpoint)))

        idf$add(Sizing_Plant = list("CndW_Loop", "Cooling", loop_setpoint, 5))

    } else {
        idf$add(
            # supply outlet setpoint
            SetpointManager_FollowOutdoorAirTemperature = list(
                "CndW_Loop_Setpoint_Manager", "Temperature",
                "OutdoorAirWetBulb", loop_setpoint, 80, 5, "CndW_Supply_Outlet_Node"
            )
        )

        idf$add(Sizing_Plant = list("CndW_Loop", "Condenser", 20, 5))
    }

    idf
}
# }}}

# create_hot_water_loop {{{
create_hot_water_loop <- function(idf, loop_setpoint = 40, demand_branches = NULL) {
    checkmate::assert_number(loop_setpoint)
    checkmate::assert_character(demand_branches, any.missing = FALSE, unique = TRUE,
        null.ok = TRUE)

    idf$add(
        # demand inlet pipe
        Pipe_Adiabatic = list("HW_Demand_Inlet_Pipe", "HW_Demand_Inlet_Node", "HW_Demand_Inlet_Pipe_Outlet_Node"),
        Branch = list("HW_Demand_Inlet_Branch", NULL, "Pipe:Adiabatic",
            "HW_Demand_Inlet_Pipe", "HW_Demand_Inlet_Node", "HW_Demand_Inlet_Pipe_Outlet_Node"
        ),

        # demand bypass pipe
        Pipe_Adiabatic = list("HW_Demand_Bypass_Pipe", "HW_Demand_Bypass_Pipe_Inlet_Node", "HW_Demand_Bypass_Pipe_Outlet_Node"),
        Branch = list("HW_Demand_Bypass_Branch", NULL, "Pipe:Adiabatic",
            "HW_Demand_Bypass_Pipe", "HW_Demand_Bypass_Pipe_Inlet_Node", "HW_Demand_Bypass_Pipe_Outlet_Node"
        ),

        # demand outlet pipe
        Pipe_Adiabatic = list("HW_Demand_Outlet_Pipe", "HW_Demand_Outlet_Pipe_Inlet_Node", "HW_Demand_Outlet_Node"),
        Branch = list("HW_Demand_Outlet_Branch", NULL, "Pipe:Adiabatic",
            "HW_Demand_Outlet_Pipe", "HW_Demand_Outlet_Pipe_Inlet_Node", "HW_Demand_Outlet_Node"
        ),

        # branch list
        BranchList = as.list(c("HW_Demand_Branches",
            "HW_Demand_Inlet_Branch",
            demand_branches,
            "HW_Demand_Bypass_Branch",
            "HW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("HW_Demand_Splitter",
            "HW_Demand_Inlet_Branch",
            demand_branches,
            "HW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("HW_Demand_Mixer",
            "HW_Demand_Outlet_Branch",
            demand_branches,
            "HW_Demand_Bypass_Branch"
        )),
        ConnectorList = list("HW_Demand_Connectors",
            "Connector:Splitter", "HW_Demand_Splitter",
            "Connector:Mixer", "HW_Demand_Mixer"
        ),

        # supply inlet pump
        Pump_VariableSpeed = list("HW_Pump", "HW_Supply_Inlet_Node", "HW_Pump_Outlet_Node",
            "Autosize", 2E5, 30000, 0.9, 0,
            0, 1, 0, 0, 0, "Intermittent"
        ),
        Branch = list("HW_Supply_Inlet_Branch", NULL, "Pump:VariableSpeed",
            "HW_Pump", "HW_Supply_Inlet_Node", "HW_Pump_Outlet_Node"
        ),

        # supply heating equipment
        DistrictHeating = list("Purchased_Heating", "Purchased_Heating_Inlet_Node", "Purchased_Heating_Outlet_Node", 1E6),
        Branch = list("HW_Supply_Equipment_Branch", NULL, "DistrictHeating",
            "Purchased_Heating", "Purchased_Heating_Inlet_Node", "Purchased_Heating_Outlet_Node"
        ),

        # supply bypass pipe
        Pipe_Adiabatic = list("HW_Supply_Bypass_Pipe", "HW_Supply_Bypass_Pipe_Inlet_Node", "HW_Supply_Bypass_Pipe_Outlet_Node"),
        Branch = list("HW_Supply_Bypass_Branch", NULL, "Pipe:Adiabatic",
            "HW_Supply_Bypass_Pipe", "HW_Supply_Bypass_Pipe_Inlet_Node", "HW_Supply_Bypass_Pipe_Outlet_Node"
        ),

        # supply outlet pipe
        Pipe_Adiabatic = list("HW_Supply_Outlet_Pipe", "HW_Supply_Outlet_Pipe_Inlet_Node", "HW_Supply_Outlet_Node"),
        Branch = list("HW_Supply_Outlet_Branch", NULL, "Pipe:Adiabatic",
            "HW_Supply_Outlet_Pipe", "HW_Supply_Outlet_Pipe_Inlet_Node", "HW_Supply_Outlet_Node"
        ),

        # branch list
        BranchList = list("HW_Supply_Branches",
            "HW_Supply_Inlet_Branch",
            "HW_Supply_Equipment_Branch", "HW_Supply_Bypass_Branch",
            "HW_Supply_Outlet_Branch"
        ),

        # splitter and mixer
        Connector_Splitter = list("HW_Supply_Splitter",
            "HW_Supply_Inlet_Branch",
            "HW_Supply_Equipment_Branch", "HW_Supply_Bypass_Branch"
        ),
        Connector_Mixer = list("HW_Supply_Mixer",
            "HW_Supply_Outlet_Branch",
            "HW_Supply_Equipment_Branch", "HW_Supply_Bypass_Branch"
        ),
        ConnectorList = list("HW_Supply_Connectors",
            "Connector:Splitter", "HW_Supply_Splitter",
            "Connector:Mixer", "HW_Supply_Mixer"
        ),

        # plant equipment list
        PlantEquipmentList = list("HW_Equipment_List", "DistrictHeating", "Purchased_Heating"),

        # plant equipment operation
        PlantEquipmentOperation_HeatingLoad = list("HW_Operation_Scheme", 0, 1E6, "HW_Equipment_List"),
        PlantEquipmentOperationSchemes = list("HW_Loop_Operation_Scheme_List", "PlantEquipmentOperation:HeatingLoad",
            "HW_Operation_Scheme", "Sch_Always_On"
        ),

        # plant loop
        PlantLoop = list("HW_Loop", "Water", NULL,
            "HW_Loop_Operation_Scheme_List", "HW_Supply_Outlet_Node",
            100, 10, "Autosize", 0, "Autocalculate",
            "HW_Supply_Inlet_Node", "HW_Supply_Outlet_Node",
            "HW_Supply_Branches", "HW_Supply_Connectors",
            "HW_Demand_Inlet_Node", "HW_Demand_Outlet_Node",
            "HW_Demand_Branches", "HW_Demand_Connectors"
        ),

        # supply outlet setpoint
        Schedule_Compact = list("Sch_HW_Loop_Temp", "Temperature",
            "Through: 12/31", "For: AllDays", "Until: 24:00", as.character(loop_setpoint)
        ),
        SetpointManager_Scheduled = list("HW_Loop_Setpoint_Manager", "Temperature",
            "Sch_HW_Loop_Temp", "HW_Supply_Outlet_Node"
        ),

        # sizing plant
        Sizing_Plant = list("HW_Loop", "Heating", loop_setpoint, 10)
    )

    idf
}
# }}}

# add_natural_ventilation {{{
add_natural_ventilation <- function(idf, ach = 5, max_oa_temp = 30) {
    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    idf$add("ZoneVentilation:DesignFlowRate" := list(
        sprintf("NatVent_%s", zones),
        zones,
        "Sch_ACMV",
        "AirChanges/Hour",
        air_changes_per_hour = ach,
        ventilation_type = "natural",
        maximum_outdoor_temperature = max_oa_temp
    ))

    idf
}
# }}}

# add_doas {{{
add_doas <- function(idf, sat = 20) {
    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    doas_meta <- data.table::data.table(
        zone = zones,
        zone_inlet = sprintf("%s_Air_Inlet_Node", zones),
        zone_equiplist = sprintf("%s_Equipment", zones),
        vavbox = sprintf("%s_DOAS_VAV_Box", zones),
        vavbox_inlet = sprintf("%s_DOAS_VAV_Box_Inlet_Node", zones),
        vavbox_outlet = sprintf("%s_DOAS_VAV_Box_Outlet_Node", zones),
        adu = sprintf("%s_DOAS_ADU", zones),
        level = data.table::fcase(
            grepl("Top", zones), "Top",
            grepl("Mid", zones), "Mid",
            grepl("Bot", zones), "Bot"
        )
    )

    # add air terminal
    idf$add(
        # air terminal sizing
        DesignSpecification_AirTerminal_Sizing = list(
            "DOAS_ADU_Sizing",
            fraction_of_design_cooling_load = 0,
            cooling_design_supply_air_temperature_difference_ratio = 1,
            fraction_of_design_heating_load = 0,
            heating_design_supply_air_temperature_difference_ratio = 1,
            fraction_of_minimum_outdoor_air_flow = 1
        ),
        # vav box
        AirTerminal_SingleDuct_VAV_NoReheat := list(
            name = doas_meta$vavbox,
            air_outlet_node_name = doas_meta$vavbox_outlet,
            air_inlet_node_name = doas_meta$vavbox_inlet,
            maximum_air_flow_rate = "Autosize",
            constant_minimum_air_flow_fraction = 1
        ),
        # air distribution unit
        ZoneHVAC_AirDistributionUnit := list(
            name = doas_meta$adu,
            air_distribution_unit_outlet_node_name = doas_meta$vavbox_outlet,
            air_terminal_object_type = "AirTerminal:SingleDuct:VAV:NoReheat",
            air_terminal_name = doas_meta$vavbox,
            design_specification_air_terminal_sizing_object_name = "DOAS_ADU_Sizing"
        )
    )

    # update zone equipment list
    equiplist <- idf$to_table(class = "ZoneHVAC:EquipmentList")[
        J(doas_meta$zone_equiplist), on = "name"]
    equiplist <- equiplist[doas_meta, on = c("name" = "zone_equiplist"), by = .EACHI, {
        # empty
        if (anyNA(value[c(.N-5, .N-6)])) {
            sq <- (.N - 2) / 6
            value <- c(value[-c((.N-5):.N)],
                "ZoneHVAC:AirDistributionUnit",
                i.adu, sq, sq, NA, NA
            )
        } else {
            sq <- (.N - 2) / 6 + 1
            value <- c(value,
                "ZoneHVAC:AirDistributionUnit",
                i.adu, sq, sq, NA, NA
            )
        }
        list(id = id[1], class = class[1], index = seq_along(value), value = value)
    }]
    eplusr::without_checking(idf$update(equiplist))

    # update zone equipment connection
    equipconn <- idf$to_table(class = "ZoneHVAC:EquipmentConnections", wide = TRUE)[
        J(doas_meta$zone), on = "Zone Name"]
    equipconn[doas_meta, on = c("Zone Name" = "zone"), `:=`(
        "Zone Conditioning Equipment List Name" = i.zone_equiplist,
        "Zone Air Inlet Node or NodeList Name" = i.vavbox_outlet
    )]
    idf$update(eplusr::dt_to_load(equipconn))

    doas_meta_loop <- doas_meta[, by = "level", list(
        fan = sprintf("%s_DOAS_Fan", .BY$level),
        fan_inlet = sprintf("%s_DOAS_Inlet_Node", .BY$level),
        fan_outlet = sprintf("%s_DOAS_Fan_Outlet_Node", .BY$level),
        dx = sprintf("%s_DOAS_DX", .BY$level),
        dxsys = sprintf("%s_DOAS_DX_System", .BY$level),
        dx_inlet = sprintf("%s_DOAS_Fan_Outlet_Node", .BY$level),
        dx_outlet = sprintf("%s_DOAS_DX_Outlet_Node", .BY$level),
        reheat = sprintf("%s_DOAS_Reheat", .BY$level),
        reheat_inlet = sprintf("%s_DOAS_DX_Outlet_Node", .BY$level),
        reheat_outlet = sprintf("%s_DOAS_Reheat_Outlet_Node", .BY$level),
        airloop = sprintf("%s_DOAS", level),
        airloop_supply_inlet = sprintf("%s_DOAS_Inlet_Node", .BY$level),
        airloop_supply_outlet = sprintf("%s_DOAS_Reheat_Outlet_Node", .BY$level),
        airloop_demand_inlet = sprintf("%s_DOAS_Demand_Inlet_Node", .BY$level),
        vavbox_inlet = list(vavbox_inlet)
    )]

    # add equipment
    # add fan
    idf$add(
        Fan_VariableVolume := list(
            doas_meta_loop$fan,
            "Sch_Occupancy",
            fan_total_efficiency = 0.7,
            pressure_rise = 1000,
            maximum_flow_rate = "Autosize",
            fan_power_minimum_flow_rate_input_method = "Fraction",
            motor_efficiency = 0.91,
            motor_in_airstream_fraction = 1,
            fan_power_coefficient_1 = 0.0013,
            fan_power_coefficient_2 = 0.147,
            fan_power_coefficient_3 = 0.9506,
            fan_power_coefficient_4 = 0.0998,
            air_inlet_node_name = doas_meta_loop$fan_inlet,
            air_outlet_node_name = doas_meta_loop$fan_outlet,
            end_use_subcategory = "DOAS Fan"
        )
    )

    # DX coil
    # Ref: Carrier Weathermaster 50HJ006 in EnergyPlus Dataset DXCoolingCoil.idf
    idf$load("
        ! Manufacturer = Carrier, Model Line = Weathermaster (Electric/Electric -- 50HJ006)
        ! Reference point = 95F OAT, 67F EWBT, 2000 cfm
        ! Reference Capacity = 18687 W (5.32 tons)
        ! Compressor Type = Scroll
        ! Refrigerant = R-22
        ! Expansion Valve Type = Thermostatic Expansion Valve
        ! Curve set (5 Curves):
        ! Cooling Capacity Function of Temperature Curve
        ! x = Entering Wet-bulb Temp and y = Outdoor Dry-bulb Temp
        Curve:Biquadratic,
          DOAS_DX_Coil_CapFT,      !- Name
          0.3116765,               !- Coefficient1 Constant
          0.0622847,               !- Coefficient2 x
          -0.0008633,              !- Coefficient3 x**2
          -0.0066556,              !- Coefficient4 y
          -0.0000462,              !- Coefficient5 y**2
          0.0001349,               !- Coefficient6 x*y
          16.67,                   !- Minimum Value of x
          22.22,                   !- Maximum Value of x
          23.89,                   !- Minimum Value of y
          51.67,                   !- Maximum Value of y
          ,                        !- Minimum Curve Output
          ,                        !- Maximum Curve Output
          Temperature,             !- Input Unit Type for X
          Temperature,             !- Input Unit Type for Y
          Dimensionless;           !- Output Unit Type

        ! EIR Function of Temperature Curve
        ! x = Entering Wet-bulb Temp and y = Outdoor Dry-bulb Temp
        Curve:Biquadratic,
          DOAS_DX_Coil_EIRFT,      !- Name
          1.0505234,               !- Coefficient1 Constant
          -0.0653230,              !- Coefficient2 x
          0.0021068,               !- Coefficient3 x**2
          0.0235557,               !- Coefficient4 y
          0.0004542,               !- Coefficient5 y**2
          -0.0014092,              !- Coefficient6 x*y
          16.67,                   !- Minimum Value of x
          22.22,                   !- Maximum Value of x
          23.89,                   !- Minimum Value of y
          51.67,                   !- Maximum Value of y
          ,                        !- Minimum Curve Output
          ,                        !- Maximum Curve Output
          Temperature,             !- Input Unit Type for X
          Temperature,             !- Input Unit Type for Y
          Dimensionless;           !- Output Unit Type

        ! Cooling Capacity Function of Flow Fraction Curve
        ! x = Flow Fraction
        Curve:Quadratic,
          DOAS_DX_Coil_CapFFF,     !- Name
          0.6583072,               !- Coefficient1 Constant
          0.5294956,               !- Coefficient2 x
          -0.1869478,              !- Coefficient3 x**2
          0.75,                    !- Minimum Value of x
          1.25;                    !- Maximum Value of x

        ! EIR Function of Flow Fraction Curve
        ! x = Flow Fraction
        Curve:Quadratic,
          DOAS_DX_Coil_EIRFFF,     !- Name
          1.3293472,               !- Coefficient1 Constant
          -0.5218591,              !- Coefficient2 x
          0.1918220,               !- Coefficient3 x**2
          0.75,                    !- Minimum Value of x
          1.25;                    !- Maximum Value of x

        ! Part Load Fraction Function of Part Load Ratio Curve
        ! (Generic curve used and not derived from manufacturer's data)
        ! x = Part Load Ratio
        Curve:Quadratic,
          DOAS_DX_Coil_PLRFFEIR,   !- Name
          0.85,                    !- Coefficient1 Constant
          0.15,                    !- Coefficient2 x
          0.0,                     !- Coefficient3 x**2
          0.0,                     !- Minimum Value of x
          1.0;                     !- Maximum Value of x
        "
    )
    idf$add(
        # DX coils
        Coil_Cooling_DX_SingleSpeed := list(
            name = doas_meta_loop$dx,
            gross_rated_total_cooling_capacity = "Autosize",
            gross_rated_sensible_heat_ratio = "Autosize",
            gross_rated_cooling_cop = 4.15,
            rated_air_flow_rate = "Autosize",
            air_inlet_node_name = doas_meta_loop$dx_inlet,
            air_outlet_node_name = doas_meta_loop$dx_outlet,
            total_cooling_capacity_function_of_temperature_curve_name = "DOAS_DX_Coil_CapFT",
            total_cooling_capacity_function_of_flow_fraction_curve_name = "DOAS_DX_Coil_CapFFF",
            energy_input_ratio_function_of_temperature_curve_name = "DOAS_DX_Coil_EIRFT",
            energy_input_ratio_function_of_flow_fraction_curve_name = "DOAS_DX_Coil_EIRFFF",
            part_load_fraction_correlation_curve_name = "DOAS_DX_Coil_PLRFFEIR"
        ),
        CoilSystem_Cooling_DX := list(
            name = doas_meta_loop$dxsys,
            dx_cooling_coil_system_inlet_node_name = doas_meta_loop$dx_inlet,
            dx_cooling_coil_system_outlet_node_name = doas_meta_loop$dx_outlet,
            dx_cooling_coil_system_sensor_node_name = doas_meta_loop$dx_outlet,
            cooling_coil_object_type = "Coil:Cooling:DX:SingleSpeed",
            cooling_coil_name = doas_meta_loop$dx,
            dehumidification_control_type = "Multimode",
            run_on_sensible_load = "No",
            run_on_latent_load = "Yes"
        ),
        Schedule_Compact = list(
            "Sch_DOAS_DX_SAT",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(sat - 0.1)
        ),
        SetpointManager_Scheduled := list(
            name = sprintf("%s_DX_SAT_Setpoint_Manager", doas_meta_loop$dxsys),
            control_variable = "Temperature",
            schedule_name = "Sch_DOAS_DX_SAT",
            setpoint_node_or_nodelist_name = doas_meta_loop$dx_outlet
        ),
        Schedule_Compact = list(
            "Sch_DOAS_DX_Hum",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            "0.00924"
        ),
        SetpointManager_Scheduled := list(
            name = sprintf("%s_DX_Hum_Setpoint_Manager", doas_meta_loop$dxsys),
            control_variable = "MaximumHumidityRatio",
            schedule_name = "Sch_DOAS_DX_Hum",
            setpoint_node_or_nodelist_name = doas_meta_loop$dx_outlet
        ),

        # reheat coil
        Coil_Heating_Electric := list(
            name = doas_meta_loop$reheat,
            air_inlet_node_name = doas_meta_loop$reheat_inlet,
            air_outlet_node_name = doas_meta_loop$reheat_outlet,
            temperature_setpoint_node_name = doas_meta_loop$reheat_outlet
        ),
        Schedule_Compact = list(
            "Sch_DOAS_Reheat_SAT",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(sat)
        ),
        SetpointManager_Scheduled := list(
            name = sprintf("%s_REHEAT_Setpoint_Manager", doas_meta_loop$dxsys),
            control_variable = "Temperature",
            schedule_name = "Sch_DOAS_Reheat_SAT",
            setpoint_node_or_nodelist_name = doas_meta_loop$reheat_outlet
        )
    )

    doas_meta_loop[, by = "airloop", `:=`(
        branch = list(c(
            sprintf("%s_Main_Branch", .BY$airloop), "",
            "Fan:VariableVolume", fan, fan_inlet, fan_outlet,
            "CoilSystem:Cooling:DX", dxsys, dx_inlet, dx_outlet,
            "Coil:Heating:Electric", reheat, reheat_inlet, reheat_outlet
        )),
        branchlist = list(c(
            sprintf("%s_Branches", .BY$airloop),
            sprintf("%s_Main_Branch", .BY$airloop)
        )),
        splitter = list(c(
            sprintf("%s_Splitter", .BY$airloop),
            airloop_demand_inlet, vavbox_inlet[[1]]
        )),
        supplypath = list(c(
            sprintf("%s_Supply_Path", .BY$airloop),
            airloop_demand_inlet,
            "AirLoopHVAC:ZoneSplitter", sprintf("%s_Splitter", .BY$airloop)
        ))
    )]

    # add airloop
    lapply(doas_meta_loop$branch, function(...) idf$add(Branch = as.list(...)))
    lapply(doas_meta_loop$branchlist, function(...) idf$add(BranchList = as.list(...)))
    lapply(doas_meta_loop$splitter, function(...) idf$add(AirLoopHVAC_ZoneSplitter = as.list(...)))
    lapply(doas_meta_loop$supplypath, function(...) idf$add(AirLoopHVAC_SupplyPath = as.list(...)))
    lapply(doas_meta_loop$fan_inlet, function(...) idf$add(OutdoorAir_NodeList = as.list(...)))

    idf$add(
        # availability
        AvailabilityManager_Scheduled = list(
            "DOAS_Availability_Manager",
            "Sch_Occupancy"
        ),
        AvailabilityManagerAssignmentList = list(
            "DOAS_Availability_Manager_List",
            "AvailabilityManager:Scheduled",
            "DOAS_Availability_Manager"
        ),
        AirLoopHVAC := list(
            name = doas_meta_loop$airloop,
            availability_manager_list_name = "DOAS_Availability_Manager_List",
            design_supply_air_flow_rate = "Autosize",
            branch_list_name = sprintf("%s_Branches", doas_meta_loop$airloop),
            supply_side_inlet_node_name = doas_meta_loop$airloop_supply_inlet,
            demand_side_inlet_node_names = doas_meta_loop$airloop_demand_inlet,
            supply_side_outlet_node_names = doas_meta_loop$airloop_supply_outlet
        ),
        Sizing_System := list(
            airloop_name = doas_meta_loop$airloop,
            type_of_load_to_size_on = "VentilationRequirement",
            design_outdoor_air_flow_rate = "Autosize",
            central_heating_maximum_system_air_flow_ratio = "Autosize",
            preheat_design_temperature = 2,
            preheat_design_humidity_ratio = 0.008,
            precool_design_temperature = 11,
            precool_design_humidity_ratio = 0.008,
            central_cooling_design_supply_air_temperature = 19,
            central_heating_design_supply_air_temperature = 20
        )
    )

    # update zong sizing
    idf$set(Sizing_Zone := list(
        account_for_dedicated_outdoor_air_system = "Yes",
        dedicated_outdoor_air_system_control_strategy = "ColdSupplyAir",
        dedicated_outdoor_air_low_setpoint_temperature_for_design = sat - 0.5,
        dedicated_outdoor_air_high_setpoint_temperature_for_design = sat
    ))

    idf
}
# }}}

# set_air_velocity {{{
set_air_velocity <- function(idf, velocity = 0.3) {
    sch_velo <- "Sch_Air_Velo"
    idf$object(sch_velo)$set(field_4 = sprintf("%.1f", velocity))

    idf
}
# }}}

# add_ceiling_fan {{{
add_ceiling_fan <- function(idf, watts_per_area = 0.5) {
    if (watts_per_area == 0.0) return(idf)

    z <- grep("Stairs|Toilet|Plenum|Parking", idf$object_name("Zone")$Zone, invert = TRUE, value = TRUE)

    dt <- idf$definition("ElectricEquipment")$to_table(all = TRUE)
    dt[, value := c(NA, NA, "Sch_Occupancy", "Watts/Area", NA, watts_per_area, NA, 0.0, 0.3, 0.0, "Personal Fans")]
    dt <- data.table::rbindlist(replicate(length(z), dt, simplify = FALSE))
    dt[index == 1L, id := .I]
    data.table::setnafill(dt, "locf", cols = "id")

    dt[index == 1L, value := paste(z, "Personal_Fans", sep = "_")]
    dt[index == 2L, value := z]

    eplusr::with_silent(idf$load(dt))

    idf
}
# }}}

# set_coil_setpoint {{{
set_coil_setpoint <- function (idf, setpoint = 12.8) {
    sch_sat <- "Sch_Coil_SAT"
    idf$object(sch_sat)$set(
        sch_sat, "Temperature", "Through: 12/31",
        "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", setpoint)
    )

    idf
}
# }}}

# set_evaporator_setpoint {{{
set_evaporator_setpoint <- function (idf, setpoint = 6.7) {
    sch_chilled <- "Sch_ChW_Loop_Temp"
    idf$object(sch_chilled)$set(
        sch_chilled, "Temperature", "Through: 12/31",
        "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", setpoint)
    )

    idf
}
# }}}

# set_condenser_setpoint {{{
set_condenser_setpoint <- function (idf, setpoint = 29.5) {
    sch_condenser <- "Sch_CndW_Loop_Temp"
    idf$object(sch_condenser)$set(
        sch_condenser, "Temperature", "Through: 12/31",
        "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", setpoint)
    )

    idf
}
# }}}

# read_eui {{{
read_eui <- function(job) {
    checkmate::assert_r6(job)
    checkmate::assert_multi_class(job, c("EplusSql", "EplusJob", "EplusGroupJob"))

    dt <- job$tabular_data(
        report_name = "AnnualBuildingUtilityPerformanceSummary",
        table_name = "Site and Source Energy",
        wide = TRUE
    )[[1L]]
    data.table::set(dt, NULL, c("report_name", "report_for", "table_name"), NULL)
    data.table::set(dt, NULL, "row_name", gsub(" Energy", "", dt[["row_name"]], fixed = TRUE))
    data.table::setnames(dt, "row_name", "type")

    dt <- set_col_units(dt, seq_len(ncol(dt))[-(1:2)], c("kWh", rep("kWh/m2", 2L)))
    tidy_names(dt)

    data.table::setnames(dt, names(dt)[4:5], gsub("_building", "", names(dt)[4:5], fixed = TRUE))

    dt
}
# }}}

# read_building_area {{{
read_building_area <- function(job) {
    checkmate::assert_r6(job)
    checkmate::assert_multi_class(job, c("EplusSql", "EplusJob", "EplusGroupJob"))

    dt <- job$tabular_data(
        report_name = "AnnualBuildingUtilityPerformanceSummary",
        table_name = "Building Area"
    )[, list(
        case,
        type = gsub(" Building Area", "", row_name, fixed = TRUE),
        value = units::set_units(
            units::set_units(as.double(value), units[1L], mode = "standard"),
            "m2", mode = "standard"
        )
    )]
}
# }}}

# read_end_uses {{{
read_end_uses <- function(job, normalize = TRUE) {
    checkmate::assert_r6(job)
    checkmate::assert_multi_class(job, c("EplusSql", "EplusJob", "EplusGroupJob"))

    dt <- job$tabular_data(
        report_name = "AnnualBuildingUtilityPerformanceSummary",
        table_name = "End Uses By Subcategory",
        wide = TRUE
    )[[1L]]
    data.table::set(dt, NULL, c("report_name", "report_for", "table_name"), NULL)
    data.table::setnames(dt, "row_name", "category")

    # Starting from EnergyPlus v9.3, in the SQL Output file, for ReportName =
    # "AnnualBuildingUtilityPerformanceSummary" and ReportName =
    # "DemandEndUseComponentsSummary", the tables TableName = "End Uses by
    # Subcategory" have been refactored. RowName is now in the format <End Use
    # Category>:<End Use Subcategory>.
    if (!"Subcategory" %in% names(dt)) {
        data.table::set(dt, NULL, c("category", "subcategory"),
            data.table::tstrsplit(dt$category, ":", fixed = TRUE)
        )
        data.table::setcolorder(dt, c("case", "category", "subcategory"))
    }

    u <- stringi::stri_extract_first_regex(names(dt)[-c(1:3)], "(?<=\\[).+(?=\\])")
    u[u == "m3"] <- "m^3"
    dt <- set_col_units(dt, seq_len(ncol(dt))[-c(1:3)], u)
    dt <- tidy_names(dt)

    if (!normalize) return(dt)

    # read total building area
    area <- read_building_area(job)[type == "Total"]
    dt[area, on = "case", area := i.value]

    cols <- paste0("`", setdiff(names(dt), c("case", "category", "subcategory")), "`")
    expr <- paste0(sprintf("%s = %s / area", cols, cols), collapse = ", ")
    lang <- parse(text = sprintf("`:=`(%s)", expr))
    dt[, eval(lang)]
    data.table::set(dt, NULL, "area", NULL)

    dt
}
# }}}

# set_col_units {{{
set_col_units <- function(dt, cols, units = NULL) {
    # separate column names and units
    nms <- stringi::stri_match_last_regex(names(dt)[cols], "(.+) \\[(.+)\\]")[, c(2L, 3L)]

    data.table::setnames(dt, names(dt)[cols][!is.na(nms[, 1L])], nms[, 1L][!is.na(nms[, 1L])])

    if (is.null(units)) {
        for (j in seq_along(cols)) {
            if (!is.numeric(dt[[cols[j]]])) next
            data.table::set(dt, NULL, cols[j], units::set_units(dt[[cols[j]]], nms[, 2L][[j]], mode = "standard"))
        }
    } else {
        checkmate::assert_character(units, any.missing = FALSE, all.missing = FALSE, len = length(cols))

        for (j in seq_along(cols)) {
            if (!is.numeric(dt[[cols[j]]])) next
            data.table::set(dt, NULL, cols[j],
                units::set_units(
                    units::set_units(dt[[cols[j]]], nms[, 2L][[j]], mode = "standard"),
                    units[[j]], mode = "standard"
                )
            )
        }
    }

    dt
}
# }}}

# tidy_names {{{
tidy_names <- function(dt) {
    data.table::setnames(dt, gsub("\\s+", "_", tolower(names(dt))))
}
# }}}

# remove_all_vavbox {{{
remove_all_vavbox <- function(idf) {
    checkmate::assert_r6(idf, "Idf")

    terminals <- idf$ZoneHVAC_AirDistributionUnit
    if (!is.null(terminals)) for (term in terminals) remove_airterminal(term)

    idf
}
# }}}

# remove_all_waterloop {{{
remove_all_waterloop <- function(idf) {
    checkmate::assert_r6(idf, "Idf")

    loops <- idf$PlantLoop
    if (!is.null(loops)) for (loop in loops) remove_waterloop(loop)

    loops <- idf$CondenserLoop
    if (!is.null(loops)) for (loop in loops) remove_waterloop(loop)

    idf
}
# }}}

# remove_all_airloop {{{
remove_all_airloop <- function(idf) {
    checkmate::assert_r6(idf, "Idf")

    loops <- idf$AirLoopHVAC
    if (!is.null(loops)) for (loop in loops) remove_airloop(loop)

    idf
}
# }}}

# remove_airterminal {{{
remove_airterminal <- function(airterminal) {
    checkmate::assert_r6(airterminal, "IdfObject")
    # checkmate::assert_choice(airterminal$class_name(), c("ZoneHVAC:AirDistributionUnit"))

    # get parent idf
    idf <- airterminal$parent()

    refs <- airterminal$ref_by_object(depth = 1L, class = "ZoneHVAC:EquipmentConnections")

    for (obj in refs) {
        if (obj$class_name() == "ZoneHVAC:EquipmentConnections") {
            eplusr::without_checking(
                obj$set(
                    zone_conditioning_equipment_list_name = NULL,
                    zone_air_inlet_node_or_nodelist_name = NULL
                )
            )
        } else if (obj$class_name() == "ZoneHVAC:EquipmentList") {
            eplusr::without_checking(
                idf$update(obj$to_table()[index > 2L, value := NA_character_])
            )
        }
    }

    idf$del(airterminal$id(), .ref_to = TRUE)

    idf
}
# }}}

# remove_waterloop {{{
remove_waterloop <- function(waterloop, equip = TRUE) {
    checkmate::assert_r6(waterloop, "IdfObject")
    cls_loop <- checkmate::assert_choice(waterloop$class_name(), c("PlantLoop", "CondenserLoop"))

    # get parent idf
    idf <- waterloop$parent()

    # sizing plant
    sizing <- waterloop$value_relation("Name", "ref_by", class = "Sizing:Plant")$ref_by
    del_if_exist(idf, sizing$object_id)

    prefix <- if (cls_loop == "PlantLoop") "Plant" else "Condenser"

    # setpoint manager for this airloop
    if (length(cls_spm <- get_setpoint_manager_classes(idf))) {
        # all node relations in airloop
        rel_nodes <- waterloop$value_relation(
            c(paste(prefix, "Side Inlet Node Name"),
              paste(prefix, "Side Outlet Node Name"),
              "Demand Side Inlet Node Name",
              "Demand Side Outlet Node Name"),
            "node", class = cls_spm
        )$node
        del_if_exist(idf, rel_nodes$object_id)
    }

    # availability
    if (cls_loop == "PlantLoop") {
        avail <- waterloop$value_relation("Availability Manager List Name", "ref_to", depth = 1L, class_ref = "none")$ref_to
        nm_branlst_demand <- "Demand Side Branch List Name"
        nm_connlst_demand <- "Demand Side Connector List Name"
    } else {
        avail <- sizing[0L]
        nm_branlst_demand <- "Condenser Demand Side Branch List Name"
        nm_connlst_demand <- "Condenser Demand Side Connector List Name"
    }

    # operation scheme
    optsch <- waterloop$value_relation(paste(prefix, "Equipment Operation Scheme Name"), "ref_to", depth = 2L, class_ref = "none")$ref_to

    # branches
    bran_supply <- waterloop$value_relation(paste(prefix, "Side Branch List Name"), "ref_to", depth = 2L, class_ref = "none")$ref_to
    bran_demand <- waterloop$value_relation(nm_branlst_demand, "ref_to", depth = 2L, class_ref = "none")$ref_to

    # connectors
    conn_supply <- waterloop$value_relation(paste(prefix, "Side Connector List Name"), "ref_to", depth = 1L, class_ref = "none")$ref_to
    conn_demand <- waterloop$value_relation(nm_connlst_demand, "ref_to", depth = 1L, class_ref = "none")$ref_to

    # remove water loop
    idf$del(waterloop$id())

    # skip any schedules before removing operation scheme and availability
    cls_sch <- idf$class_name(by_group = TRUE)[["Schedules"]]
    optsch <- optsch[!J(cls_sch), on = "src_class_name"]
    avail <- avail[!J(cls_sch), on = "src_class_name"]

    # remove operation scheme
    del_if_exist(idf, optsch[, c(src_object_id, object_id)])
    # remove availability
    del_if_exist(idf, avail[, c(src_object_id, object_id)])

    # remove connectors
    del_if_exist(idf, conn_supply[, c(src_object_id, object_id)])
    del_if_exist(idf, conn_demand[, c(src_object_id, object_id)])

    # remove any setpoint manager
    for (bran in idf$objects(bran_supply[dep == 2, src_object_id])) {
        remove_setpoint_managers_for_branch(bran)
    }
    for (bran in idf$objects(bran_demand[dep == 2, src_object_id])) {
        remove_setpoint_managers_for_branch(bran)
    }

    # remove branches and branchlists
    del_if_exist(idf, bran_supply[dep < 2, c(src_object_id, object_id)])
    del_if_exist(idf, bran_demand[dep < 2, c(src_object_id, object_id)])

    # if the equipment is also used in a branch/branchlist in another loop,
    # remove it
    for (obj in idf$objects(bran_supply[dep == 2, src_object_id])) {
        remove_equipment_from_loop(obj)
    }
    for (obj in idf$objects(bran_demand[dep == 2, src_object_id])) {
        remove_equipment_from_loop(obj)
    }

    idf
}
# }}}

# remove_airloop {{{
remove_airloop <- function(airloop, oa_system = TRUE) {
    checkmate::assert_r6(airloop, "IdfObject")
    checkmate::assert_string(airloop$class_name(), "AirLoopHVAC")

    # get parent idf
    idf <- airloop$parent()

    # sizing system
    sizing <- airloop$value_relation("Name", "ref_by", class = "Sizing:System")$ref_by
    del_if_exist(idf, sizing$object_id)

    # setpoint manager for this airloop
    if (length(cls_spm <- get_setpoint_manager_classes(idf))) {
        # all node relations in airloop
        rel_nodes <- airloop$value_relation(
            c("Supply Side Inlet Node Name", "Supply Side Outlet Node Names",
              "Demand Side Inlet Node Names", "Demand Side Outlet Node Name"),
            "node", class = cls_spm
        )$node
        del_if_exist(idf, rel_nodes$object_id)
    }

    # availability
    avail <- airloop$value_relation("Availability Manager List Name", "ref_to", depth = 1L, class_ref = "none")$ref_to
    # controllers
    ctrl <- airloop$value_relation("Controller List Name", "ref_to", depth = 1L, class_ref = "none")$ref_to
    # branches
    bran <- airloop$value_relation("Branch List Name", "ref_to", depth = 2L, class_ref = "none")$ref_to
    # connectors
    conn <- airloop$value_relation("Connector List Name", "ref_to", depth = 1L, class_ref = "none")$ref_to
    # supply path
    supply <- airloop$value_relation("Demand Side Inlet Node Names", "node", class = "AirLoopHVAC:SupplyPath")$node
    # return path
    return <- airloop$value_relation("Demand Side Outlet Node Name", "node", class = "AirLoopHVAC:ReturnPath")$node
    # splitter
    if (nrow(supply)) {
        splitter <- idf$object(supply$object_id)$value_relation(NULL, "ref_to",
            class = c("AirLoopHVAC:ZoneSplitter", "AirLoopHVAC:SupplyPlenum"))$ref_to
    } else {
        splitter <- supply[0L]
    }
    # mixer
    if (nrow(return)) {
        mixer <- idf$object(return$object_id)$value_relation(NULL, "ref_to",
            class = c("AirLoopHVAC:ZoneMixer", "AirLoopHVAC:ReturnPlenum"))$ref_to
    } else {
        mixer <- return[0L]
    }

    # remove airloop
    idf$del(airloop$id())

    # skip any schedules
    cls_sch <- idf$class_name(by_group = TRUE)[["Schedules"]]
    avail <- avail[!J(cls_sch), on = "src_class_name"]

    # remove availability
    del_if_exist(idf, avail[, c(src_object_id, object_id)])

    # remove controller
    del_if_exist(idf, ctrl[, c(src_object_id, object_id)])

    # remove connectors
    del_if_exist(idf, conn[, c(src_object_id, object_id)])

    # remove paths
    del_if_exist(idf, supply[, c(src_object_id, object_id)])
    del_if_exist(idf, return[, c(src_object_id, object_id)])
    del_if_exist(idf, splitter[, c(src_object_id, object_id)])
    del_if_exist(idf, mixer[, c(src_object_id, object_id)])

    # handle outdoor air system
    if (!length(id_oa_system <- bran[src_class_name == "AirLoopHVAC:OutdoorAirSystem" & dep == 2L, src_object_id])) {
        # remove branches but keep equipments
        del_if_exist(idf, bran[dep < 2L, c(src_object_id, object_id)])
    } else {
        # remove branches but keep equipments
        del_if_exist(idf, setdiff(bran[dep < 2L, c(src_object_id, object_id)], id_oa_system))

        oa_systems <- idf$objects(id_oa_system)
        for (oa_sys in oa_systems) remove_oa_system(oa_sys)
    }

    # if the equipment is also used in a branch/branchlist in another loop,
    # remove it
    # exclude OA system
    ids <- bran[dep == 2L & src_class_name != "AirLoopHVAC:OutdoorAirSystem", src_object_id]
    for (obj in idf$objects(ids)) {
        remove_equipment_from_loop(obj)
    }

    idf
}
# }}}

# remove_oa_system {{{
remove_oa_system <- function(oa_system) {
    checkmate::assert_r6(oa_system, "IdfObject")
    checkmate::assert_string(oa_system$class_name(), "AirLoopHVAC:OutdoorAirSystem")

    # get parent idf
    idf <- oa_system$parent()

    # controllers
    ctrl <- oa_system$value_relation("Controller List Name", "ref_to", depth = 2L, class_ref = "none")$ref_to
    # branches
    bran <- oa_system$value_relation("Outdoor Air Equipment List Name", "ref_to", depth = 2L, class_ref = "none")$ref_to
    # availability
    avail <- oa_system$value_relation("Availability Manager List Name", "ref_to", depth = 1L, class_ref = "none")$ref_to

    # remove branch
    del_if_exist(idf, bran[, c(src_object_id, object_id)])
    # remove availability
    del_if_exist(idf, avail[, c(src_object_id, object_id)])

    # skip any schedules
    cls_sch <- idf$class_name(by_group = TRUE)[["Schedules"]]
    ctrl <- ctrl[!J(cls_sch), on = "src_class_name"]

    if (nrow(ctrl)) {
        # remove mechanical ventilation if exists
        if (nrow(mv <- ctrl[src_class_name == "Controller:MechanicalVentilation"])) {
            # delete mechanical ventilation if not referred by others
            rel <- idf$object(mv$src_object_id, mv$src_class_name)$value_relation(NULL, "ref_by")$ref_by

            if (length(setdiff(rel$object_id, ctrl$src_object_id))) {
                ctrl <- ctrl[!J(rel$src_object_id), on = "src_object_id"]
            }
        }

        # remove controller itself and skip schedules
        del_if_exist(idf, ctrl[, c(src_object_id, object_id)])
    }

    idf
}
# }}}

# remove_equipment_from_loop {{{
remove_equipment_from_loop <- function(equipment, ref_to = TRUE) {
    checkmate::assert_r6(equipment, "IdfObject")

    idf <- equipment$parent()

    ref_by <- equipment$value_relation(NULL, "ref_by", depth = 1L,
        class_ref = "none",
        class = c("BranchList", "Connector:Splitter", "Connector:Mixer"))$ref_by

    if (!nrow(ref_by)) return(idf$del(equipment$id()))

    # if this equipment is also refered in an airloop, this should be an
    # equipment with both air nodes and water nodes
    # in this case, remove the equipment from the branch
    equipment$value_relation(1L, "ref_by", depth = NULL, class_ref = "none",
        class = "AirLoopHVAC"
    )

    id <- ref_by[dep == 1L, object_id]

    # if current equipment is used in a branch that contains multiple equipment,
    # only remove it from that branch and keep the branch list and connectors
    # untouched
    dt_bran <- idf$to_table(ref_by[dep == 0L & class_name == "Branch", object_id])
    id_bran_mult <- dt_bran[, list(mult = "Component 2 Object Type" %in% field), by = "id"][
        mult == TRUE, id]
    if (length(id_bran_mult)) {
        id <- setdiff(id, ref_by[dep == 1L & src_object_id %in% id_bran_mult, object_id])

        dt_bran_mult <- dt_bran[J(id_bran_mult), on = "id"]
        dt_bran_mult_del_idx <- dt_bran_mult[value == equipment$name(),
            list(index = c(index - 1L, index, index + 1L, index + 2L)), by = "id"]
        dt_bran_mult_pre <- dt_bran_mult[!dt_bran_mult_del_idx, on = c("id", "index")][
            , list(index = seq_len(.N), class, value), by = "id"]
        dt_bran_mult_emp <- dt_bran_mult[dt_bran_mult_del_idx, on = c("id", "index")][
            dt_bran_mult_pre[, by = "id", list(index = max(index))], on = "id",
            index := seq_len(.N) + i.index, by = .EACHI][
            , `:=`(name = NULL, field = NULL, value = NA_character_)]
        dt_bran_mult <- data.table::rbindlist(list(dt_bran_mult_pre, dt_bran_mult_emp), use.names = TRUE)

        idf$update(dt_bran_mult)
    }

    # remove current branch from branch list and connectors if the branch
    # contains only the equipment and nothing else
    if (length(id)) {
        # get branch name
        bran <- ref_by[dep == 0L & class_name == "Branch", object_name]

        dt <- idf$to_table(id)

        dt_pre <- dt[!J(bran), on = "value"][, by = c("id", "class"), list(value, index = seq_len(.N))]
        dt_emp <- dt[J(bran), on = "value"][, value := NA_character_]
        dt_emp[dt_pre[, by = "id", list(index = max(index))], on = "id", index := seq_len(.N) + i.index, by = .EACHI]
        dt_emp[, `:=`(name = NULL, field = NULL)]
        dt <- data.table::rbindlist(list(dt_pre, dt_emp), use.names = TRUE)

        idf$update(dt)
    }

    del_if_exist(idf, c(equipment$id(), setdiff(dt_bran$id, id_bran_mult)), ref_to = ref_to)

    idf
}
# }}}

# remove_setpoint_managers_for_branch {{{
remove_setpoint_managers_for_branch <- function(branch, class = NULL) {
    idf <- branch$parent()

    if (is.null(class)) class <- get_setpoint_manager_classes(idf)

    rel <- branch$value_relation(NULL, "node", class = class)$node

    if (!nrow(rel)) return(idf)

    del_if_exist(idf, rel[class_name %in% class, object_id])
}
# }}}

# get_setpoint_manager_classes {{{
get_setpoint_manager_classes <- function(idf) {
    idf$class_name(by_group = TRUE)[["Setpoint Managers"]]
}
# }}}

# del_if_exist {{{
del_if_exist <- function(idf, id, ref_to = FALSE) {
    if (!length(id)) return(idf)
    id <- unique(id[idf$is_valid_id(id)])
    if (!length(id)) return(idf)
    idf$del(id, .ref_to = ref_to)
}
# }}}
