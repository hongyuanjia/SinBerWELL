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

# add_radiant_floor {{{
add_radiant_floor <- function(idf, core, perimeter) {
    checkmate::assert_double(core, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)
    checkmate::assert_double(perimeter, lower = 16, upper = 30, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)

    # add control temperature schedules
    nm_sch <- idf$object_name("Schedule:Compact")[[1]]
    if ("Sch_Zone_RadiantHeating_Setpoint_Core" %in% nm_sch) {
        idf$set("Sch_Zone_RadiantHeating_Setpoint_Core" = list(
            field_4 = as.character(core[1])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_RadiantHeating_Setpoint_Core",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(core[1])
        ))
    }
    if ("Sch_Zone_RadiantHeating_Setpoint_Perimeter" %in% nm_sch) {
        idf$set("Sch_Zone_RadiantHeating_Setpoint_Perimeter" = list(
            field_4 = as.character(perimeter[1])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_RadiantHeating_Setpoint_Perimeter",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(perimeter[1])
        ))
    }
    if ("Sch_Zone_RaidantCooling_Setpoint_Core" %in% nm_sch) {
        idf$set("Sch_Zone_RadiantCooling_Setpoint_Core" = list(
            field_4 = as.character(core[2])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_RadiantCooling_Setpoint_Core",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(core[2])
        ))
    }
    if ("Sch_Zone_RadiantCooling_Setpoint_Perimeter" %in% nm_sch) {
        idf$set("Sch_Zone_RadiantCooling_Setpoint_Perimeter" = list(
            field_4 = as.character(perimeter[2])
        ))
    } else {
        idf$add(Schedule_Compact = list(
            "Sch_Zone_RadiantCooling_Setpoint_Perimeter",
            "Temperature", "Through: 12/31", "For: AllDays", "Until: 24:00",
            as.character(perimeter[2])
        ))
    }

    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    # get floors
    floors <- idf$to_table(class = "BuildingSurface:Detailed", wide = TRUE)[
        `Surface Type` == "Floor" & `Zone Name` %in% zones,
        .(id, name, zone = `Zone Name`)]

    # get surface areas in order to calculate flow fraction for each surface
    floors[, area := idf$geometry()$area(object = floors$name)$area]
    floors[, fraction := round(area / sum(area), 3), by = "zone"]
    floors[, area := NULL]

    # update radiant floor construction
    val <- idf$object("SGP_Floor")$value()
    val$Name <- "SGP_Floor_Radiant"
    val$`Source Present After Layer Number` <- 1
    val$`Temperature Calculation Requested After Layer Number` <- 1
    val$`Dimensions for The CTF Calculation` <- 1
    val$`Tube Spacing` <- 0.1524
    idf$add(Construction_InternalSource = val)
    idf$set(.(floors$id) := list(Construction_Name = "SGP_Floor_Radiant"))

    # group surfaces by zone to create radiant surface groups
    rad_flrs <- floors[, by = "zone", {
        val <- mapply(function(surf, frac) c(surf, frac), name, fraction, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        val <- c(sprintf("%s_Radiant_Floor", .BY$zone), unlist(val))
        list(class = "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup", index = seq_along(val), value = val)
    }]
    rad_flrs[, `:=`(id = data.table::rleid(zone))]
    idf$load(rad_flrs)

    # create low temperature radiant
    rad <- idf$to_table(class = rep("ZoneHVAC:LowTemperatureRadiant:VariableFlow", length(zones)),
        init = TRUE, wide = TRUE)
    rad[, `:=`(
        Name = sprintf("%s_Radiant", zones),
        `Availability Schedule Name` = "Sch_Always_On",
        # `Availability Schedule Name` = "Sch_ACMV",
        `Zone Name` = zones,
        `Surface Name or Radiant Surface Group Name` = sprintf("%s_Radiant_Floor", zones),
        `Heating Design Capacity` = "Autosize",
        `Heating Water Inlet Node Name` = sprintf("%s_RadiantHeating_Water_Inlet_Node", zones),
        `Heating Water Outlet Node Name` = sprintf("%s_RadiantHeating_Water_Outlet_Node", zones),
        `Heating Control Temperature Schedule Name` = sprintf("Sch_Zone_RadiantHeating_Setpoint_%s", ifelse(grepl("^Core", zones), "Core", "Perimeter")),
        `Maximum Hot Water Flow` = "Autosize",
        `Cooling Design Capacity` = "Autosize",
        `Cooling Water Inlet Node Name` = sprintf("%s_RadiantCooling_Water_Inlet_Node", zones),
        `Cooling Water Outlet Node Name` = sprintf("%s_RadiantCooling_Water_Outlet_Node", zones),
        `Cooling Control Temperature Schedule Name` = sprintf("Sch_Zone_RadiantCooling_Setpoint_%s", ifelse(grepl("^Core", zones), "Core", "Perimeter")),
        `Condensation Control Type` = "Off",
        `Condensation Control Dewpoint Offset` = 1,
        `Maximum Cold Water Flow` = "Autosize"
    )]
    idf$load(eplusr::dt_to_load(rad))

    # update zone equipment list
    equiplist <- idf$to_table(class = "ZoneHVAC:EquipmentList", wide = TRUE)
    equiplist <- equiplist[match(zones, gsub("_Equipment", "", name))]
    # add radiant floor to the first cooling option
    equiplist[, `:=`(
        `Zone Equipment 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Zone Equipment 1 Name` = rad$Name,
        `Zone Equipment 1 Cooling Sequence` = 1,
        `Zone Equipment 1 Heating or No-Load Sequence` = 1,
        `Zone Equipment 1 Sequential Cooling Fraction Schedule Name` = paste0(zones, "_Equipment CoolingFrac1"),
        `Zone Equipment 1 Sequential Heating Fraction Schedule Name` = paste0(zones, "_Equipment HeatingFrac1")
    )]
    idf$update(eplusr::dt_to_load(equiplist))

    # create one branch for each radiant cooling floor {{{
    branch_clg <- idf$to_table(class = rep("Branch", length(zones)), wide = TRUE, init = TRUE)
    branch_htg <- idf$to_table(class = rep("Branch", length(zones)), wide = TRUE, init = TRUE)
    branch_clg[, `:=`(
        Name = sprintf("%s_RadiantCooling_Branch", zones),
        `Component 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Component 1 Name` = sprintf("%s_Radiant", zones),
        `Component 1 Inlet Node Name` = sprintf("%s_RadiantCooling_Water_Inlet_Node", zones),
        `Component 1 Outlet Node Name` = sprintf("%s_RadiantCooling_Water_Outlet_Node", zones)
    )]
    branch_htg[, `:=`(
        Name = sprintf("%s_RadiantHeating_Branch", zones),
        `Component 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Component 1 Name` = sprintf("%s_Radiant", zones),
        `Component 1 Inlet Node Name` = sprintf("%s_RadiantHeating_Water_Inlet_Node", zones),
        `Component 1 Outlet Node Name` = sprintf("%s_RadiantHeating_Water_Outlet_Node", zones)
    )]
    idf$load(eplusr::dt_to_load(branch_clg), eplusr::dt_to_load(branch_htg))
    # }}}

    # insert radiant cooling branches into the condenser demand branch list and
    # use cooling tower for radiant system cooling {{{
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
            branch_clg$Name,
            "CndW_Demand_Bypass_Branch",
            "CndW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("CndW_Demand_Splitter",
            "CndW_Demand_Inlet_Branch",
            branch_clg$Name,
            "CndW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("CndW_Demand_Mixer",
            "CndW_Demand_Outlet_Branch",
            branch_clg$Name,
            "CndW_Demand_Bypass_Branch"
        )),
        ConnectorList = list("CndW_Demand_Connectors",
            "Connector:Splitter", "CndW_Demand_Splitter",
            "Connector:Mixer", "CndW_Demand_Mixer"
        ),

        # supply inlet pump
        Pump_VariableSpeed = list("CndW_Pump",
            "CndW_Supply_Inlet_Node", "CndW_Pump_Outlet_Node",
            0.06, 3.5E5, 30000, 0.9, 0,
            0, 1, 0, 0, 0, "Intermittent"
        ),
        Branch = list("CndW_Supply_Inlet_Branch", NULL, "Pump:VariableSpeed", "CndW_Pump",
            "CndW_Supply_Inlet_Node", "CndW_Pump_Outlet_Node"
        ),

        # supply cooling equipment: cooling tower
        CoolingTower_SingleSpeed = list("SGP_CoolingTower_Radiant",
            "CndW_Tower_Radiant_Inlet_Node", "CndW_Tower_Radiant_Outlet_Node",
            design_water_flow_rate = 0.08,
            design_air_flow_rate = 52.0,
            design_fan_power = 19600,
            design_u_factor_times_area_value = 114900,
            free_convection_regime_air_flow_rate = 6.46,
            free_convection_regime_air_flow_rate_sizing_factor = 0.125,
            free_convection_regime_u_factor_times_area_value = 11480,
            free_convection_u_factor_times_area_value_sizing_factor = 0.1,
            "UFactorTimesAreaAndDesignWaterFlowRate", 1.25,
            NULL, NULL, 0.1, NULL, NULL, NULL, NULL, 0, 2,
            NULL, NULL, 0.2, 0.008, NULL, 3, NULL, NULL, NULL, "FanCycling",
            3, "MinimalCell", 0.33, 2.5, 1
        ),
        Branch = list("CndW_Supply_Equipment_Branch", NULL, "CoolingTower:SingleSpeed",
            "SGP_CoolingTower_Radiant",
            "CndW_Tower_Radiant_Inlet_Node", "CndW_Tower_Radiant_Outlet_Node"
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
        CondenserEquipmentList = list("CndW_Equipment_List", "CoolingTower:SingleSpeed", "SGP_CoolingTower_Radiant"),

        # plant equipment operation
        PlantEquipmentOperation_CoolingLoad = list("CndW_Operation_Scheme", 0, 1E9, "CndW_Equipment_List"),
        CondenserEquipmentOperationSchemes = list("CndW_Loop_Operation_Scheme_List", "PlantEquipmentOperation:CoolingLoad",
            "CndW_Operation_Scheme", "Sch_Always_On"
        ),

        # plant loop
        CondenserLoop = list("CndW_Radiant_Loop", "Water", NULL,
            "CndW_Loop_Operation_Scheme_List", "CndW_Supply_Outlet_Node",
            100, 10, 0.06, 0, "Autocalculate",
            "CndW_Supply_Inlet_Node", "CndW_Supply_Outlet_Node",
            "CndW_Supply_Branches", "CndW_Supply_Connectors",
            "CndW_Demand_Inlet_Node", "CndW_Demand_Outlet_Node",
            "CndW_Demand_Branches", "CndW_Demand_Connectors"
        ),

        # supply outlet setpoint
        # SetpointManager_FollowOutdoorAirTemperature = list(
        #     "CndW_Loop_Setpoint_Manager", "Temperature",
        #     "OutdoorAirWetBulb", -7, 80, 10, "CndW_Supply_Outlet_Node"
        # ),
        SetpointManager_Scheduled = list(
            "CndW_Loop_Setpoint_Manager", "Temperature",
            "Sch_CndW_Loop_Temp", "CndW_Supply_Outlet_Node"
        ),

        Sizing_Plant = list("CndW_Radiant_Loop", "Cooling", 6.7, 5)
    )
    # }}}

    # change the cooling tower supply temperature to 20
    idf$set(Sch_CndW_Loop_Temp = list(field_4 = "20"))

    # in order to make a full HVAC typology, should add heating loop {{{
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
            branch_htg$Name,
            "HW_Demand_Bypass_Branch",
            "HW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("HW_Demand_Splitter",
            "HW_Demand_Inlet_Branch",
            branch_htg$Name,
            "HW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("HW_Demand_Mixer",
            "HW_Demand_Outlet_Branch",
            branch_htg$Name,
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
            "Through: 12/31", "For: AllDays", "Until: 24:00", "30"
        ),
        SetpointManager_Scheduled = list("HW_Loop_Setpoint_Manager", "Temperature",
            "Sch_HW_Loop_Temp", "HW_Supply_Outlet_Node"
        ),

        # sizing plant
        Sizing_Plant = list("HW_Loop", "Heating", 60, 10)
    )
    # }}}

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
        "Sch_Occupancy",
        "AirChanges/Hour",
        air_changes_per_hour = ach,
        ventilation_type = "natural",
        maximum_outdoor_temperature = max_oa_temp
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
set_coil_setpoint <- function (idf, setpoint = 12.8, ddy_setpoint = setpoint) {
    sch_sat <- "Sch_Coil_SAT"
    idf$object(sch_sat)$set(
        sch_sat, "Temperature", "Through: 12/31",
        "For: SummerDesignDay", "Until: 24:00", sprintf("%.1f", ddy_setpoint),
        "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", setpoint)
    )

    idf
}
# }}}

# set_evaporator_setpoint {{{
set_evaporator_setpoint <- function (idf, setpoint = 6.7, ddy_setpoint = setpoint) {
    sch_chilled <- "Sch_ChW_Loop_Temp"
    idf$object(sch_chilled)$set(
        sch_chilled, "Temperature", "Through: 12/31",
        "For: SummerDesignDay", "Until: 24:00", sprintf("%.1f", ddy_setpoint),
        "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", setpoint)
    )

    idf
}
# }}}

# set_condenser_setpoint {{{
set_condenser_setpoint <- function (idf, setpoint = 29.5, ddy_setpoint = 29.5) {
    sch_condenser <- "Sch_CndW_Loop_Temp"
    idf$object(sch_condenser)$set(
        sch_condenser, "Temperature", "Through: 12/31",
        "For: SummerDesignDay", "Until: 24:00", sprintf("%.1f", ddy_setpoint),
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
