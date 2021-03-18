# set_thermostat_singlcooling {{{
set_thermostat_singlecooling <- function(idf, core, perimeter) {
    checkmate::assert_number(core, lower = 16, upper = 30)
    checkmate::assert_number(perimeter, lower = 16, upper = 30)

    idf$set("Sch_Zone_Thermostat_Control_Type" = list(field_4 = "2"))

    # get conditioned zones
    zones <- idf$to_table(class = "ZoneControl:Thermostat")[
        field == "Zone or ZoneList Name", value]

    without_checking({
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

        idf$load(dt_to_load(sgl))
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

    without_checking({
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

        idf$load(dt_to_load(dual))
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
        `Availability Schedule Name` = "Sch_ACMV",
        `Zone Name` = zones,
        `Surface Name or Radiant Surface Group Name` = sprintf("%s_Radiant_Floor", zones),
        `Heating Design Capacity` = "Autosize",
        `Heating Water Inlet Node Name` = sprintf("%s_RadiantHeating_Water_Inlet_Node", zones),
        `Heating Water Outlet Node Name` = sprintf("%s_RadiantHeating_Water_Outlet_Node", zones),
        `Heating Control Temperature Schedule Name` = sprintf("Sch_Zone_RadiantHeating_Setpoint_%s", ifelse(grepl("^Core", zones), "Core", "Perimeter")),
        `Cooling Design Capacity` = "Autosize",
        `Cooling Water Inlet Node Name` = sprintf("%s_RadiantCooling_Water_Inlet_Node", zones),
        `Cooling Water Outlet Node Name` = sprintf("%s_RadiantCooling_Water_Outlet_Node", zones),
        `Cooling Control Temperature Schedule Name` = sprintf("Sch_Zone_RadiantCooling_Setpoint_%s", ifelse(grepl("^Core", zones), "Core", "Perimeter")),
        `Condensation Control Type` = "VariableOff",
        `Condensation Control Dewpoint Offset` = 1
    )]
    idf$load(dt_to_load(rad))

    # update zone equipment list
    equiplist <- idf$to_table(class = "ZoneHVAC:EquipmentList", wide = TRUE, all = TRUE)[, .SD, .SDcols = 1:17]
    equiplist <- equiplist[match(zones, gsub("_Equipment", "", name))]
    # move the vav box to the second cooling option
    equiplist[, `:=`(
        `Zone Equipment 2 Object Type` = `Zone Equipment 1 Object Type`,
        `Zone Equipment 2 Name` = `Zone Equipment 1 Name`,
        `Zone Equipment 2 Cooling Sequence` = 2,
        `Zone Equipment 2 Heating or No-Load Sequence` = 2,
        `Zone Equipment 2 Sequential Cooling Fraction Schedule Name` = `Zone Equipment 1 Sequential Cooling Fraction Schedule Name`,
        `Zone Equipment 2 Sequential Heating Fraction Schedule Name` = `Zone Equipment 1 Sequential Heating Fraction Schedule Name`
    )]
    # add radiant floor to the first cooling option
    equiplist[, `:=`(
        `Zone Equipment 1 Object Type` = "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        `Zone Equipment 1 Name` = rad$Name,
        `Zone Equipment 1 Cooling Sequence` = 1,
        `Zone Equipment 1 Heating or No-Load Sequence` = 1
    )]
    idf$update(dt_to_load(equiplist))

    # create one branch for each radiant cooling floor
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
    idf$load(dt_to_load(branch_clg), dt_to_load(branch_htg))

    idf$object("CndW_Demand_Mixer")
    idf$object("CndW_Demand_Splitter")

    # insert radiant cooling branches into the condenser demand branch list
    idf$set(
        CndW_Demand_Branches = as.list(c(
            "CndW_Demand_Branches",
            "CndW_Demand_Inlet_Branch",
            branch_clg$Name, "CndW_Demand_Load_Branch_1",
            "CndW_Demand_Bypass_Branch",
            "CndW_Demand_Outlet_Branch"
        )),
        CndW_Demand_Mixer = as.list(c(
            "CndW_Demand_Mixer",
            "CndW_Demand_Outlet_Branch",
            branch_clg$Name,
            "CndW_Demand_Load_Branch_1",
            "CndW_Demand_Bypass_Branch"
        )),
        CndW_Demand_Splitter = as.list(c(
            "CndW_Demand_Splitter",
            "CndW_Demand_Inlet_Branch",
            branch_clg$Name, "CndW_Demand_Load_Branch_1",
            "CndW_Demand_Bypass_Branch"
        ))
    )

    # in order to make a full HVAC typology, should add heating loop
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
            sprintf("%s_RadiantHeating_Branch", zones),
            "HW_Demand_Bypass_Branch",
            "HW_Demand_Outlet_Branch"
        )),

        # splitter and mixer
        Connector_Splitter = as.list(c("HW_Demand_Splitter",
            "HW_Demand_Inlet_Branch",
            sprintf("%s_RadiantHeating_Branch", zones),
            "HW_Demand_Bypass_Branch"
        )),
        Connector_Mixer = as.list(c("HW_Demand_Mixer",
            "HW_Demand_Outlet_Branch",
            sprintf("%s_RadiantHeating_Branch", zones),
            "HW_Demand_Bypass_Branch"
        )),
        ConnectorList = list("HW_Demand_Connectors",
            "Connector:Splitter", "HW_Demand_Splitter",
            "Connector:Mixer", "HW_Demand_Mixer"
        ),

        # supply inlet pump
        Pump_VariableSpeed = list("HW_Pump", "HW_Supply_Inlet_Node", "HW_Pump_Outlet_Node",
            "Autosize", 2E5, 30000, 0.9, 0, pump_control_type = "Intermittent"
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
        Sizing_Plant = list("HW_Loop", "Heating", 30, 5)
    )

    idf
}
# }}}

# turn_off_air_system {{{
turn_off_air_system <- function(idf) {
    avail <- sprintf("VAV_%s_Availability", c("Bot", "Mid", "Top"))
    avail_list <- paste(avail, "List", sep = "_")

    # remove existing
    valid <- idf$is_valid_name(c(avail, avail_list))
    if (any(valid)) idf$del(c(avail, avail_list)[valid], .force = TRUE)

    if (!idf$is_valid_name("Sch_Always_Off")) {
        idf$add(Schedule_Compact = list(
            "Sch_Always_Off",
            "On/Off", "Through: 12/31", "For: AllDays",
            "Until: 24:00", "0"
        ))
    }

    idf$add(
        AvailabilityManager_Scheduled := list(
            sprintf("VAV_%s_Availability", c("Bot", "Mid", "Top")),
            "Sch_Always_Off"
        ),
        AvailabilityManagerAssignmentList := list(
            sprintf("VAV_%s_Availability_List", c("Bot", "Mid", "Top")),
            "AvailabilityManager:Scheduled",
            sprintf("VAV_%s_Availability", c("Bot", "Mid", "Top"))
        )
    )

    idf$set(c(idf$object_id(class = "AirLoopHVAC")[[1]]) := list(
        `Availability Manager List Name` = avail_list
    ))

    idf
}
# }}}

# add_natural_ventilation {{{
add_natural_ventilation <- function(idf, ach = 5, max_oa_temp = 30) {
    idf$add(Schedule_Constant = list("HybridVentType", "Any Number", 1))
    idf$add(Schedule_Constant = list("Always 0", "Any Number", 0))

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
