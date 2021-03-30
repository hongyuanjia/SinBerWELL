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

# get_conditioned_zones {{{
get_conditioned_zones <- function(idf) {
    checkmate::assert_r6(idf, "Idf")

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    cls_thermo <- "ZoneControl:Thermostat"
    if (!idf$is_valid_class(cls_thermo)) {
        stop(sprintf("No '%s' objects were found which are needed when getting conditioned zones.", cls_thermo))
    }

    val_thermo <- eplusr::get_idf_value(idd_env, idf_env, class = cls_thermo, field = "Zone or ZoneList Name")

    # expand zone list
    rel_thermo <- eplusr::get_idf_relation(idd_env, idf_env, NULL, val_thermo$value_id,
        "ref_to", depth = 1L, class = "Zone")

    ids <- unique(rel_thermo$src_object_id)
    if (idf$is_valid_class("ZoneList")) {
        ids <- setdiff(ids, idf$object_id("ZoneList")$ZoneList)
    }

    stats::setNames(ids, idf_env$object[J(ids), on = "object_id", object_name])
}
# }}}

# set_vav_system_load_ratio {{{
set_vav_system_load_ratio <- function(idf) {
    checkmate::assert_r6(idf, "Idf")

    # using the sizing information to get a proper ratio of cooling loads
    # that the VAV system should serve
    job <- idf$run(NULL, tempdir(), echo = FALSE)
    sizes <- job$read_table("ComponentSizes")

    # get radiant cooling capacity
    sizes_rad <- sizes[
        comp_type == "ZoneHVAC:LowTemperatureRadiant:VariableFlow" &
        description == "Design Size Cooling Design Capacity"
    ][, value := as.numeric(value)]

    # get radiant cooling loads per floor
    sizes_rad[, floor := gsub(".+(BOT|MID|TOP).+", "\\1", comp_name)]
    sizes_rad_per_floor <- sizes_rad[, .(value = sum(value)), by = "floor"]
    sizes_rad_per_floor[, coil := sprintf("VAV_%s_COIL", floor)]

    # get the coil sized capacity
    sizes_coil <- job$tabular_data(report_name = "HVACSizingSummary",
        table_name = "Coil Sizing Summary", wide = TRUE, string_value = FALSE)[[1]]

    # get the sizing ratio for coils
    ratio_coil <- sizes_coil[sizes_rad_per_floor, on = c("row_name" = "coil"),
        round(1 - (i.value / `Coil Final Gross Total Capacity [W]`), 3)]

    # change VAV system sizing method and apply the cooling load ratios
    idf$set(
        Sizing_System := list(
            "Cooling Design Capacity Method" = "FractionOfAutosizedCoolingCapacity",
            "Fraction of Autosized Cooling Design Capacity" = ratio_coil
        )
    )
}
# }}}

# get_zone_equipment_connections {{{
get_zone_equipment_connections <- function(idf, ids) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    obj_zone <- eplusr::get_idf_object(idd_env, idf_env, "Zone", ids)

    # get zone equipment connection
    rel_conn <- eplusr::get_idf_relation(idd_env, idf_env, obj_zone$object_id, NULL,
        "ref_by", class_ref = "none", class = "ZoneHVAC:EquipmentConnections")
    rel_conn <- rel_conn[J(obj_zone$object_id), on = "src_object_id"]

    # create equipment connection if not exist
    if (anyNA(rel_conn$object_id)) {
        mis <- rel_conn[is.na(object_id)]

        # get zone names without equipment connection
        nm_zone <- obj_zone[J(mis$src_object_id), on = "object_id", object_name]

        # create equipment connection
        nm_node <- new_node_name(idf, sprintf("%s_Air_Node", nm_zone))
        obj_conn <- eplusr::without_checking(idf$add("ZoneHVAC:EquipmentConnections" := list(
            zone_name = nm_zone, zone_air_node_name = nm_node
        ), .all = TRUE))
        equipconn <- get_named_id(obj_conn)
        rel_conn[is.na(object_id), object_id := equipconn]
    }

    # get zone equipment list
    rel_list <- eplusr::get_idf_relation(idd_env, idf_env, rel_conn$object_id, NULL,
        "ref_to", class_ref = "none", class = "ZoneHVAC:EquipmentList")
    rel_list <- rel_list[J(rel_conn$object_id), on = "object_id"]

    # create equipment list if not exist
    if (anyNA(rel_list$src_object_id)) {
        mis <- rel_conn[J(rel_list[is.na(src_object_id), object_id]), on = "object_id"]

        # get zone names without equipment list
        nm_zone <- obj_zone[J(mis$src_object_id), on = "object_id", object_name]

        # create equipment list
        nm_list <- new_object_name(idf, "ZoneHVAC:EquipmentList", sprintf("%s_Equipment_List", nm_zone))
        obj_list <- eplusr::without_checking(idf$add("ZoneHVAC:EquipmentList" := list(name = nm_list)))
        equiplist <- get_named_id(obj_list)
        rel_list[is.na(src_object_id), src_object_id := equiplist]

        # assign to zone equipment connections
        obj <- idf$set(c(mis$object_id) := list(zone_conditioning_equipment_list_name = names(equiplist)))
    }

    data.table::setnames(rel_conn, c("object_id", "src_object_id"), c("id_conn", "id_zone"))
    data.table::setnames(rel_list, c("object_id", "src_object_id"), c("id_conn", "id_list"))
    data.table::set(rel_list, NULL, setdiff(names(rel_list), c("id_conn", "id_list")), NULL)

    rel <- rel_list[rel_conn, on = "id_conn", id_zone := i.id_zone][J(ids), on = "id_zone"]
    rel[idf_env$object, on = c("id_zone" = "object_id"), name_zone := i.object_name]
    rel[idf_env$object, on = c("id_list" = "object_id"), name_list := i.object_name]

    data.table::setcolorder(rel, c("id_zone", "name_zone", "id_list", "name_list", "id_conn"))[]
}
# }}}

# new_radiant_cooling {{{
new_radiant_cooling <- function(idf, ids_zone, id_const, type = "floor",
                                setpoint = c(20, 28), throttling_range = 2) {
    checkmate::assert_integer(ids_zone, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
    checkmate::assert_int(id_const)
    checkmate::assert_double(setpoint, lower = 10, upper = 40, any.missing = FALSE,
        len = 2, sorted = TRUE, unique = TRUE)
    checkmate::assert_number(throttling_range, lower = 0, upper = 100)
    checkmate::assert_choice(type, c("floor", "ceiling"))
    type <- switch(type, floor = "Floor", ceiling = "Ceiling")

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    obj_zone <- eplusr::get_idf_object(idd_env, idf_env, "Zone", ids_zone)
    obj_const <- eplusr::get_idf_object(idd_env, idf_env, "Construction:InternalSource", id_const)

    # get update zone surface construction {{{
    geom <- idf$geometry()
    surf <- eplusr::get_priv_env(geom)$m_geoms$surface
    surf <- surf[J(obj_zone$object_name), on = "zone_name"][surface_type %in% type,
        .SD, .SDcols = c("id", "name", "surface_type", "construction_name", "zone_name")]
    # get surface areas in order to calculate flow fraction for each surface
    surf[, area := geom$area(object = surf$name)$area]
    surf[, flow_fraction := round(area / sum(area), 3), by = c("zone_name", "surface_type")]
    surf[, area := NULL]

    idf$set(.(surf[surface_type == type, id]) := list(construction_name = obj_const$object_name))
    # }}}

    # group surfaces by zone and type to create radiant surface groups {{{
    dt_zone <- unique(surf[, .SD, .SDcols = c("zone_name", "surface_type")], by = "zone_name")
    nm_grp <- new_object_name(idf, "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup",
        sprintf("%s_Radiant_%s_Surfaces", dt_zone$zone_name, dt_zone$surface_type))
    dt_zone[, group_surface := nm_grp]
    surf[dt_zone, on = "zone_name", `:=`(group_surface = i.group_surface)]
    rad_surf <- surf[, by = c("zone_name", "surface_type"), {
        val <- mapply(function(surf, frac) c(surf, frac), name, flow_fraction, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        val <- c(group_surface[1L], unlist(val))
        list(class = "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup", index = seq_along(val), value = val)
    }]
    rad_surf[, `:=`(id = data.table::rleid(zone_name, surface_type))]
    idf$load(rad_surf)
    # }}}

    # create low temperature radiant {{{
    # control setpoint schedules
    sch_heating <- new_schedule_compact(idf, "Sch_Zone_Radiant_Heating_Setpoint",
        "Temperature", allday = list(..24 = setpoint[1L]))
    sch_cooling <- new_schedule_compact(idf, "Sch_Zone_Radiant_Cooling_Setpoint",
        "Temperature", allday = list(..24 = setpoint[2L]))

    dt_zone[, radiant := new_object_name(idf, "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        sprintf("%s_Radiant_%s", zone_name, surface_type))]

    rad <- idf$to_table(class = rep("ZoneHVAC:LowTemperatureRadiant:VariableFlow", nrow(dt_zone)),
        init = TRUE, wide = TRUE)
    rad[, `:=`(
        Name = dt_zone$radiant,
        `Zone Name` = dt_zone$zone_name,
        `Surface Name or Radiant Surface Group Name` = dt_zone$group_surface,
        `Heating Design Capacity` = "Autosize",
        `Heating Water Inlet Node Name` = new_node_name(idf, sprintf("%s_Heating_Water_Inlet_Node", dt_zone$radiant)),
        `Heating Water Outlet Node Name` = new_node_name(idf, sprintf("%s_Heating_Water_Outlet_Node", dt_zone$radiant)),
        `Heating Control Temperature Schedule Name` = names(sch_heating),
        `Maximum Hot Water Flow` = "Autosize",
        `Cooling Design Capacity` = "Autosize",
        `Cooling Water Inlet Node Name` = new_node_name(idf, sprintf("%s_Cooling_Water_Inlet_Node", dt_zone$radiant)),
        `Cooling Water Outlet Node Name` = new_node_name(idf, sprintf("%s_Cooling_Water_Outlet_Node", dt_zone$radiant)),
        `Cooling Control Temperature Schedule Name` = names(sch_cooling),
        `Condensation Control Type` = "Off",
        `Condensation Control Dewpoint Offset` = 1,
        `Maximum Cold Water Flow` = "Autosize",
        `Cooling Control Throttling Range` = throttling_range
    )]
    obj <- idf$load(eplusr::dt_to_load(rad))
    # }}}

    get_named_id(obj)
}
# }}}
# new_chilled_water_loop {{{
new_chilled_water_loop <- function(idf, name_prefix = "ChW", supply_setpoint = 6.7,
                                   id_chiller, id_pump, ids_equip = NULL,
                                   use_equip_branch = FALSE) {
    checkmate::assert_string(name_prefix)
    checkmate::assert_number(supply_setpoint)
    checkmate::assert_int(id_chiller, null.ok = TRUE)
    checkmate::assert_int(id_pump, null.ok = TRUE)
    checkmate::assert_integer(ids_equip, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
    checkmate::assert_flag(use_equip_branch)

    node_dict <- get_pair_node_dict(idf)

    obj_pump <- idf$object(id_pump)
    if (!obj_pump$class_name() %in% idf$definition()$class_name(by_group = TRUE)$Pumps) {
        stop(sprintf("'id_pump' (%i) should be an object ID for pump object, but is binded to an object in class %s.",
            id_pump, obj_pump$class_name()
        ))
    }

    cls_chiller <- idf$definition()$class_name(by_group = TRUE)[["Plant Heating and Cooling Equipment"]]
    cls_chiller <- c("DistrictCooling", cls_chiller[startsWith(cls_chiller, "Chiller")])

    cls_tower <- idf$definition()$class_name(by_group = TRUE)[["Condenser Equipment and Heat Exchangers"]]
    cls_tower <- cls_tower[grepl("CoolingTower:|FluidCooler:|HeatExchanger:|", cls_tower)]
    cls_tower <- cls_tower[!cls_tower %in% paste0("GroundHeatExchanger:", c("Vertical:Properties", "Vertical:Array", "Vertical:Single", "ResponseFactors"))]

    obj_chiller <- idf$object(id_chiller)
    if (!obj_chiller$class_name() %in% c(cls_chiller, cls_tower)) {
        stop(sprintf("'id_chiller' (%i) should be an object ID for district cooling or chiller object, but is binded to an object in class %s.",
            id_chiller, obj_chiller$class_name()
        ))
    }

    # demand pipes
    demand_inlet <- sprintf("%s_Demand_Inlet_Pipe", name_prefix)
    demand_bypass <- sprintf("%s_Demand_Bypass_Pipe", name_prefix)
    demand_outlet <- sprintf("%s_Demand_Outlet_Pipe", name_prefix)
    demand_pipes <- new_pipe_adiabatic(idf, c(demand_inlet, demand_bypass, demand_outlet))
    demand_pipes_branch <- new_branch(idf, demand_pipes, NULL, "water", node_dict)

    # demand equipment
    demand_equip_branch <- NULL
    if (!is.null(ids_equip)) {
        # create new branches
        if (!use_equip_branch) {
            dt_equip <- idf$to_table(ids_equip)[, list(name = name[1]), by = "id"]
            demand_equip_branch <- new_branch(idf, dt_equip$id, NULL, "water", node_dict)
        # else directly treat input ID as branches
        } else {
            dt_equip <- idf$to_table(ids_equip, "Branch", wide = TRUE)
            demand_equip_branch <- setNames(dt_equip$id, dt_equip$name)
        }
    }

    # demand branches
    demand_branches <- c(demand_pipes_branch[1L], demand_equip_branch, demand_pipes_branch[-1L])

    # demand branchlist
    demand_branchlist <- new_branchlist(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # demand connectors
    demand_connectors <- new_connectors(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # supply pipes
    supply_bypass <- sprintf("%s_Supply_Bypass_Pipe", name_prefix)
    supply_outlet <- sprintf("%s_Supply_Outlet_Pipe", name_prefix)
    supply_pipes <- new_pipe_adiabatic(idf, c(supply_bypass, supply_outlet))
    supply_pipes_branch <- new_branch(idf, supply_pipes, NULL, "water", node_dict)

    # supply pump
    supply_pump_branch <- new_branch(idf, obj_pump$id(), NULL, "water", node_dict)

    # supply chiller
    supply_chiller_branch <- new_branch(idf, obj_chiller$id(), NULL, "water",
        node_dict[class == obj_chiller$class_name() & !grepl("Condenser", inlet)])

    # supply branches
    supply_branches <- c(supply_pump_branch, supply_chiller_branch, supply_pipes_branch)

    # supply branchlist
    supply_branchlist <- new_branchlist(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # supply connectors
    supply_connectors <- new_connectors(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # plant equipment operation
    plant_equiplist <- new_plant_equipment_list(idf, sprintf("%s_Equipment_List", name_prefix), obj_chiller$id())
    plant_equipopt <- new_plant_equipment_operation(idf, sprintf("%s_Equipment_Operation", name_prefix), "cooling_load", plant_equiplist, 0, 1E9)
    plant_equipoptsch <- new_plant_equipment_operation_schemes(idf, sprintf("%s_Equipment_Operation_Schemes", name_prefix),
        plant_equipopt, idf$object("Sch_Always_On")$id())

    # plant nodes
    supply_inlet <- idf$object(supply_pump_branch)$Component_1_Inlet_Node_Name
    supply_outlet <- idf$object(supply_pipes_branch[2])$Component_1_Outlet_Node_Name
    demand_inlet <- idf$object(demand_pipes_branch[1])$Component_1_Inlet_Node_Name
    demand_outlet <- idf$object(demand_pipes_branch[3])$Component_1_Outlet_Node_Name

    nm_loop <- new_object_name(idf, "PlantLoop", sprintf("%s_Loop", name_prefix))

    obj <- idf$add(
        # plant loop
        PlantLoop = list(
            name = nm_loop,
            fluid_type = "Water",
            plant_equipment_operation_scheme_name = names(plant_equipoptsch),
            loop_temperature_setpoint_node_name = supply_outlet,
            maximum_loop_temperature = 99,
            minimum_loop_temperature = 1,
            maximum_loop_flow_rate = "Autosize",
            minimum_loop_flow_rate = 0,
            plant_loop_volume = "Autocalculate",
            plant_side_inlet_node_name = supply_inlet,
            plant_side_outlet_node_name = supply_outlet,
            plant_side_branch_list_name = names(supply_branchlist),
            plant_side_connector_list_name = names(supply_connectors[3L]),
            demand_side_inlet_node_name = demand_inlet,
            demand_side_outlet_node_name = demand_outlet,
            demand_side_branch_list_name = names(demand_branchlist),
            demand_side_connector_list_name = names(demand_connectors[3L])
        ),

        Sizing_Plant = list(nm_loop, "Cooling", supply_setpoint, 5)
    )

    loop <- get_named_id(obj[1L])

    sch <- new_schedule_compact(idf,
        sprintf("%s_Loop_Supply_Temp", name_prefix),
        "Temperature", allday = list(..24 = supply_setpoint)
    )

    nm_spm <- new_object_name(idf, "SetpointManager:Scheduled", sprintf("%s_Loop_SPM_Scheduled", name_prefix))
    spm <- get_named_id(idf$add(
        SetpointManager_Scheduled = list(nm_spm, "Temperature", names(sch), supply_outlet)
    ))

    list(plant_loop = loop,
         operation_schemes = plant_equipoptsch,
         setpoint_manager = spm,
         supply = list(branch_list = supply_branches, splitter = supply_connectors[1], mixer = supply_connectors[2]),
         demand = list(branch_list = demand_branches, splitter = demand_connectors[1], mixer = demand_connectors[2])
    )
}
# }}}
# new_condenser_water_loop {{{
new_condenser_water_loop <- function(idf, name_prefix = "CndW", supply_setpoint = 29.5,
                                     id_tower, id_pump, ids_equip = NULL,
                                     use_equip_branch = FALSE) {
    checkmate::assert_string(name_prefix)
    checkmate::assert_number(supply_setpoint)
    checkmate::assert_int(id_tower, null.ok = TRUE)
    checkmate::assert_int(id_pump, null.ok = TRUE)
    checkmate::assert_integer(ids_equip, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
    checkmate::assert_flag(use_equip_branch)

    node_dict <- get_pair_node_dict(idf)

    obj_pump <- idf$object(id_pump)
    if (!obj_pump$class_name() %in% idf$definition()$class_name(by_group = TRUE)$Pumps) {
        stop(sprintf("'id_pump' (%i) should be an object ID for pump object, but is binded to an object in class %s.",
            id_pump, obj_pump$class_name()
        ))
    }

    cls_tower <- idf$definition()$class_name(by_group = TRUE)[["Condenser Equipment and Heat Exchangers"]]
    cls_tower <- cls_tower[grepl("CoolingTower:|FluidCooler:|HeatExchanger:|", cls_tower)]
    cls_tower <- cls_tower[!cls_tower %in% paste0("GroundHeatExchanger:", c("Vertical:Properties", "Vertical:Array", "Vertical:Single", "ResponseFactors"))]
    obj_tower <- idf$object(id_tower)
    if (!obj_tower$class_name() %in% cls_tower) {
        stop(sprintf("'id_pump' (%i) should be an object ID for condenser equipment, but is binded to an object in class '%s'.",
            id_tower, obj_tower$class_name()
        ))
    }

    # demand pipes
    demand_inlet <- sprintf("%s_Demand_Inlet_Pipe", name_prefix)
    demand_bypass <- sprintf("%s_Demand_Bypass_Pipe", name_prefix)
    demand_outlet <- sprintf("%s_Demand_Outlet_Pipe", name_prefix)
    demand_pipes <- new_pipe_adiabatic(idf, c(demand_inlet, demand_bypass, demand_outlet))
    demand_pipes_branch <- new_branch(idf, demand_pipes, NULL, "water", node_dict)

    # demand equipment
    demand_equip_branch <- NULL
    if (!is.null(ids_equip)) {
        # create new branches
        if (!use_equip_branch) {
            dt_equip <- idf$to_table(ids_equip)[, list(name = name[1]), by = "id"]
            demand_equip_branch <- new_branch(idf, dt_equip$id, NULL, "water", node_dict)
        # else directly treat input ID as branches
        } else {
            dt_equip <- idf$to_table(ids_equip, "Branch", wide = TRUE)
            demand_equip_branch <- setNames(dt_equip$id, dt_equip$name)
        }
    }

    # demand branches
    demand_branches <- c(demand_pipes_branch[1L], demand_equip_branch, demand_pipes_branch[-1L])

    # demand branchlist
    demand_branchlist <- new_branchlist(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # demand connectors
    demand_connectors <- new_connectors(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # supply pipes
    supply_bypass <- sprintf("%s_Supply_Bypass_Pipe", name_prefix)
    supply_outlet <- sprintf("%s_Supply_Outlet_Pipe", name_prefix)
    supply_pipes <- new_pipe_adiabatic(idf, c(supply_bypass, supply_outlet))
    supply_pipes_branch <- new_branch(idf, supply_pipes, NULL, "water", node_dict)

    # supply pump
    supply_pump_branch <- new_branch(idf, obj_pump$id(), NULL, "water", node_dict)

    # supply tower
    supply_tower_branch <- new_branch(idf, obj_tower$id(), NULL, "water")

    # supply branches
    supply_branches <- c(supply_pump_branch, supply_tower_branch, supply_pipes_branch)

    # supply branchlist
    supply_branchlist <- new_branchlist(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # supply connectors
    supply_connectors <- new_connectors(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # plant equipment operation
    plant_equiplist <- new_condenser_equipment_list(idf, sprintf("%s_Equipment_List", name_prefix), obj_tower$id())
    plant_equipopt <- new_plant_equipment_operation(idf, sprintf("%s_Equipment_Operation", name_prefix), "cooling_load", plant_equiplist, 0, 1E9)
    plant_equipoptsch <- new_condenser_equipment_operation_schemes(idf, sprintf("%s_Equipment_Operation_Schemes", name_prefix),
        plant_equipopt, idf$object("Sch_Always_On")$id())

    # plant nodes
    supply_inlet <- idf$object(supply_pump_branch)$Component_1_Inlet_Node_Name
    supply_outlet <- idf$object(supply_pipes_branch[2])$Component_1_Outlet_Node_Name
    demand_inlet <- idf$object(demand_pipes_branch[1])$Component_1_Inlet_Node_Name
    demand_outlet <- idf$object(demand_pipes_branch[3])$Component_1_Outlet_Node_Name

    nm_loop <- new_object_name(idf, "CondenserLoop", sprintf("%s_Loop", name_prefix))

    obj <- idf$add(
        # condenser loop
        CondenserLoop = list(
            name = nm_loop,
            fluid_type = "Water",
            condenser_equipment_operation_scheme_name = names(plant_equipoptsch),
            condenser_loop_temperature_setpoint_node_name = supply_outlet,
            maximum_loop_temperature = 99,
            minimum_loop_temperature = 1,
            maximum_loop_flow_rate = "Autosize",
            minimum_loop_flow_rate = 0,
            condenser_loop_volume = "Autocalculate",
            condenser_side_inlet_node_name = supply_inlet,
            condenser_side_outlet_node_name = supply_outlet,
            condenser_side_branch_list_name = names(supply_branchlist),
            condenser_side_connector_list_name = names(supply_connectors[3L]),
            demand_side_inlet_node_name = demand_inlet,
            demand_side_outlet_node_name = demand_outlet,
            condenser_demand_side_branch_list_name = names(demand_branchlist),
            condenser_demand_side_connector_list_name = names(demand_connectors[3L])
        ),

        Sizing_Plant = list(nm_loop, "Condenser", supply_setpoint, 5)
    )

    loop <- get_named_id(obj[1L])

    sch <- new_schedule_compact(idf,
        sprintf("%s_Loop_Supply_Temp", name_prefix),
        "Temperature", allday = list(..24 = supply_setpoint)
    )

    nm_spm <- new_object_name(idf, "SetpointManager:Scheduled", sprintf("%s_Loop_SPM_Scheduled", name_prefix))
    spm <- get_named_id(idf$add(
        SetpointManager_Scheduled = list(nm_spm, "Temperature", names(sch), supply_outlet)
    ))

    list(condenser_loop = loop,
         operation_schemes = plant_equipoptsch,
         setpoint_manager = spm,
         supply = list(branch_list = supply_branches, splitter = supply_connectors[1], mixer = supply_connectors[2]),
         demand = list(branch_list = demand_branches, splitter = demand_connectors[1], mixer = demand_connectors[2])
    )
}
# }}}
# new_hot_water_loop {{{
new_hot_water_loop <- function(idf, name_prefix = "HW", supply_setpoint = 60,
                               id_boiler, id_pump, ids_equip = NULL,
                               use_equip_branch = FALSE) {
    checkmate::assert_string(name_prefix)
    checkmate::assert_number(supply_setpoint)
    checkmate::assert_int(id_boiler, null.ok = TRUE)
    checkmate::assert_int(id_pump, null.ok = TRUE)
    checkmate::assert_integer(ids_equip, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
    checkmate::assert_flag(use_equip_branch)

    node_dict <- get_pair_node_dict(idf)

    obj_pump <- idf$object(id_pump)
    if (!obj_pump$class_name() %in% idf$definition()$class_name(by_group = TRUE)$Pumps) {
        stop(sprintf("'id_pump' (%i) should be an object ID for pump object, but is binded to an object in class %s.",
            id_pump, obj_pump$class_name()
        ))
    }

    cls_boiler <- idf$definition()$class_name(by_group = TRUE)[["Plant Heating and Cooling Equipment"]]
    cls_boiler <- c("DistrictHeating", cls_boiler[startsWith(cls_boiler, "Boiler")])

    cls_ex <- idf$definition()$class_name(by_group = TRUE)[["Condenser Equipment and Heat Exchangers"]]
    cls_ex <- cls_ex[grepl("HeatExchanger:", cls_ex)]
    cls_ex <- cls_ex[!cls_ex %in% paste0("GroundHeatExchanger:", c("Vertical:Properties", "Vertical:Array", "Vertical:Single", "ResponseFactors"))]

    obj_boiler <- idf$object(id_boiler)
    if (!obj_boiler$class_name() %in% c(cls_boiler, cls_ex)) {
        stop(sprintf("'id_pump' (%i) should be an object ID for district heating or boiler object or heat exchanger, but is binded to an object in class %s.",
            id_boiler, obj_boiler$class_name()
        ))
    }

    # demand pipes
    demand_inlet <- sprintf("%s_Demand_Inlet_Pipe", name_prefix)
    demand_bypass <- sprintf("%s_Demand_Bypass_Pipe", name_prefix)
    demand_outlet <- sprintf("%s_Demand_Outlet_Pipe", name_prefix)
    demand_pipes <- new_pipe_adiabatic(idf, c(demand_inlet, demand_bypass, demand_outlet))
    demand_pipes_branch <- new_branch(idf, demand_pipes, NULL, "water", node_dict)

    # demand equipment
    demand_equip_branch <- NULL
    if (!is.null(ids_equip)) {
        # create new branches
        if (!use_equip_branch) {
            dt_equip <- idf$to_table(ids_equip)[, list(name = name[1]), by = "id"]
            demand_equip_branch <- new_branch(idf, dt_equip$id, NULL, "water", node_dict)
        # else directly treat input ID as branches
        } else {
            dt_equip <- idf$to_table(ids_equip, "Branch", wide = TRUE)
            demand_equip_branch <- setNames(dt_equip$id, dt_equip$name)
        }
    }

    # demand branches
    demand_branches <- c(demand_pipes_branch[1L], demand_equip_branch, demand_pipes_branch[-1L])

    # demand branchlist
    demand_branchlist <- new_branchlist(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # demand connectors
    demand_connectors <- new_connectors(idf, sprintf("%s_Demand", name_prefix), demand_branches)

    # supply pipes
    supply_bypass <- sprintf("%s_Supply_Bypass_Pipe", name_prefix)
    supply_outlet <- sprintf("%s_Supply_Outlet_Pipe", name_prefix)
    supply_pipes <- new_pipe_adiabatic(idf, c(supply_bypass, supply_outlet))
    supply_pipes_branch <- new_branch(idf, supply_pipes, NULL, "water", node_dict)

    # supply pump
    supply_pump_branch <- new_branch(idf, obj_pump$id(), NULL, "water", node_dict)

    # supply boiler
    supply_boiler_branch <- new_branch(idf, obj_boiler$id(), NULL, "water",
        node_dict[class == obj_boiler$class_name()])

    # supply branches
    supply_branches <- c(supply_pump_branch, supply_boiler_branch, supply_pipes_branch)

    # supply branchlist
    supply_branchlist <- new_branchlist(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # supply connectors
    supply_connectors <- new_connectors(idf, sprintf("%s_Supply", name_prefix), supply_branches)

    # plant equipment operation
    plant_equiplist <- new_plant_equipment_list(idf, sprintf("%s_Equipment_List", name_prefix), obj_boiler$id())
    plant_equipopt <- new_plant_equipment_operation(idf, sprintf("%s_Equipment_Operation", name_prefix), "cooling_load", plant_equiplist, 0, 1E9)
    plant_equipoptsch <- new_plant_equipment_operation_schemes(idf, sprintf("%s_Equipment_Operation_Schemes", name_prefix),
        plant_equipopt, idf$object("Sch_Always_On")$id())

    # plant nodes
    supply_inlet <- idf$object(supply_pump_branch)$Component_1_Inlet_Node_Name
    supply_outlet <- idf$object(supply_pipes_branch[2])$Component_1_Outlet_Node_Name
    demand_inlet <- idf$object(demand_pipes_branch[1])$Component_1_Inlet_Node_Name
    demand_outlet <- idf$object(demand_pipes_branch[3])$Component_1_Outlet_Node_Name

    nm_loop <- new_object_name(idf, "PlantLoop", sprintf("%s_Loop", name_prefix))

    obj <- idf$add(
        # plant loop
        PlantLoop = list(
            name = nm_loop,
            fluid_type = "Water",
            plant_equipment_operation_scheme_name = names(plant_equipoptsch),
            loop_temperature_setpoint_node_name = supply_outlet,
            maximum_loop_temperature = 99,
            minimum_loop_temperature = 1,
            maximum_loop_flow_rate = "Autosize",
            minimum_loop_flow_rate = 0,
            plant_loop_volume = "Autocalculate",
            plant_side_inlet_node_name = supply_inlet,
            plant_side_outlet_node_name = supply_outlet,
            plant_side_branch_list_name = names(supply_branchlist),
            plant_side_connector_list_name = names(supply_connectors[3L]),
            demand_side_inlet_node_name = demand_inlet,
            demand_side_outlet_node_name = demand_outlet,
            demand_side_branch_list_name = names(demand_branchlist),
            demand_side_connector_list_name = names(demand_connectors[3L])
        ),

        Sizing_Plant = list(nm_loop, "Heating", supply_setpoint, 10)
    )

    loop <- get_named_id(obj[1L])

    sch <- new_schedule_compact(idf,
        sprintf("%s_Loop_Supply_Temp", name_prefix),
        "Temperature", allday = list(..24 = supply_setpoint)
    )

    nm_spm <- new_object_name(idf, "SetpointManager:Scheduled", sprintf("%s_Loop_SPM_Scheduled", name_prefix))
    spm <- get_named_id(idf$add(
        SetpointManager_Scheduled = list(nm_spm, "Temperature", names(sch), supply_outlet)
    ))

    list(plant_loop = loop,
         operation_schemes = plant_equipoptsch,
         setpoint_manager = spm,
         supply = list(branch_list = supply_branches, splitter = supply_connectors[1], mixer = supply_connectors[2]),
         demand = list(branch_list = demand_branches, splitter = demand_connectors[1], mixer = demand_connectors[2])
    )
}
# }}}

# new_schedule_compact {{{
new_schedule_compact <- function(idf, name, type_limits, ...) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_string(name)

    name <- new_object_name(idf, "Schedule:Compact", name)

    sch <- eplusr::schedule_compact(idf, name, new = TRUE)
    sch$type_limits(type_limits)
    eplusr:::idfsch_cmpt_set(NULL, sch, eplusr::get_priv_env(sch), ..., .env = parent.frame())

    stats::setNames(sch$id(), sch$name())
}
# }}}
# new_pipe_adiabatic {{{
new_pipe_adiabatic <- function(idf, names) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_character(names, any.missing = FALSE, unique = TRUE)

    pipe <- idf$to_table(class = rep("Pipe:Adiabatic", length(names)), init = TRUE, wide = TRUE)

    names <- new_object_name(idf, "Pipe:Adiabatic", names)

    pipe[, `:=`(
        Name = names,
        `Inlet Node Name` = sprintf("%s_Inlet_Node", names),
        `Outlet Node Name` = sprintf("%s_Outlet_Node", names)
    )]

    obj <- idf$load(eplusr::dt_to_load(pipe))

    get_named_id(obj)
}
# }}}

# new_branch {{{
new_branch <- function(idf, ids, name_prefix = NULL, node_type = NULL, node_dict = NULL) {
    nodes <- get_pair_node(idf, ids, node_type, node_dict)

    checkmate::assert_character(name_prefix, null.ok = TRUE, max.len = length(ids))
    if (is.null(name_prefix)) {
        name_prefix <- rep("", length(ids))
    } else if (length(name_prefix) == 1L) {
        name_prefix <- rep(paste0("_", name_prefix), length(ids))
    } else if (length(name_prefix) == length(ids)) {
        name_prefix <- paste0("_", name_prefix)
    } else {
        stop("'name_prefix' for branches should be NULL, or have a length of 1 of the same length as 'ids'")
    }

    bran <- idf$to_table(class = rep("Branch", nrow(nodes)), init = TRUE, wide = TRUE)

    nm <- new_object_name(idf, "Branch", sprintf("%s%s_Branch", nodes$name, name_prefix))

    bran[, `:=`(
        Name = nm,
        `Component 1 Object Type` = nodes$class,
        `Component 1 Name` = nodes$name,
        `Component 1 Inlet Node Name` = nodes$value_inlet,
        `Component 1 Outlet Node Name` = nodes$value_outlet
    )]

    obj <- idf$load(eplusr::dt_to_load(bran))

    get_named_id(obj)
}
# }}}
# new_branchlist {{{
new_branchlist <- function(idf, name_prefix = "ChW_Demand", ids) {
    checkmate::assert_string(name_prefix)
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    nm_branlist <- new_object_name(idf, "BranchList", sprintf("%s_Branches", name_prefix))

    dt_obj <- idf$to_table(ids, class = "Branch")[, list(name = name[1]), by = "id"]

    obj <- idf$add(BranchList = as.list(c(nm_branlist, dt_obj$name)))

    get_named_id(obj)
}
# }}}
# new_connectors {{{
new_connectors <- function(idf, name_prefix = "ChW_Demand", ids) {
    checkmate::assert_string(name_prefix)
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    dt_obj <- idf$to_table(ids, class = "Branch")[, list(name = name[1]), by = "id"]

    nm_splitter <- new_object_name(idf, "Connector:Splitter", sprintf("%s_Splitter", name_prefix))
    nm_mixer <- new_object_name(idf, "Connector:Mixer", sprintf("%s_Mixer", name_prefix))
    nm_conn <- new_object_name(idf, "ConnectorList", sprintf("%s_Connectors", name_prefix))

    obj <- idf$add(
        # splitter and mixer
        Connector_Splitter = as.list(c(nm_splitter, dt_obj$name[-length(ids)])),
        Connector_Mixer = as.list(c(nm_mixer, dt_obj$name[length(ids)], dt_obj$name[-c(1L, length(ids))])),
        ConnectorList = as.list(c(nm_conn, "Connector:Splitter", nm_splitter, "Connector:Mixer", nm_mixer))
    )

    get_named_id(obj)
}
# }}}

# new_plant_equipment_list {{{
new_plant_equipment_list <- function(idf, name, ids) {
    checkmate::assert_string(name)
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    name <- new_object_name(idf, "PlantEquipmentList", name)

    dt_obj <- idf$to_table(ids)[, list(class = class[1L], name = name[1L]), by = "id"]

    val <- data.table::transpose(list(dt_obj$class, dt_obj$name))
    obj <- idf$add(PlantEquipmentList = as.list(c(name, unlist(val))))

    get_named_id(obj)
}
# }}}
# new_condenser_equipment_list {{{
new_condenser_equipment_list <- function(idf, name, ids) {
    checkmate::assert_string(name)
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    name <- new_object_name(idf, "CondenserEquipmentList", name)

    dt_obj <- idf$to_table(ids)[, list(class = class[1L], name = name[1]), by = "id"]

    val <- data.table::transpose(list(dt_obj$class, dt_obj$name))
    obj <- idf$add(CondenserEquipmentList = as.list(c(name, unlist(val))))

    get_named_id(obj)
}
# }}}
# new_plant_equipment_operation {{{
new_plant_equipment_operation <- function(idf, name, type, ids, lowers, uppers, ref_node = NULL) {
    checkmate::assert_string(name)

    cls <- idf$class_name(by_group = TRUE, all = TRUE)[["Plant-Condenser Control"]]
    cls <- cls[startsWith(cls, "PlantEquipmentOperation:")]
    cls <- grep("ComponentSetpoint|ThermalEnergyStorage", cls, value = TRUE, invert = TRUE)
    types <- tolower(gsub("([a-z])([A-Z])", "\\1_\\2", substring(cls, 25L)))

    checkmate::assert_choice(type, types)
    checkmate::assert_integer(ids, any.missing = FALSE)
    checkmate::assert_numeric(lowers, finite = TRUE, any.missing = FALSE, len = length(ids), sorted = TRUE)
    checkmate::assert_numeric(uppers, finite = TRUE, any.missing = FALSE, len = length(ids), sorted = TRUE)
    checkmate::assert_string(ref_node, null.ok = TRUE)

    dt_obj <- idf$to_table(ids)[, list(name = name[1]), by = "id"]

    val <- unlist(lapply(seq_len(nrow(dt_obj)), function(i) list(lowers[i], uppers[i], dt_obj$name[i])), FALSE)

    if (endsWith(type, "difference")) {
        if (is.null(ref_node)) {
            stop(sprintf("'ref_node' should not be empty when 'type' is '%s'.", type))
        }
        val <- c(list(ref_node), val)
    }

    cls <- cls[match(type, types)]
    name <- new_object_name(idf, cls, name)

    val <- c(list(name), val)

    obj <- idf$add(.(cls) := val)

    get_named_id(obj)
}
# }}}
# new_plant_equipment_operation_schemes {{{
new_plant_equipment_operation_schemes <- function(idf, name, ids_opt, ids_sch) {
    checkmate::assert_string(name)
    checkmate::assert_integer(ids_opt, any.missing = FALSE, unique = TRUE)
    checkmate::assert_integer(ids_sch, any.missing = FALSE)

    dt_opt <- idf$to_table(ids_opt)[, list(class = class[1L], name = name[1L]), by = "id"]
    dt_sch <- idf$to_table(ids_sch)[, list(name = name[1L]), by = "id"]

    val <- unlist(data.table::transpose(list(dt_opt$class, dt_opt$name, dt_sch$name)))

    name <- new_object_name(idf, "PlantEquipmentOperationSchemes", name)

    val <- c(name, val)

    obj <- idf$add(PlantEquipmentOperationSchemes = as.list(val))

    get_named_id(obj)
}
# }}}
# new_condenser_equipment_operation_schemes {{{
new_condenser_equipment_operation_schemes <- function(idf, name, ids_opt, ids_sch) {
    checkmate::assert_string(name)
    checkmate::assert_integer(ids_opt, any.missing = FALSE, unique = TRUE)
    checkmate::assert_integer(ids_sch, any.missing = FALSE)

    dt_opt <- idf$to_table(ids_opt)[, list(class = class[1L], name = name[1]), by = "id"]
    dt_sch <- idf$to_table(ids_sch)[, list(name = name[1]), by = "id"]

    val <- unlist(data.table::transpose(list(dt_opt$class, dt_opt$name, dt_sch$name)))

    name <- new_object_name(idf, "CondenserEquipmentOperationSchemes", name)

    val <- c(name, val)

    obj <- idf$add(CondenserEquipmentOperationSchemes = as.list(val))

    get_named_id(obj)
}
# }}}

# new_object_name {{{
new_object_name <- function(idf, class, names) {
    checkmate::assert_string(class)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    cls <- eplusr::get_idd_class(idd_env, class)

    if (!cls$class_id %in% idf_env$object$class_id) return(names)

    dict <- idf_env$object[J(cls$class_id), on = "class_id", object_name]

    make_unique(names, dict)
}
# }}}
# new_node_name {{{
new_node_name <- function(idf, nodes) {
    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    dict <- eplusr::get_idf_value(idd_env, idf_env, property = "type")[
        type == "node" & !is.na(value_chr), tolower(value_chr)]

    make_unique(nodes, dict)
}
# }}}
# make_unique {{{
make_unique <- function(x, dict) {
    lower <- stringi::stri_trans_tolower(x)
    res <- make.unique(c(stringi::stri_trans_tolower(dict), lower), "_")[-(1:length(dict))]
    changed <- lower != res
    x[changed] <- paste0(
        x[changed],
        stringi::stri_sub(res[changed], stringi::stri_length(x[changed]) + 1L)
    )

    x
}
# }}}

# get_named_id {{{
get_named_id <- function(objs) vapply(objs, function(x) x$id(), integer(1L))
# }}}
# get_pair_node_dict {{{
get_pair_node_dict <- function(idf) {
    # get supported classes in Branch
    all_cls <- idf$definition("Branch")$field_relation("Component 1 Name", "ref_to")$ref_to$src_class_name

    # extract all fields
    fld <- idf$definition()$to_table(class = all_cls)

    # get inlet nodes
    nodes_inlet <- fld[grepl("Inlet Node", field, fixed = TRUE), list(class, inlet = field)]
    nodes_inlet[, outlet := gsub("Inlet", "Outlet", inlet, fixed = TRUE)]
    nodes_outlet <- fld[grepl("Outlet Node", field, fixed = TRUE), list(class, outlet = field)]
    nodes_outlet[, inlet := gsub("Outlet", "Inlet", outlet, fixed = TRUE)]

    nodes <- nodes_inlet[nodes_outlet, on = c("class", "inlet", "outlet"), nomatch = NULL]

    nodes[, type := data.table::fcase(
        grepl("Air", inlet, fixed = TRUE), "air",
        grepl("Water", inlet, fixed = TRUE), "water"
    )]
    nodes[is.na(type) & class %in% c("Duct", "TemperingValve"), type := "air"]
    nodes[is.na(type) & class %in% c("TemperingValve", "LoadProfile:Plant", "HeatExchanger:FluidToFluid", "ZoneHVAC:Baseboard:RadiantConvective:Steam"), type := "water"]
    nodes[is.na(type) & grepl("Water|Pump|Pipe|DX|Generator|HeatExchanger|Plant|Chiller|ThermalStorage|SolarCollector", class), type := "water"]
    nodes[is.na(type) & grepl("Plant", inlet, fixed = TRUE), type := "water"]

    nodes
}
# }}}
# get_pair_node {{{
get_pair_node <- function(idf, ids, node_type = NULL, dict = NULL) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)
    checkmate::assert_choice(node_type, c("air", "water"), null.ok = TRUE)
    checkmate::assert_data_table(dict, null.ok = TRUE)

    obj <- idf$to_table(ids)

    if (is.null(dict)) dict <- get_pair_node_dict(idf)

    inlet <- dict[, list(class, inlet, type)][obj, on = c("class", "inlet" = "field"), nomatch = NULL]
    inlet <- inlet[J(ids), on = "id"]
    outlet <- dict[, list(class, outlet, type)][obj, on = c("class", "outlet" = "field"), nomatch = NULL]
    outlet <- outlet[J(ids), on = "id"]

    if (any(invld <- !obj$class %in% dict$class)) {
        stop("Invalid component type for 'Branch': ", paste(unique(obj$class[invld]), collapse = ", "))
    }

    # check node
    if (is.null(node_type)) {
        if (length(id_mult <- inlet[, by = "id", .N][N > 1, id])) {
            stop(inlet[J(id_mult[1L]), on = "id", sprintf(
                "Please specify type of nodes to locate. Multiple inlet nodes found for %s '%s': %s",
                class[1], name[1], paste(value, collapse = ", "))]
            )
        }
        if (length(id_mult <- outlet[, by = "id", .N][N > 1, id])) {
            stop(outlet[J(id_mult[1L]), on = "id", sprintf(
                "Please specify type of nodes to locate. Multiple outlet nodes found for %s '%s': %s",
                class[1], name[1], paste(value, collapse = ", "))]
            )
        }
    } else {
        inlet <- inlet[J(ids, node_type), on = c("id", "type")]
        if (anyNA(inlet$inlet)) {
            invld <- obj[J(inlet[is.na(inlet), id]), on = "id", mult = "first"]
            stop(sprintf("Failed to find %s inlet node for objects below:\n%s",
                node_type,
                invld[, paste(sprintf("[%i] '%s' [ID:%i] in class '%s'",
                    .I, name, id, class), collapse = "\n")]
            ))
        }
        outlet <- outlet[J(ids, node_type), on = c("id", "type")]
        if (anyNA(outlet$outlet)) {
            invld <- obj[J(outlet[is.na(outlet), id]), on = "id", mult = "first"]
            stop(sprintf("Failed to find %s outlet node for objects below:\n%s",
                node_type,
                invld[, paste(sprintf("[%i] '%s' [ID:%i] in class '%s'",
                    .I, name, id, class), collapse = "\n")]
            ))
        }
    }

    inlet[, outlet := gsub("Inlet", "Outlet", inlet, fixed = TRUE)]
    inlet[outlet, on = c("id", "outlet"), `:=`(index_outlet = i.index, value_outlet = i.value)]

    data.table::setnames(inlet, c("index", "value", "inlet", "outlet"),
        c("index_inlet", "value_inlet", "field_inlet", "field_outlet"))
    data.table::setcolorder(inlet,
        c("id", "name", "class", "type",
          "index_inlet", "field_inlet", "value_inlet",
          "index_outlet", "field_outlet", "value_outlet"))

    if (anyNA(inlet$value_outlet)) {
        stop(inlet[is.na(value_outlet)][1 , sprintf(
            "Failed to find outlet node '%s' for %s '%s'",
            outlet, class[1], name[1])]
        )
        stop(sprintf("Failed to find corresponding outlet node for object inlet node below:\n%s",
            inlet[is.na(value_outlet)][, paste(sprintf(
                "[%i] '%s' [ID:%i] in class '%s' (Inlet node: '%s')",
                .I, name, id, class, value_inlet), collapse = "\n")]
        ))
    }

    inlet
}
# }}}

# del_all_water_loops {{{
del_all_water_loops <- function(idf, keep_component = FALSE) {
    checkmate::assert_r6(idf, "Idf")

    ids <- idf$object_id()[c("CondenserLoop", "PlantLoop")]

    if (!is.null(ids[[1L]])) {
        for (id in ids[[1L]]){
            del_condenser_loop(idf, id, keep_component = keep_component)
        }
    }

    if (!is.null(ids[[2L]])) {
        for (id in ids[[2L]]){
            del_plant_loop(idf, id, keep_component = keep_component)
        }
    }

    idf
}
# }}}
# del_water_loop {{{
del_water_loop <- function(idf, id_loop, type = "plant", keep_branch = FALSE, keep_component = FALSE) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_int(id_loop)
    checkmate::assert_choice(type, c("plant", "condenser"))
    checkmate::assert_flag(keep_branch)
    checkmate::assert_flag(keep_component)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    if (type == "plant") {
        cls <- "PlantLoop"
        fld <- c(
            "Name",                                  # 1
            "Availability Manager List Name",        # 2
            "Plant Equipment Operation Scheme Name", # 3
            "Plant Side Branch List Name",           # 4
            "Demand Side Branch List Name",          # 5
            "Plant Side Connector List Name",        # 6
            "Demand Side Connector List Name"        # 7
        )
    } else {
        cls <- "CondenserLoop"
        fld <- c(
            "Name",                                      # 1
            "Fluid Type",                                # 2 just a placeholder
            "Condenser Equipment Operation Scheme Name", # 3
            "Condenser Side Branch List Name",           # 4
            "Condenser Demand Side Branch List Name",    # 5
            "Condenser Side Connector List Name",        # 6
            "Condenser Demand Side Connector List Name"  # 7
        )
    }

    dt_obj <- eplusr::get_idf_object(idd_env, idf_env, cls, id_loop)
    dt_val <- eplusr::get_idf_value(idd_env, idf_env, cls, id_loop, fld)

    # setpoint manager for this airloop
    del_setpoint_managers(idf, id_loop)

    # sizing plant
    rel <- eplusr::get_idf_relation(idd_env, idf_env, NULL, dt_val$value_id[1], "ref_by", 0L,
        class_ref = "none")
    del_if_exist(idf, rel$object_id)

    rel <- eplusr::get_idf_relation(idd_env, idf_env, NULL, dt_val$value_id, "ref_to", 2L, TRUE,
        class_ref = "none", match_all = TRUE)

    # skip any schedules
    ids_sch <- eplusr:::get_idf_object_multi_scope(idd_env, idf_env, group = "Schedules")$object_id
    rel <- rel[!J(ids_sch), on = "object_id"][!J(ids_sch), on = "src_object_id"]

    # availability
    ids_avail <- get_ids_from_rel(rel, dt_val$value_id[2L], NULL, "source")

    # operation scheme
    ids_optsch <- get_ids_from_rel(rel, dt_val$value_id[3L], NULL, "source")

    # connectors
    ids_conn <- get_ids_from_rel(rel, dt_val$value_id[6:7], 1L, "source")

    del_if_exist(idf, c(id_loop, ids_avail, ids_optsch, ids_conn))

    # branches
    rel_bran <- get_leaf_from_rel(rel, dt_val$value_id[4:5], NULL, "source")
    disconnect_branch(idf, rel_bran[dep == 2L, unique(object_id)])
    del_if_exist(idf, rel_bran[dep == 0L, src_object_id])

    if (!keep_branch) {
        ids_bran <- rel_bran[dep == 2L, unique(object_id)]
        del_if_exist(idf, ids_bran)
    }
    if (!keep_component) {
        ids_comp <- rel_bran[dep == 2L, unique(src_object_id)]
        ids_bran <- rel_bran[dep == 2L, unique(object_id)]

        disconnect_component(idf, ids_comp, ids_bran)

        # keep the component if it is still referred in another branch
        rel_comp_keep <- eplusr::get_idf_relation(idd_env, idf_env, ids_comp, NULL,
            "ref_by", class = "Branch", class_ref = "none")

        if (nrow(rel_comp_keep)) {
            obj_keep <- eplusr::get_idf_object(idd_env, idf_env, NULL, rel_comp_keep$src_object_id)
            obj_bran <- eplusr::get_idf_object(idd_env, idf_env, NULL, rel_comp_keep$object_id)
            message(sprintf("Components below are used in branches not belong to this loop and will be kept:\n%s",
                paste(sprintf("[%i] '%s' object '%s' [ID:%i] in Branch %s [ID:%i]",
                    seq_len(nrow(rel_comp_keep)),
                    obj_keep$class_name, obj_keep$object_name, obj_keep$object_id,
                    obj_bran$object_name, obj_bran$object_id
                ), collapse = "\n")
            ))
            ids_comp <- setdiff(ids_comp, obj_keep$object_id)
        }

        del_setpoint_managers(idf, ids_comp)
        del_if_exist(idf, ids_comp, ref_to = TRUE)
    }

    idf
}
# }}}
# del_plant_loop {{{
del_plant_loop <- function(idf, id_loop, keep_branch = FALSE, keep_component = TRUE) {
    del_water_loop(idf, id_loop, "plant", keep_branch, keep_component)
}
# }}}
# del_condenser_loop {{{
del_condenser_loop <- function(idf, id_loop, keep_branch = FALSE, keep_component = TRUE) {
    del_water_loop(idf, id_loop, "condenser", keep_branch, keep_component)
}
# }}}
# del_setpoint_managers {{{
del_setpoint_managers <- function(idf, ids) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    val <- eplusr::get_idf_value(idd_env, idf_env, object = ids, property = "type")[
        J("node"), on = "type", nomatch = NULL]

    if (!nrow(val)) return(idf)

    rel <- eplusr::get_idf_node_relation(idd_env, idf_env,
        val$object_id, val$value_id, class = get_setpoint_manager_classes(idf),
        name = TRUE
    )

    del_if_exist(idf, rel$object_id)
}
# }}}
# get_setpoint_manager_classes {{{
get_setpoint_manager_classes <- function(idf) {
    idf$class_name(by_group = TRUE)[["Setpoint Managers"]]
}
# }}}

# disconnect_equipment {{{
disconnect_equipment <- function(idf, ids, ids_zone = NULL) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)
    checkmate::assert_integer(ids_zone, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    dt_obj <- eplusr::get_idf_object(idd_env, idf_env, object = ids)

    rel_obj <- eplusr::get_idf_relation(idd_env, idf_env, dt_obj$object_id, NULL,
        "ref_by", class = "Branch", class_ref = "none")
    if (!is.null(ids_branch)) rel_obj <- rel_obj[object_id %in% ids_branch]

    del_idf_value_by_relation(idf, rel_obj)
}
# }}}
# disconnect_component {{{
disconnect_component <- function(idf, ids, ids_branch = NULL) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)
    checkmate::assert_integer(ids_branch, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    dt_obj <- eplusr::get_idf_object(idd_env, idf_env, object = ids)

    rel_obj <- eplusr::get_idf_relation(idd_env, idf_env, dt_obj$object_id, NULL,
        "ref_by", class = "Branch", class_ref = "none")
    if (!is.null(ids_branch)) rel_obj <- rel_obj[object_id %in% ids_branch]

    del_idf_value_by_relation(idf, rel_obj)
}
# }}}
# disconnect_branch {{{
disconnect_branch <- function(idf, ids) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_integer(ids, any.missing = FALSE, unique = TRUE)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    dt_obj <- eplusr::get_idf_object(idd_env, idf_env, "Branch", ids)

    rel_obj <- eplusr::get_idf_relation(idd_env, idf_env, dt_obj$object_id, NULL,
        "ref_by", class_ref = "none")

    del_idf_value_by_relation(idf, rel_obj)
}
# }}}
# del_idf_value_by_relation {{{
del_idf_value_by_relation <- function(idf, relation) {
    checkmate::assert_r6(idf, "Idf")
    checkmate::assert_data_table(relation)

    if (!nrow(relation)) return(idf)

    idd_env <- eplusr::get_priv_env(idf)$idd_env()
    idf_env <- eplusr::get_priv_env(idf)$idf_env()

    ids <- unique(relation$object_id)
    dt_obj <- eplusr::get_idf_object(idd_env, idf_env, object = ids)
    dt_val <- eplusr::get_idf_value(idd_env, idf_env, object = ids, property = "extensible_group")

    dt_val[relation, on = "value_id", value_id_del := i.value_id]

    # if extensibile field, remove the whole extensible group
    ext <- dt_val[!is.na(value_id_del) & extensible_group > 0L, list(object_id, extensible_group)]
    dt_val[ext, on = c("object_id", "extensible_group"), `:=`(value_chr = NA_character_, value_num = NA_real_, value_id_del = value_id)]
    # directly remove otherwise
    dt_val[!is.na(value_id_del) & extensible_group == 0L, `:=`(value_chr = NA_character_, value_num = NA_real_)]

    dt_val[, by = "object_id", `:=`(
        value_chr = c(value_chr[is.na(value_id_del)], value_chr[!is.na(value_id_del)]),
        value_num = c(value_num[is.na(value_id_del)], value_num[!is.na(value_id_del)])
    )]
    dt_val[, c("value_id_del", "extensible_group") := NULL]

    chk <- eplusr::level_checks()
    chk$required_field <- FALSE
    set <- eplusr::set_idf_object(idd_env, idf_env, dt_obj, dt_val, replace = TRUE, level = chk)
    eplusr::get_priv_env(idf)$log_add_order(c(set$changed, set$updated))
    eplusr::get_priv_env(idf)$log_unsaved()
    eplusr::get_priv_env(idf)$log_new_uuid()
    eplusr::get_priv_env(idf)$update_idf_env(set)

    idf
}
# }}}

# get_ids_from_rel {{{
get_ids_from_rel <- function(rel, init_value_id, depth = 0L, type) {
    checkmate::assert_int(depth, null.ok = TRUE)
    checkmate::assert_choice(type, c("source", "target"))

    if (is.null(depth) || depth > max(rel$dep)) depth <- max(rel$dep)

    cur_depth <- 0L

    if (type == "source") {
        col_init <- "value_id"
        col_on <- "object_id"
        col_id <- "src_object_id"

    } else {
        col_init <- "src_value_id"
        col_on <- "src_object_id"
        col_id <- "object_id"
    }

    id <- unique(rel[J(cur_depth, init_value_id), on = c("dep", col_init),
        .SD, .SDcols = col_id, nomatch = NULL][[1L]])

    ids <- id

    if (!length(id)) return(ids)

    cur_depth <- cur_depth + 1L
    while (cur_depth <= depth) {
        id <- unique(rel[J(cur_depth, id), on = c("dep", col_on),
            .SD, .SDcols = col_id, nomatch = NULL][[1L]])

        if (!length(id)) break

        ids <- c(ids, id)
        cur_depth <- cur_depth + 1L
    }

    unique(ids)
}
# }}}
# get_leaf_from_rel {{{
get_leaf_from_rel <- function(rel, init_value_id, depth = 0L, type) {
    checkmate::assert_int(depth, null.ok = TRUE)
    checkmate::assert_choice(type, c("source", "target"))

    if (is.null(depth) || depth > max(rel$dep)) depth <- max(rel$dep)

    ids <- c()

    cur_depth <- 0L

    if (type == "source") {
        col_init <- "value_id"
        col_on <- "object_id"
        col_id <- "src_object_id"

    } else {
        col_init <- "src_value_id"
        col_on <- "src_object_id"
        col_id <- "object_id"
    }

    cur_leaf <- rel[J(cur_depth, init_value_id), on = c("dep", col_init), nomatch = NULL]

    if (!nrow(cur_leaf)) return(cur_leaf)

    leaf <- cur_leaf

    cur_depth <- cur_depth + 1L
    while (cur_depth <= depth) {
        cur_leaf <- rel[J(cur_depth, unique(cur_leaf[[col_id]])),
            on = c("dep", col_on), nomatch = NULL]

        leaf <- data.table::rbindlist(list(leaf, cur_leaf))

        if (!nrow(cur_leaf)) break

        cur_depth <- cur_depth + 1L
    }

    leaf
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

# del_if_exist {{{
del_if_exist <- function(idf, id, ref_to = FALSE) {
    if (!length(id)) return(idf)
    id <- unique(id)
    id <- id[idf$is_valid_id(id)]
    if (!length(id)) return(idf)
    idf$del(id, .ref_to = ref_to)
}
# }}}
