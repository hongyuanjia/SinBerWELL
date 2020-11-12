# get_aircon_zones {{{
get_aircon_zones <- function(idf) {
    # only for office areas
    zones <- idf$object_name("Zone", simplify = TRUE)
    grep("Stairs|Toilet|Plenum|Parking", zones, invert = TRUE, value = TRUE)
}
# }}}

# apply_combined_strategies {{{
apply_combined_strategies <- function (idf,
    indoor = 3, coil = 2, evaporator = 2, economizer = 2,
    sch_frac_occu = 0.4, sch_frac_equip = 0.7, sch_frac_light = 0.4,
    CO2_ctrl = TRUE, dayl_ctrl = TRUE, LED = TRUE, lowe = TRUE
)
{
    if (indoor > 0) {
        set_indoor_setpoint(idf, perimeter = 23 + indoor, core = 24 + indoor)
        add_ceiling_fan(idf)
    }
    if (coil > 0) set_coil_setpoint(idf, 12.8 + coil)
    if (evaporator > 0) set_evaporator_setpoint(idf, 6.7 + coil)
    if (economizer > 0) add_economizer(idf, 24 + economizer)

    if (sch_frac_occu < 1.0) set_sch_occupancy(idf, sch_frac_occu)
    if (sch_frac_equip < 1.0) set_sch_equipment(idf, sch_frac_equip)
    if (sch_frac_light < 1.0) set_sch_lighting(idf, sch_frac_light)

    if (CO2_ctrl) add_CO2_ctrlr(idf, source = 400, setpoint = 1115)

    if (LED) use_LED(idf)
    if (lowe) use_lowe_win(idf)

    if (dayl_ctrl) add_daylight_control(idf)

    idf
}
# }}}

# add_hybrid_ventilation {{{
add_hybrid_ventilation <- function(idf) {
    idf$add(Schedule_Constant = list("HybridVentType", "Any Number", 1))
    idf$add(Schedule_Constant = list("Always 0", "Any Number", 0))

    zones <- c("Core_Bot", "Core_Mid", "Core_Top")
    loops <- data.table::fcase(
        grepl("Bot", zones), "VAV_Bot",
        grepl("Mid", zones), "VAV_Mid",
        grepl("Top", zones), "VAV_Top"
    )

    idf$add(
        "ZoneVentilation:DesignFlowRate" := list(
            sprintf("MechVent_%s", zones),
            zones,
            "Sch_Occupancy",
            "AirChanges/Hour",
            air_changes_per_hour = 2,
            ventilation_type = "natural",
            fan_pressure_rise = 200,
            fan_total_efficiency = 0.61
        ),

        "AvailabilityManager:HybridVentilation" := list(
            sprintf("Hybrid Vent %s", zones),
            loops,
            zones, "HybridVentType",
            use_weather_file_rain_indicators = "no",
            maximum_wind_speed = 40,
            minimum_outdoor_temperature = 0,
            maximum_outdoor_temperature = 29,
            minimum_outdoor_enthalpy = 20000,
            maximum_outdoor_enthalpy = 30000,
            minimum_outdoor_ventilation_air_schedule_name = "Sch_Always_On",
            simple_airflow_control_type_schedule_name = "Always 0",
            zoneventilation_object_name = sprintf("MechVent_%s", zones)
        ),

        "Output:Variable" := list(
            "*",
            "Availability Manager Hybrid Ventilation Control Status",
            "Hourly"
        )
    )

    idf
}
# }}}

# set_indoor_setpoint {{{
set_indoor_setpoint <- function (idf, perimeter = 23, core = perimeter + 1,
                               ddy_perimeter = 23, ddy_core = ddy_perimeter + 1) {
    sch_peri <- "Sch_Zone_Cooling_Setpoint_Solar"
    sch_core <- "Sch_Zone_Cooling_Setpoint_Wo_Solar"

    if (ddy_perimeter == perimeter) {
        idf$object(sch_peri)$set(
            sch_peri, "Temperature", "Through: 12/31",
            "For: AllDays", "Until: 24:00", sprintf("%.1f", perimeter)
        )
    } else {
        idf$object(sch_peri)$set(
            sch_peri, "Temperature", "Through: 12/31",
            "For: SummerDesignDay", "Until: 24:00", sprintf("%.1f", ddy_perimeter),
            "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", perimeter)
        )
    }

    if (ddy_core == core) {
        idf$object(sch_core)$set(
            sch_core, "Temperature", "Through: 12/31",
            "For: AllDays", "Until: 24:00", sprintf("%.1f", core)
        )
    } else {
        idf$object(sch_core)$set(
            sch_core, "Temperature", "Through: 12/31",
            "For: SummerDesignDay", "Until: 24:00", sprintf("%.1f", ddy_core),
            "For: AllOtherDays", "Until: 24:00", sprintf("%.1f", core)
        )
    }

    idf
}
# }}}

# set_coil_setpoint {{{
set_coil_setpoint <- function (idf, setpoint = 12.8, ddy_setpoint = 12.8) {
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
set_evaporator_setpoint <- function (idf, setpoint = 6.7 + 3, ddy_setpoint = 6.7) {
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

# add_economizer {{{
add_economizer <- function (idf, setpoint = 24 + 3, ddy_setpoint = 24) {
    # set mixed air temperature setpoint schedule
    create_simple_sch(idf, "Sch_MAT", sprintf("%.1f", setpoint), sprintf("%.1f", ddy_setpoint))

    # add economizer
    idf$set("Controller:OutdoorAir" := list(
        Economizer_Control_Type = "FixedDryBulb",
        Economizer_Maximum_Limit_Dry_Bulb_Temperature = setpoint
    ))

    # create setpoint manager
    zn <- grep("core_(bot|mid|top)", idf$object_name("Zone", TRUE), value = TRUE, ignore.case = TRUE)
    node <- idf$to_table(class = "Controller:OutdoorAir", wide = TRUE)[["Mixed Air Node Name"]]
    l <- replicate(length(zn), idf$definition("SetpointManager:Scheduled")$to_table(), FALSE)
    dt <- data.table::rbindlist(lapply(seq_along(zn), function (i) {
        l[[i]][, value := c(
            name = paste0(zn[i], "_MAT_Setpoint"),
            control_variable = "Temperature",
            schedule_name = "Sch_MAT",
            setpoint_node_or_nodelist_name = node[i]
        )][, id := i]
    }))
    idf$load(dt)

    idf
}
# }}}

# add_CO2_ctrlr {{{
add_CO2_ctrlr <- function (idf, source = 400, setpoint = 1115) {
    # set CO2 source schedule
    # reference: ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt
    create_simple_sch(idf, "Sch_Outdoor_CO2", sprintf("%.1f", source), sprintf("%.1f", source))

    # add contaminant source
    if (idf$is_valid_class("ZoneAirContaminantBalance")) {
        idf$set("ZoneAirContaminantBalance" := list("Yes", "Sch_Outdoor_CO2", "No"))
    } else {
        idf$add("ZoneAirContaminantBalance" = list("Yes", "Sch_Outdoor_CO2", "No"))
    }

    # set CO2 setpoint schedule
    create_simple_sch(idf, "Sch_CO2_Setpoint", sprintf("%.1f", setpoint), sprintf("%.1f", setpoint))

    # add contaminant controller
    zn <- grep("core_(bot|mid|top)", idf$object_name("Zone", TRUE), value = TRUE, ignore.case = TRUE)
    l <- replicate(length(zn), idf$definition("ZoneControl:ContaminantController")$to_table(), FALSE)
    dt <- data.table::rbindlist(lapply(seq_along(zn), function (i) {
        l[[i]][, value := c(
            name = paste0(zn[i], "_CO2_Controller"),
            zone_name = zn[i],
            carbon_dioxide_control_availability_schedule_name = "Sch_ACMV",
            carbon_dioxide_setpoint_schedule_name = "Sch_CO2_Setpoint"
        )][, id := i]
    }))
    eplusr::with_silent(idf$load(dt))

    # change mechanical ventilation control methods
    idf$set("Controller:MechanicalVentilation" := list(system_outdoor_air_method = "IndoorAirQualityProcedure"))

    idf
}
# }}}

# use_lowe_win {{{
use_lowe_win <- function (idf) {
    idf$set("FenestrationSurface:Detailed" := list(construction_name = "SGP_DoublepaneWindow"))
    idf
}
# }}}

# add_daylight_control {{{
add_daylight_control <- function (idf, illu_setpoint = 500, glare_setpoint = 19,
                                  dist_from_wall = 1.6764, dist_from_floor = 0.762) {
    create_simple_sch(idf, "Sch_Daylight_Control_Availability", 1, 0)

    # get azimuth of exterior window
    win <- idf$to_table(class = "FenestrationSurface:Detailed", wide = TRUE, string_value = FALSE)
    vert_win <- get_outward_normal(get_vertices(win))[, azimuth := get_azimuth(c(x, y, z)), by = "id"]
    win[vert_win, on = "id", azimuth := i.azimuth]
    data.table::set(win, NULL, "Building Surface Name", toupper(win[["Building Surface Name"]]))

    # get all exterior walls with windows
    surf <- idf$to_table(win[["Building Surface Name"]], wide = TRUE, string_value = FALSE)
    vert_surf <- get_vertices(surf)[, lapply(.SD, list), by = "id"]
    surf[vert_surf, on = "id", `:=`(X = i.x, Y = i.y, Z = i.z)]
    data.table::set(surf, NULL, "name", toupper(surf$name))

    # get azimuth
    surf[win, on = c(name = "Building Surface Name"), azimuth := i.azimuth]

    # add reference point
    pnt <- surf[, by = c("id", "azimuth"), c("ref_X", "ref_Y", "ref_Z") := {
        # north
        if (azimuth == 0) {
            x <- mean(range(X[[1L]]))
            y <- Y[[1L]][[1L]] - dist_from_wall
        # east
        } else if (azimuth == 90) {
            x <- X[[1L]][[1L]] - dist_from_wall
            y <- mean(range(Y[[1L]]))
        # south
        } else if (azimuth == 180) {
            x <- mean(range(X[[1L]]))
            y <- Y[[1L]][[1L]] + dist_from_wall
        # north
        } else {
            x <- X[[1L]][[1L]] + dist_from_wall
            y <- mean(range(Y[[1L]]))
        }

        z <- min(Z[[1L]]) + dist_from_floor

        list(x, y, z)
    }][, list(id, class = "Daylighting:ReferencePoint", Name = paste0(`Zone Name`, "_DaylRefPt1"),
        `Zone Name`, ref_X, ref_Y, ref_Z
    )]
    data.table::setnames(pnt, sprintf("ref_%s", c("X", "Y", "Z")),
        sprintf("%s-Coordinate of Reference Point", c("X", "Y", "Z"))
    )
    idf$load(eplusr::dt_to_load(pnt))

    # add daylighting control
    l <- replicate(nrow(pnt), idf$definition("Daylighting:Controls")$to_table(), FALSE)
    for (i in seq_len(nrow(pnt))) data.table::set(l[[i]], NULL, "id", i)
    dt <- data.table::rbindlist(l)
    dt[J(1L), on = "index", value := paste0(pnt[["Zone Name"]], "_DaylCtrl")]
    dt[J(2L), on = "index", value := pnt[["Zone Name"]]]
    dt[J(10L), on = "index", value := pnt[["Name"]]]
    dt[J(14L), on = "index", value := pnt[["Name"]]]
    dt[J(c(3:9)), on = "index", by = "id", value := c(
        "SplitFlux", "Sch_Daylight_Control_Availability", "Continuous",
        "0.3", "0.2", 1, 1
    )]
    dt[J(11L), on = "index", value := "90"]
    dt[J(12L), on = "index", value := as.character(glare_setpoint)]
    dt[J(16L), on = "index", value := as.character(illu_setpoint)]
    idf$load(dt)

    idf
}
# }}}

# create_simple_sch {{{
create_simple_sch <- function (idf, name, value, ddy_value) {
    if (idf$is_valid_name(name)) eplusr::with_silent(idf$del(name, .force = TRUE))
    idf$add("Schedule:Compact" = list(
        name, "Any Number", "Through: 12/31",
        "For: SummerDesignDay", "Until: 24:00", as.character(ddy_value),
        "For: AllOtherDays", "Until: 24:00", as.character(value)
    ))[[1L]]
}
# }}}

# set_sch_occupancy {{{
set_sch_occupancy <- function (idf, frac = 1.0) {
    apply_sch_frac(idf, "Sch_Occupancy", frac)
}
# }}}

# set_sch_equipment {{{
set_sch_equipment <- function (idf, frac = 1.0) {
    apply_sch_frac(idf, "Sch_Equipment", frac)
}
# }}}

# set_sch_lighting {{{
set_sch_lighting <- function (idf, frac = 1.0) {
    apply_sch_frac(idf, "Sch_Lighting_Offices&Toilets", frac)
}
# }}}

# apply_sch_frac {{{
apply_sch_frac <- function (idf, sch, frac = 1.0) {
    dt <- idf$"Schedule:Compact"[[sch]]$to_table()
    i <- dt[grepl("Weekdays", value), which = TRUE]
    val <- dt[index >= (i + 2) & (index - (i + 2)) %% 2 == 0][1:24]
    val[, value := sprintf("%.3f", as.double(value) * frac)]
    idf$update(val)
    idf
}
# }}}

# use_LED {{{
use_LED <- function (idf) {
    set_lpd(idf, watts_per_area = 5, frac_rad = 0, frac_vis = 0.85)

    # assume all lights with LPD <= 5W/m2 are LED
    dt <- idf$to_table(class = "Lights", wide = TRUE, string_value = FALSE)

    dt <- dt[tolower(`Design Level Calculation Method`) == "watts/area" & `Watts per Zone Floor Area` <= 5][
        , `:=`(`Fraction Radiant` = 0, `Fraction Visible` = 0.85)
    ]

    idf$update(eplusr::dt_to_load(dt))
    idf
}
# }}}

# set_lpd {{{
set_lpd <- function (idf, watts_per_area = 14, frac_rad = NA, frac_vis = NA) {
    z <- grep("Plenum|Parking", idf$object_name("Zone")$Zone, invert = TRUE, value = TRUE)

    id_lt <- vapply(z, function (zn) idf$Zone[[zn]]$ref_by_object(class = "Lights")[[1]]$id(), integer(1))

    idf$set(.(id_lt) := list(
        Design_Level_Calculation_Method = "Watts/Area",
        Watts_per_Zone_Floor_Area = watts_per_area
    ))

    if (!is.na(frac_rad)) idf$set(.(id_lt) := list(Fraction_Radiant = frac_rad))
    if (!is.na(frac_vis)) idf$set(.(id_lt) := list(Fraction_Visible = frac_vis))

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

# read_comfort_not_met {{{
read_comfort_not_met <- function(job) {
    checkmate::assert_r6(job)
    checkmate::assert_multi_class(job, c("EplusSql", "EplusJob", "EplusGroupJob"))

    # read comfort and setpoint not met summary
    dt <- job$tabular_data(
        report_name = "AnnualBuildingUtilityPerformanceSummary",
        table_name = "Comfort and Setpoint Not Met Summary",
        wide = TRUE
    )[[1L]]

    data.table::set(dt, NULL, c("report_name", "report_for", "table_name"), NULL)
    data.table::setnames(dt, "row_name", "type")
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

# get_vertices {{{
get_vertices <- function (dt, keep = NULL) {
    dt <- data.table::melt.data.table(dt, c("id", keep),
        patterns("X-coordinate", "Y-coordinate", "Z-coordinate"),
        value.name = c("x", "y", "z")
    )
    data.table::set(dt, NULL, "variable", NULL)
    # retain the original order
    data.table::setorderv(dt, "id")
}
# }}}
# align_face {{{
align_face <- function (dt) {
    norm <- get_outward_normal(dt)
    dt[norm, on = "id", by = .EACHI, {
        align <- align_z_prime(i.x, i.y, i.z)

        # get aligned vertices
        align_inv <- solve(align)
        align_vert <- apply(matrix(c(x, y, z, rep(1.0, .N)), ncol = 4L), 1, function (x) align_inv %*% x)[1:3,]

        # compute translation to minimum in aligned system
        trans <- diag(nrow = 4L)
        trans[1:3, 4L] <- apply(align_vert, 1, min)
        list(trans = list(align %*% trans))
    }]
}
# }}}
# get_outward_normal {{{
get_outward_normal <- function (dt) {
    # calculate normal vector of surfaces using Newell Method
    # Reference: https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method
    dt[, by = "id", {
        nx <- seq_len(.N) %% .N + 1L
        # calculate the distance from the origin to the first point on each polygon
        norm <- normalize(c(
            x = sum((z + z[nx]) * (y - y[nx])),
            y = sum((x + x[nx]) * (z - z[nx])),
            z = sum((y + y[nx]) * (x - x[nx]))
        ))
        data.table::setattr(as.list(norm), "names", c("x", "y", "z"))
    }]
}
# }}}
# align_z_prime {{{
align_z_prime <- function (x, y, z) {
    axis_x <- c(1, 0, 0)
    axis_y <- c(0, 1, 0)
    axis_z <- c(0, 0, 1)
    axis_x_neg <- c(-1, 0, 0)

    zp <- normalize(c(x, y, z))

    dot_zp <- as.numeric(zp %*% axis_z)

    if (abs(dot_zp) < 0.99) {
        yp <- normalize(axis_z - as.numeric(zp %*% axis_z) * zp)
        xp <- crossproduct(yp, zp)
    } else {
        xp <- normalize(axis_x_neg - as.numeric(zp %*% axis_x_neg) * zp)
        yp <- crossproduct(zp, xp)
    }

    trans <- diag(nrow = 4L)
    trans[1:3, 1] <- xp
    trans[1:3, 2] <- yp
    trans[1:3, 3] <- zp
    trans
}
# }}}
# crossproduct {{{
crossproduct <- function(v1, v2) {
    v1[c(2L, 3L, 1L)] * v2[c(3L, 1L, 2L)] - v1[c(3L, 1L, 2L)] * v2[c(2L, 3L, 1L)]
}
# }}}
# normalize {{{
normalize <- function(v) v / sqrt(sum(v^2)) 
# }}}
# deg_to_rad {{{
deg_to_rad <- function (x) x / 180 * pi
# }}}
# rad_to_deg {{{
rad_to_deg <- function (x) x / pi * 180
# }}}
# get_angle {{{
get_angle <- function (v1, v2) {
    normalize(v1) %*% normalize(v2)
    d <- rad_to_deg(acos(normalize(v1) %*% normalize(v2)))[1]
    if (v1[[1]] < 0) d <- d + 180
    d
}
# }}}
# get_tilt {{{
get_tilt <- function (out_norm) {
    get_angle(out_norm, c(0, 0, 1))
}
# }}}
# get_azimuth {{{
get_azimuth <- function (out_norm) {
    get_angle(out_norm, c(0, 1, 0))
}
# }}}
