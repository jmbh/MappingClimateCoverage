# jonashaslbeck@protonmail.com; Dec, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# This contains some meta settings used in plotting

# --------------------------------------------
# --------- Date Indicator -------------------
# --------------------------------------------

figV <- ""

# --------------------------------------------
# --------- Labels and Colors ----------------
# --------------------------------------------

cols <- c("#d62728", "#0072B2", "#029E73", "#E69F00", "#ff7f0e", "#CC79A7", "#56B4E9")
outlets = c("taz.de", "zeit.de", "spiegel.de", "sueddeutsche.de", "faz.net", "welt.de", "bild.de")
n_outlets <- length(outlets)

cols_wG <- c("#d62728", "#0072B2", "#029E73", "#E69F00", "#ff7f0e", "#CC79A7", "#56B4E9", "grey")
outlets_wG <- c("taz.de", "zeit.de", "spiegel.de", "sueddeutsche.de", "faz.net", "welt.de", "bild.de", "theguardian.com")
n_outlets_wG <- length(outlets_wG)

# Category names
v_cats <- c("Causes", "Impacts", "Mitigation", "Adaptation")
v_typea_names <- c("Report", "Interview", "Opinion Piece")



# --------------------------------------------------
# --------- Main Item Selection --------------------
# --------------------------------------------------

# Causes
causes <- c("causes_human",
            "causes_fossil_fuels",
            "causes_agriculture",
            "causes_overconsumption",
            "causes_economic_system",
            "causes_affluent_footprint",
            "causes_dev_countries_contrib")

causes_disp <- c("Human caused", 
                 "Fossil Fuels", 
                 "Agriculture & Diets",
                 "Overconsumption",
                 "Economic System",
                 "Unequal Footprint",
                 "Historic Contribution")

causes_disp_lb <- c("Human-\ncaused", 
                    "Fossil\nFuels", 
                    "Agriculture\n & Diets",
                    "Over-\n consumption",
                    "Economic\nSystem",
                    "Unequal\nFootprint",
                    "Historic\nContribution")

# Impact
impact <- c("impact_adverse",
            "impact_extreme_weather",
            "impact_sea_level_rise",
            "impact_biodiversity_loss",
            "impact_food_water_security",
            "impact_migration",
            "impact_science_threat")
impact_disp <- c("Adverse Impacts", 
                 "Extreme Weather", 
                 "Sea Level Rise", 
                 "Biodiversity Loss",
                 "Food & Water Security",
                 "Displacement & Migration",
                 "Worse than Expected")
impact_disp_lb <- c("Adverse\nImpacts", 
                    "Extreme\nWeather", 
                    "Sea Level\nRise", 
                    "Biodiversity\nLoss",
                    "Food &\nWater\nSecurity",
                    "Displacement\n& Migration",
                    "Worse\nthan\nExpected")

# Mitigation
mitigation <- c("mitigation_reduction_fossil",
                "mitigation_renewable_energy",
                "mitigation_electrification",
                "mitigation_diets_livestock",
                "mitigation_reduce_flying",
                "mitigation_carbon_tax",
                "mitigation_net_zero")
mitigation_disp <- c("Fossil Fuel Reduction",
                     "Renewable Energy",
                     "Electrification",
                     "Diets & Livestock",
                     "Reduce Flying",
                     "Carbon Tax",
                     "Net Zero")
mitigation_disp_lb <- c("Fossil Fuel\n Reduction",
                        "Renewable\nEnergy",
                        "Electri-\nfication",
                        "Diets &\nLivestock",
                        "Reduce\nFlying",
                        "Carbon Tax",
                        "Net Zero")

# Adaptation
adaptation <- c("adaptation_infrastructure_resilience",
                "adaptation_agriculture_resilience",
                "adaptation_ecosystem_restoration",
                "adaptation_public_health",
                "adaptation_disaster_preparedness",
                "adaptation_human_migration",
                "adaptation_countries")

adaptation_disp <- c("Infrastr. & Resilience",
                     "Agriculture Resilience",
                     "Ecosystem Restoration",
                     "Public Health",
                     "Disaster Preparedness",
                     "Human Migration",
                     "Help Poor Countries")
adaptation_disp_lb <- c("Infrastr. &\nResilience",
                        "Agriculture\nResilience",
                        "Ecosystem\n Restoration",
                        "Public\nHealth",
                        "Disaster\nPreparedness",
                        "Human\n Migration",
                        "Help Poor\nCountries")


l_labels_P <- list(causes, 
                   impact, 
                   mitigation, 
                   adaptation)

l_indP_lab <- list(causes_disp, 
                   impact_disp, 
                   mitigation_disp, 
                   adaptation_disp)

l_indP_lab_lb <- list(causes_disp_lb, 
                      impact_disp_lb, 
                      mitigation_disp_lb, 
                      adaptation_disp_lb)


