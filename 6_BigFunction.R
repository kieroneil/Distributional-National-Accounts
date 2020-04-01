
## ---------------------------------------------------------------------------------------------------------------------
#The `format_dina()` function takes the files in the `Dina_subset` and does the following to them
# 1. Loads them
# 2. Renames the variables and factor levels
# 3. Reconciles the aggregated elements of the data
# 4. Creates proportions
# 5. Creates distributions

# Returns a single data frame


## ---------------------------------------------------------------------------------------------------------------------
# This is The Big Function
# It assumes that the files for the years you want are in the `Dina_subset` folder
format_dina <- function(path = "Data/Dina_subset/") {
  library(tidyverse)
  library(fs)
  library(haven)
  
  # Get the paths for your subset of files. From the `fs` package
  paths <- dir_ls(path) 
  #files
  
  # Import all of the Stata dta files into a single dataframe
  # Also, put the year from the filename into a new column
  dina_df <- map_dfr(paths, ~ read_dta(.x), .id = "filename") %>%
    extract(filename, "year", "(\\d{4})")
  
  # Get the names of all the grouping variables
  group_names <- names(dina_df[5:15])
  
  # Change factor levels to decriptive labels 
  # Assign to new vars and then drop originals and put at front
  dina_df2 <- dina_df %>%
    #filter(year == 2018) %>%
    mutate_at(group_names, as.character) %>%
    mutate(gender = if_else(female == "1", "Female", "Male", "Unknown"),
           agegroup_primary = recode(ageprim, "0" = "20-64", "20" = "20-44", 
                                     "45" = "45-64", "65" = "65 Plus"),
           agegroup_secondary = recode(agesec, "0" = "20-64", "20" = "20-44", 
                                       "45" = "45-64", "65" = "65 Plus"), 
           agegroup_imputed = recode(age, "0" = "20-64", "20" = "20-44", 
                                     "45" = "45-64", "65" = "65 Plus"), 
           labor_status_primary = if_else(oldexm == "1", "Retired", "Working", "Unknown"),
           labor_status_secondary = if_else(oldexf == "1", "Retired", "Working", "Unknown"),
           labor_status_imputed = if_else(oldexf == "1", "Retired", "Working", "Unknown"), 
           filing_status = if_else(married == "1", "Married", "Single", "Unknown"), 
           earner_status = if_else(second == "0", "Primary", "Secondary", "Unknown"), 
           num_kids = xkidspop, 
           filer_status = if_else(filer == "1", "Filer", "Not filer", "Unknown")
    ) %>%
    select(-all_of(group_names)) %>%
    select(year, everything())
  
  # Get the names of new grouping variables
  grouping_vars <- names(dina_df2[136:146])
  
  # Give columns more descriptive names
  # I try to use consistent names so that you will be able to efficiently select 
  #  column names with `contains()`.
  dina_df3 <- dina_df2 %>%
    #select(id, group_names, everything()) %>%
    mutate_at(grouping_vars, as.factor) %>%
    rename(
      # Socio-demographic
      tax_unit_id = id,
      population_weight = dweght,
      population_weight_ptu = dweghttaxu,
      
      # Core Income & Wealth
      ttl_income_fiscal_excl_capgains = fiinc,
      income_fiscal_wages_pensions = fiwag,
      income_fiscal_business = fibus,
      income_fiscal_rents = firen,
      income_fiscal_interest = fiint,
      income_fiscal_dividends = fidiv,
      income_fiscal_nonfiler_default = fnps,
      
      ttl_income_fiscal_incl_capgains = fninc,
      income_fiscal_capital_gains = fikgi,
      
      ttl_income_factor = fainc,
      ttl_income_factor_labor = flinc,
      income_factor_labor_wages = flemp,
      income_factor_labor_mixed = flmil,
      income_factor_labor_sales_taxes = flprl,
      ttl_income_factor_capital = fkinc,
      income_factor_capital_housing = fkhou,
      income_factor_capital_equity = fkequ,
      income_factor_capital_interest = fkfix,
      income_factor_capital_business = fkbus,
      income_factor_capital_pension_benefits = fkpen,
      
      ttl_income_pretax_labor = plinc,
      ttl_contributions_social_insurance = plcon,
      contributions_social_insurance_pensions = plpco,
      income_social_share_labor = plbel,
      ttl_income_pretax_capital = pkinc,
      income_investment_payable_pensions = pkpen,
      income_social_share_capital = pkbek,
      
      ttl_income_disposable_extended = diinc,
      income_cash_disposable = dicsh,
      income_social_inkind_transfers = inkindinc, 
      ttl_income_social_collective = colexp,
      income_social_collective_property_paid_by_govt = govin, 
      income_social_collective_non_profit = npinc, 
      income_social_collective_education = educ, 
      
      ttl_income_national_factor = princ,
      
      ttl_income_national_pretax = peinc,
      ttl_income_pretax = ptinc,
      surplus_primary_public_pension_system = prisupen,
      income_investment_pensions_payable = invpen,
      
      ttl_income_national_posttax = poinc,
      surplus_primary_private_pension_system = prisupenprivate,
      surplus_primary_government = prisupgov,
      
      ttl_wealth_net = hweal,
      assets_equity = hwequ,
      assets_currency = hwfix,
      assets_housing = hwhou,
      assets_business = hwbus,
      assets_pension_lifeins = hwpen,
      liabilities_household = hwdeb,
      
      # Detailed income & wealth
      ttl_income_national_posttax_2 = poinc2,
      ttl_income_social_benefits = ben,
      ttl_income_social_othercash = dicao,
      ttl_income_social_inkind = otherkin,
      ttl_income_social_share = plben,
      ttl_income_social_collective_2 = colexp2,
      ttl_taxes_payments_contributions = tax,
      
      ttl_income_pretax_pension = ptnin, 
      income_pretax_pension_labor = plnin,
      income_pretax_pension_capital = pknin,
      
      ttl_contributions_social_insurance_govt = govcontrib, 
      contributions_social_insurance_govt_pensions_ui_di = ssuicontrib,
      contributions_social_insurance_govt_other = othercontrib,
      
      income_pension_taxable = peninc,
      income_schedule_net = schcinc,
      income_s_corp_net = scorinc,
      income_partnership_net = partinc,
      income_rental_net = rentinc,
      income_estate_trust_net = estinc,
      income_royalty_net = rylinc,
      income_other_in_agi = othinc,
      income_capital_main_house_asset = fkhoumain,
      income_capital_rental_house = fkhourent,
      income_social_insurance_retirement = ssinc_oa,
      contributions_social_pension = wagpen,
      income_social_insurance_disability = ssinc_di,
      contributions_social_insurance_di_ui = ploco,
      income_social_insurance_unemployment = uiinc,
      income_social_cash_supplemental = disup,
      income_social_cash = dicab, 
      income_social_cash_veterans = divet,
      income_social_taxcredit = dicred,
      income_social_othercash_tanf = tanfinc,
      income_social_othercash_cashlocalstate = othben,
      income_social_health_medicare = medicare,
      income_social_health_medicaid = medicaid,
      contributions_social_health_wages = waghealth,
      income_social_inkind_pell = pell,
      income_social_inkind_vethealth = vethealth,
      
      wages_all_filers_taxable = flwag,
      wages_all_filers_taxable_supplements = flsup,
      
      benefits_pension = plpbe,
      benefits_pension_capital_share = pkpbk, 
      benefits_pension_labor_share = plpbl,
      benefits_di_ui = plobe,
      
      taxes_capital_sales_excise = fkprk,
      taxes_property_housing = proprestax,
      taxes_property_business = propbustax,
      taxes_income_wealth_current = ditax,
      taxes_federal_income = ditaf,
      taxes_state_income = ditas,
      taxes_sales_excise = salestax, 
      taxes_corporate = corptax,
      taxes_estate = estatetax,
      
      payments_interest = fkdeb,
      payments_interest_mortgage = fkmor,
      payments_interest_nonmortgage = fknmo,
      
      ttl_wealth_personal_net = hwealnokg,
      wealth_rental_housing_gross = rentalhome,
      wealth_rental_housing_mortgages = rentalmort,
      wealth_rental_housing_net = rental,
      wealth_main_housing_gross = ownerhome,
      wealth_main_housing_mortgages = ownermort, 
      wealth_main_housing_net = housing,
      wealth_partnership = partw, 
      wealth_sole_proprietor = soleprop, 
      wealth_s_corp = scorw,
      wealth_equity = equity, 
      wealth_taxable_bond = taxbond, 
      wealth_muni_bond = muni,
      wealth_currency = currency, 
      wealth_non_mortgage_debt = nonmort,
      wealth_household_financial_assets = hwfin,
      wealth_household_nonfinancial_assets = hwnfa
    ) 
  
  #==========================================================
  # Reconciliation and stuff
  # * For some reason the population is multiplied by 100,000. I divide by 100,000 so that the 
  #     sum of the population weights in any given year will equal the actual adult population
  #     size in the year.
  dina_df4 <- dina_df3 %>%
    select(tax_unit_id, grouping_vars, everything()) %>%
    mutate(
      year = factor(year),
      population_weight = population_weight / 100000,
      population_weight_ptu = population_weight_ptu / 100000)
  
  #saveRDS(dina_df4, "temp/Dina_df4.RDS")
  #dina_df4 <- readRDS("Dina_df4.RDS")
  
  dina_df5 <- dina_df4 %>%
    mutate(
      # Core wealth & income aggregates
      summ_income_factor_labor = 
        income_factor_labor_wages + 
        income_factor_labor_mixed +
        income_factor_labor_sales_taxes, #flemp + flmil + flprl
      recon_income_factor_labor = 
        ttl_income_factor_labor - summ_income_factor_labor,
      
      summ_income_factor_capital = 
        income_factor_capital_housing +
        income_factor_capital_equity +
        income_factor_capital_interest +
        income_factor_capital_business +
        income_factor_capital_pension_benefits +
        payments_interest, #fkhou + fkequ + fkfix + fkbus + fkpen + fkdeb
      recon_income_factor_capital = 
        ttl_income_factor_capital - summ_income_factor_capital,
      
      summ_income_factor = 
        summ_income_factor_labor +
        summ_income_factor_capital, #flinc + fkinc
      recon_income_factor = 
        ttl_income_factor - summ_income_factor,
      
      summ_contributions_social_insurance = 
        contributions_social_insurance_pensions +
        contributions_social_insurance_di_ui,
      recon_contributions_social_insurance = 
        ttl_contributions_social_insurance - summ_contributions_social_insurance,
      
      summ_income_pretax_labor = 
        summ_income_factor_labor +
        summ_contributions_social_insurance +
        income_social_share_labor, #flinc + plcon + plbel
      recon_income_pretax_labor = 
        ttl_income_pretax_labor - summ_income_pretax_labor,
      
      summ_income_pretax_capital = 
        summ_income_factor_capital +
        income_investment_payable_pensions +
        income_social_share_capital, #fkinc + pkpen + pkbek
      recon_income_pretax_capital = 
        ttl_income_pretax_capital - summ_income_pretax_capital,
      
      summ_ttl_income_pretax = 
        summ_income_pretax_labor +
        summ_income_pretax_capital, #plinc + pkinc
      recon_ttl_income_pretax = 
        ttl_income_pretax - summ_ttl_income_pretax,
      
      summ_income_fiscal_incl_capgains = 
        income_fiscal_wages_pensions +
        income_fiscal_business + 
        income_fiscal_rents + 
        income_fiscal_interest +
        income_fiscal_dividends, #fiwag + fibus + firen + fiint + fidiv
      recon_income_fiscal_incl_capgains = 
        ttl_income_fiscal_incl_capgains - summ_income_fiscal_incl_capgains,
      
      summ_income_fiscal_excl_capgains = 
        income_fiscal_wages_pensions +
        income_fiscal_business + 
        income_fiscal_rents + 
        income_fiscal_interest +
        income_fiscal_dividends + 
        income_fiscal_capital_gains, #fiwag + fibus + firen + fiint + fidiv + fikgi
      recon_income_fiscal_excl_capgains = 
        ttl_income_fiscal_excl_capgains - summ_income_fiscal_excl_capgains,
      
      summ_income_disposable_extended = 
        income_cash_disposable +
        income_social_inkind_transfers +
        ttl_income_social_collective, #dicsh + inkindinc + colexp
      recon_income_disposable_extended = 
        ttl_income_disposable_extended - summ_income_disposable_extended,
      
      summ_income_national_factor = 
        summ_income_factor + 
        income_social_collective_property_paid_by_govt +
        income_social_collective_non_profit, #fainc + govin + npinc
      recon_income_national_factor = 
        ttl_income_national_factor - summ_income_national_factor,
      
      summ_income_national_pretax = 
        summ_ttl_income_pretax +
        income_social_collective_property_paid_by_govt +
        income_social_collective_non_profit +
        surplus_primary_public_pension_system +
        income_investment_pensions_payable, #ptinc + govin + npinc + prisupen + invpen
      recon_income_national_pretax = 
        ttl_income_national_pretax - summ_income_national_pretax,
      
      summ_income_national_posttax = 
        summ_income_disposable_extended +
        income_social_collective_property_paid_by_govt +
        income_social_collective_non_profit +
        surplus_primary_private_pension_system +
        income_investment_pensions_payable +
        surplus_primary_government, # diinc + govin + npinc + prisupenprivate + invpen + prisupgov
      recon_income_national_posttax =
        ttl_income_national_posttax - summ_income_national_posttax,
      
      summ_wealth_net = 
        assets_equity +
        assets_currency +
        assets_housing +
        assets_business +
        assets_pension_lifeins +
        liabilities_household, #hwequ + hwfix + hwhou + hwbus + hwpen + hwdeb
      recon_wealth_net = 
        ttl_wealth_net - summ_wealth_net,
      
      # Detailed wealth & income aggregates
      summ_income_social_inkind = 
        income_social_inkind_pell +
        income_social_inkind_vethealth,
      recon_income_social_inkind = 
        ttl_income_social_inkind - summ_income_social_inkind,
      
      summ_income_social_othercash = 
        income_social_othercash_tanf +
        income_social_othercash_cashlocalstate,
      recon_income_social_othercash = 
        ttl_income_social_othercash - summ_income_social_othercash,
      
      summ_income_social_share = 
        income_social_share_labor +
        income_social_share_capital,
      recon_income_social_share = 
        ttl_income_social_share - summ_income_social_share,
      
      summ_income_social_collective = 
        income_social_collective_property_paid_by_govt + 
        income_social_collective_non_profit +
        income_social_collective_education, 
      recon_income_social_collective = 
        ttl_income_social_collective - summ_income_social_collective,
      recon_income_social_collective_2 = 
        ttl_income_social_collective_2 - summ_income_social_collective,
      adj_income_social_collective = 
        ttl_income_social_collective - 
        summ_income_social_collective,
      recon_adj_income_social_collective = 
        ttl_income_social_collective - 
        summ_income_social_collective + 
        adj_income_social_collective,
      
      summ_contributions_social_insurance_govt =
        contributions_social_insurance_govt_pensions_ui_di +
        contributions_social_insurance_govt_other,
      recon_contributions_social_insurance_govt = 
        ttl_contributions_social_insurance_govt - summ_contributions_social_insurance_govt,
      
      summ_taxes_paid = 
        taxes_capital_sales_excise +
        taxes_property_housing +
        taxes_property_business +
        taxes_income_wealth_current +
        taxes_federal_income +
        taxes_state_income +
        taxes_sales_excise +
        taxes_corporate +
        taxes_estate,
      
      summ_taxes_effective_tax_rate = if_else(
        ttl_income_national_pretax != 0,
        summ_taxes_paid / ttl_income_national_pretax, 0),
      
      summ_taxes_cohort_income_prepost_tax_rate = if_else(
        summ_income_national_pretax != 0, 
        1 - (summ_income_national_posttax / summ_income_national_pretax), 
        0),
      
      summ_cohort_national_income_pretax = ttl_income_national_pretax * population_weight,
      summ_cohort_national_income_posttax = ttl_income_national_posttax * population_weight,
      summ_cohort_wealth_net = ttl_wealth_net * population_weight
    )
  
  #====================================
  # Create proportions
  
  dina_df6 <- dina_df5 %>% 
    mutate( # ttl_income_factor_labor
      prop_income_factor_labor_wages = if_else(ttl_income_factor_labor != 0, 
                                               income_factor_labor_wages / ttl_income_factor_labor, 0),
      prop_income_factor_labor_mixed = if_else(ttl_income_factor_labor != 0,
                                               income_factor_labor_mixed / ttl_income_factor_labor, 0),
      prop_income_factor_labor_sales_taxes = if_else(ttl_income_factor_labor != 0,
                                                     income_factor_labor_sales_taxes / ttl_income_factor_labor, 0),
      recon_prop_ttl_income_factor_labor = 
        prop_income_factor_labor_wages +
        prop_income_factor_labor_mixed +
        prop_income_factor_labor_sales_taxes
    ) %>%
    mutate( # ttl_income_factor_capital
      prop_income_factor_capital_housing = if_else(ttl_income_factor_capital != 0, 
                                                   income_factor_capital_housing / ttl_income_factor_capital, 0),
      prop_income_factor_capital_equity = if_else(ttl_income_factor_capital != 0,
                                                  income_factor_capital_equity / ttl_income_factor_capital, 0),
      prop_income_factor_capital_interest = if_else(ttl_income_factor_capital != 0,
                                                    income_factor_capital_interest / ttl_income_factor_capital, 0),
      prop_income_factor_capital_business = if_else(ttl_income_factor_capital != 0,
                                                    income_factor_capital_business / ttl_income_factor_capital, 0),
      prop_income_factor_capital_pension_benefits = if_else(ttl_income_factor_capital != 0,
                                                            income_factor_capital_pension_benefits / ttl_income_factor_capital, 0),
      prop_payments_interest = if_else(ttl_income_factor_capital != 0,
                                       payments_interest / ttl_income_factor_capital, 0),
      recon_prop_ttl_income_factor_capital = 
        prop_income_factor_capital_housing +
        prop_income_factor_capital_equity +
        prop_income_factor_capital_interest +
        prop_income_factor_capital_business +
        prop_income_factor_capital_pension_benefits +
        prop_payments_interest
    ) %>%
    mutate( # ttl_income_factor
      prop_ttl_income_factor_labor_if = if_else(ttl_income_factor != 0, 
                                                ttl_income_factor_labor / ttl_income_factor, 0),
      prop_ttl_income_factor_capital_if = if_else(ttl_income_factor != 0,
                                                  ttl_income_factor_capital / ttl_income_factor, 0),
      recon_prop_ttl_income_factor = 
        prop_ttl_income_factor_labor_if +
        prop_ttl_income_factor_capital_if
    ) %>%
    mutate( # ttl_contributions_social_insurance
      prop_contributions_social_insurance_pensions = if_else(ttl_contributions_social_insurance != 0, 
                                                             contributions_social_insurance_pensions / ttl_contributions_social_insurance, 0),
      prop_contributions_social_insurance_di_ui = if_else(ttl_contributions_social_insurance != 0,
                                                          contributions_social_insurance_di_ui / ttl_contributions_social_insurance, 0), 
      recon_prop_ttl_contributions_social_insurance = 
        prop_contributions_social_insurance_pensions +
        prop_contributions_social_insurance_di_ui
    ) %>%
    mutate( # ttl_income_pretax_labor
      prop_ttl_income_factor_labor_ilpr = if_else(ttl_income_pretax_labor != 0, 
                                                  ttl_income_factor_labor / ttl_income_pretax_labor, 0),
      prop_ttl_contributions_social_insurance = if_else(ttl_income_pretax_labor != 0,
                                                        ttl_contributions_social_insurance / ttl_income_pretax_labor, 0),
      prop_income_social_share_labor = if_else(ttl_income_pretax_labor != 0,
                                               income_social_share_labor / ttl_income_pretax_labor, 0),
      recon_prop_ttl_income_pretax_labor = 
        prop_ttl_income_factor_labor_ilpr +
        prop_ttl_contributions_social_insurance +
        prop_income_social_share_labor
    ) %>%
    mutate( # ttl_income_pretax_capital
      prop_ttl_income_factor_capital_icpr = if_else(ttl_income_pretax_capital != 0, 
                                                    ttl_income_factor_capital / ttl_income_pretax_capital, 0),
      prop_income_investment_payable_pensions = if_else(ttl_income_pretax_capital != 0,
                                                        income_investment_payable_pensions / ttl_income_pretax_capital, 0),
      prop_income_social_share_capital = if_else(ttl_income_pretax_capital != 0,
                                                 income_social_share_capital / ttl_income_pretax_capital, 0),
      recon_prop_ttl_income_pretax_capital = 
        prop_ttl_income_factor_capital_icpr +
        prop_income_investment_payable_pensions +
        prop_income_social_share_capital
    ) %>%
    mutate( # ttl_income_pretax
      prop_ttl_income_pretax_labor = if_else(ttl_income_pretax != 0, 
                                             ttl_income_pretax_labor / ttl_income_pretax, 0),
      prop_ttl_income_pretax_capital = if_else(ttl_income_pretax != 0,
                                               ttl_income_pretax_capital / ttl_income_pretax, 0), 
      recon_prop_ttl_income_pretax = 
        prop_ttl_income_pretax_labor +
        prop_ttl_income_pretax_capital
    ) %>%
    mutate( # ttl_income_national_factor
      prop_ttl_income_factor_inf = if_else(ttl_income_national_factor != 0, 
                                           ttl_income_factor / ttl_income_national_factor, 0),
      prop_income_social_collective_property_paid_by_govt = if_else(ttl_income_national_factor != 0,
                                                                    income_social_collective_property_paid_by_govt / ttl_income_national_factor, 0),
      prop_income_social_collective_non_profit = if_else(ttl_income_national_factor != 0,
                                                         income_social_collective_non_profit / ttl_income_national_factor, 0),
      recon_prop_ttl_income_national_factor = 
        prop_ttl_income_factor_inf +
        prop_income_social_collective_property_paid_by_govt +
        prop_income_social_collective_non_profit
    ) %>%
    mutate( # ttl_income_national_pretax
      prop_ttl_income_pretax = if_else(ttl_income_national_pretax != 0, 
                                       ttl_income_pretax / ttl_income_national_pretax, 0),
      prop_income_social_collective_property_paid_by_govt = if_else(ttl_income_national_pretax != 0,
                                                                    income_social_collective_property_paid_by_govt / ttl_income_national_pretax, 0),
      prop_income_social_collective_non_profit = if_else(ttl_income_national_pretax != 0,
                                                         income_social_collective_non_profit / ttl_income_national_pretax, 0),
      prop_surplus_primary_public_pension_system = if_else(ttl_income_national_pretax != 0,
                                                           surplus_primary_public_pension_system / ttl_income_national_pretax, 0),
      prop_income_investment_pensions_payable = if_else(ttl_income_national_pretax != 0,
                                                        income_investment_pensions_payable / ttl_income_national_pretax, 0),
      recon_prop_ttl_income_national_pretax = 
        prop_ttl_income_pretax +
        prop_income_social_collective_property_paid_by_govt +
        prop_income_social_collective_non_profit +
        prop_surplus_primary_public_pension_system +
        prop_income_investment_pensions_payable
    ) %>%
    mutate( # ttl_income_national_posttax
      prop_ttl_income_disposable_extended_inpo = if_else(ttl_income_national_posttax != 0, 
                                                         ttl_income_disposable_extended / ttl_income_national_posttax, 0),
      prop_income_social_collective_property_paid_by_govt_inpo = if_else(ttl_income_national_posttax != 0,
                                                                         income_social_collective_property_paid_by_govt / ttl_income_national_posttax, 0),
      prop_income_social_collective_non_profit_inpo = if_else(ttl_income_national_posttax != 0,
                                                              income_social_collective_non_profit / ttl_income_national_posttax, 0),
      prop_surplus_primary_public_pension_system_inpo = if_else(ttl_income_national_posttax != 0,
                                                                surplus_primary_public_pension_system / ttl_income_national_posttax, 0),
      prop_income_investment_pensions_payable_inpo = if_else(ttl_income_national_posttax != 0,
                                                             income_investment_pensions_payable / ttl_income_national_posttax, 0),
      prop_surplus_primary_government_inpo = if_else(ttl_income_national_posttax != 0,
                                                     surplus_primary_government / ttl_income_national_posttax, 0),
      recon_prop_ttl_income_national_posttax = 
        prop_ttl_income_disposable_extended_inpo +
        prop_income_social_collective_property_paid_by_govt_inpo +
        prop_income_social_collective_non_profit_inpo +
        prop_surplus_primary_public_pension_system_inpo +
        prop_income_investment_pensions_payable_inpo + 
        prop_surplus_primary_government_inpo
    ) %>%
    mutate( # ttl_wealth_net
      prop_assets_equity = if_else(ttl_wealth_net != 0, 
                                   assets_equity / ttl_wealth_net, 0),
      prop_assets_currency = if_else(ttl_wealth_net != 0,
                                     assets_currency / ttl_wealth_net, 0),
      prop_assets_housing = if_else(ttl_wealth_net != 0,
                                    assets_housing / ttl_wealth_net, 0),
      prop_assets_business = if_else(ttl_wealth_net != 0,
                                     assets_business / ttl_wealth_net, 0),
      prop_assets_pension_lifeins = if_else(ttl_wealth_net != 0,
                                            assets_pension_lifeins / ttl_wealth_net, 0),
      prop_liabilities_household = if_else(ttl_wealth_net != 0,
                                           liabilities_household / ttl_wealth_net, 0),
      recon_prop_ttl_wealth_net = 
        prop_assets_equity +
        prop_assets_currency +
        prop_assets_housing +
        prop_assets_business +
        prop_assets_pension_lifeins + 
        prop_liabilities_household
    )
  
  create_distributions_base <- function(df, years = NULL) {
    adult_population <- df %>%
      filter(year == years) %>%
      summarize(adult_population = sum(population_weight)) %>%
      pull()
    
    income_wealth_dist <- df %>%
      filter(year == years) %>%
      arrange(ttl_income_national_pretax) %>%
      mutate(cumm_population_income = cumsum(population_weight),
             dist_income_national_pretax = cumm_population_income / adult_population, 
             cumm_income_national_pretax = cumsum(ttl_income_national_pretax)) %>%
      arrange(ttl_wealth_net) %>%
      mutate(cumm_population_wealth = cumsum(population_weight),
             dist_wealth = cumm_population_wealth / adult_population, 
             cumm_wealth = cumsum(ttl_wealth_net)) %>%
      mutate(income_class = if_else(dist_income_national_pretax < .50, "bottom_50",
                                    if_else(dist_income_national_pretax >= .50 &
                                              dist_income_national_pretax < .90, "middle_class",
                                            "top_ten")),
             income_class = factor(income_class, ordered = TRUE,
                                   levels = c("bottom_50", "middle_class", "top_ten")),
             income_class_t10 = if_else(dist_income_national_pretax < .90, "bottom_90",
                                        if_else(dist_income_national_pretax >= .90 &
                                                  dist_income_national_pretax < .99, 
                                                "top_10",
                                                if_else(dist_income_national_pretax >= .99 &
                                                          dist_income_national_pretax < .999, 
                                                        "top_1", 
                                                        if_else(dist_income_national_pretax >= .999 &
                                                                  dist_income_national_pretax < .9999, 
                                                                "top_01",
                                                                "gt_top_01")))),
             income_class_t10 = factor(income_class_t10, ordered = TRUE, 
                                       levels = c("bottom_90", "top_10", "top_1", 
                                                  "top_01", "gt_top_01")), 
             
             wealth_class = if_else(dist_wealth < .50, "bottom_50",
                                    if_else(dist_wealth >= .50 &
                                              dist_wealth < .90, "middle_class",
                                            "top_ten")), 
             wealth_class = factor(wealth_class, ordered = TRUE, 
                                   levels = c("bottom_50", "middle_class", "top_ten")),
             wealth_class_t10 = if_else(dist_wealth < .90, "bottom_90",
                                        if_else(dist_wealth >= .90 &
                                                  dist_wealth < .99, 
                                                "top_10",
                                                if_else(dist_wealth >= .99 &
                                                          dist_wealth < .999, 
                                                        "top_1", 
                                                        if_else(dist_wealth >= .999 &
                                                                  dist_wealth < .9999, 
                                                                "top_01", 
                                                                "gt_top_01")))),
             wealth_class_t10 = factor(wealth_class_t10,
                                       ordered = TRUE, 
                                       levels = c("bottom_90", "top_10", "top_1", 
                                                  "top_01", "gt_top_01"))
      )
    
    return(income_wealth_dist)
  }
  
  create_distributions <- function(df, years = NULL) {
    if(length(years) == 1) {
      dist_df <- create_distributions_base(df, years)
      print(table(dist_df$year, dist_df$income_class))
      return(dist_df)
    } 
    if(length(years) > 1){
      dist_df <- map_dfr(unique(years), 
                         ~ create_distributions_base(df, .x))
      print(table(dist_df$year, dist_df$income_class))
      return(dist_df)
    }
    
    data_years <- unique(df$year)
    if(length(data_years) == 1) {
      dist_df <- create_distributions_base(df, data_years)
      print(table(dist_df$year, dist_df$income_class))
      return(dist_df)
    } 
    if(length(data_years) > 1){
      dist_df <- map_dfr(unique(data_years), 
                         ~ create_distributions_base(df, .x))
      print(table(dist_df$year, dist_df$income_class))
      return(dist_df)
    }
    
    print("Could not process your request. Check that your data has at least one year")
  }
  dina_df7 <- create_distributions(dina_df6) 
  return(dina_df7)
}

dina <- format_dina()

