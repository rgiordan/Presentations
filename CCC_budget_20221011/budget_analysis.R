library(tidyverse)
library(tidyr)

working_dir <- "/home/rgiordan/Documents/CCC"
setwd(working_dir)

budget_raw <- read.csv(file.path(working_dir, "Budget history - Summary.csv"), header=TRUE, as.is=TRUE)

head(budget_raw)

budget_raw %>%
    group_by(Income) %>%
    summarize(n=n()) %>%
    arrange(n)  %>%
    View()


keep_items <- c(
    "Total 4105 Tuition Revenue",
    "Total 5000 Personnel",
    "Total Expenses",
    "Total Income"
)

grant_items <- c(
    "4015 Government Grants",
    "4020 Individual Contributions",
    "4030 Corporate Contributions"
)

date_num <- c(
    "July 2011 - June 2012"=2011,
    "July 2012 - June 2013"=2012,
    "July 2013 - June 2014"=2013,
    "July 2014 - June 2015"=2014,
    "July 2015 - June 2016"=2015,
    "July 2016 - June 2017"=2016,
    "July 2017 - June 2018"=2017,
    "2018-2019?"=2018,
    "2019-2020"=2019,
    "2020-2021"=2020,
    "2021-2022"=2021)

unique(budget_raw$Date %in% names(date_num)) %>% all() %>% stopifnot()


DollarToNumber <- function(x) {
    x <- x %>%
        str_remove_all("\\$") %>%
        str_remove_all(",")
    x <- ifelse(x == "", "0.0", x)
    return(as.numeric(x))
}

DollarToNumber(budget_raw$Actual) %>% is.na() %>% any()


budget_df <-
    budget_raw %>%
    select(Date, Income, Actual, Budget) %>%
    mutate(Income=str_trim(Income)) %>%
    filter(Income %in% c(keep_items, grant_items)) %>%
    rename(Item=Income) %>%
    mutate(Date=as.integer(date_num[Date])) %>%
    mutate(Actual=DollarToNumber(Actual),
           Budget=DollarToNumber(Budget))

budget_df %>%
    filter(Item %in% c("Total Expenses", "Total Income")) %>%
    ggplot(aes(x=Date, y=Actual / 1000, color=Item)) +
        geom_line(lwd=1.2) + geom_point(size=2) +
        ylab("Thousands of Dollars") +
        scale_x_continuous("School year start", unique(budget_df$Date), date_num) +
    expand_limits(ymin=0)



met_budget_df <-
    budget_df %>%
    filter(Item %in% c("Total Expenses", "Total Income")) %>%
    mutate(Item=str_remove(Item, "Total "))  %>%
    pivot_wider(id_cols=Date, names_from=Item, values_from=c(Actual, Budget)) %>%
    mutate(Actual_Profit=Actual_Income - Actual_Expenses,
           Budget_Profit=Budget_Income - Budget_Expenses,
           Met_Budget=Actual_Profit >= Budget_Profit) %>%
    filter(Date <= 2018)

nrow(met_budget_df)

summary(met_budget_df$Actual_Profit)
summary(met_budget_df$Budget_Profit)

summary(100 * met_budget_df$Actual_Profit / met_budget_df$Actual_Income)

mean(met_budget_df$Actual_Profit)
mean(met_budget_df$Budget_Profit)

mean(met_budget_df$Met_Budget)


budget_df %>%
    filter(Item %in% c("Total Expenses", "Total 5000 Personnel")) %>%
    mutate(Item=case_when(Item == "Total Expenses" ~ "Total",
                          Item == "Total 5000 Personnel" ~ "Personnel",
                          TRUE ~ "Unknown")) %>%
    pivot_longer(c(Actual, Budget), names_to="Type", values_to="value") %>%
    pivot_wider(id_cols=c(Date, Type), values_from=value, names_from=Item) %>%
    mutate(Prop=Personnel / Total) %>%
    pivot_wider(id_cols=Date, values_from=Prop, names_from=Type) %>%
    ggplot(aes(x=Date)) +
        geom_point(aes(y=100 * Actual, color="Actual")) +    
        geom_point(aes(y=100 * Budget, color="Budget")) +
        geom_line(aes(y=100 * Actual, color="Actual")) +    
        geom_line(aes(y=100 * Budget, color="Budget")) +
        expand_limits(ymin=0, ymax=100) +
        ylab("Personnel % of spending") +
        scale_x_continuous("School year start", unique(budget_df$Date), date_num)



budget_df %>%
    filter(Item %in% c("Total Income", "Total 4105 Tuition Revenue")) %>%
    mutate(Item=case_when(Item == "Total Income" ~ "Total",
                          Item == "Total 4105 Tuition Revenue" ~ "Tuition",
                          TRUE ~ "Unknown")) %>%
    pivot_longer(c(Actual, Budget), names_to="Type", values_to="value") %>%
    pivot_wider(id_cols=c(Date, Type), values_from=value, names_from=Item) %>%
    mutate(Prop=Tuition / Total) %>%
    pivot_wider(id_cols=Date, values_from=Prop, names_from=Type) %>%
    ggplot(aes(x=Date)) +
    geom_point(aes(y=100 * Actual, color="Actual")) +    
    geom_point(aes(y=100 * Budget, color="Budget")) +
    geom_line(aes(y=100 * Actual, color="Actual")) +    
    geom_line(aes(y=100 * Budget, color="Budget")) +
    expand_limits(ymin=0, ymax=100) +
    ylab("Tuition % of income") +
    scale_x_continuous("School year start", unique(budget_df$Date), date_num)



# Look at income

income_items <- c(
    "Total 4105 Tuition Revenue",
    "Total 4000 Contributed Revenue",
    "Total Income",
    "4110 Fees",
    "Contract Care")

income_df <-
    budget_raw %>%
    select(Date, Income, Actual, Budget) %>%
    mutate(Income=str_trim(Income)) %>%
    rename(Item=Income) %>%
    mutate(Date=as.integer(date_num[Date])) %>%
    mutate(Actual=DollarToNumber(Actual),
           Budget=DollarToNumber(Budget)) %>%
    mutate(IsContract=((Date >= 2018) & (Item == "4115 Contract Care")) |
                      ((Date < 2018) & (Item == "Total 4115 Contract Care"))) %>%
    mutate(Item=case_when(IsContract ~ "Contract Care", TRUE ~ Item)) %>%
    filter(Item %in% income_items) %>%
    select(-IsContract)

ggplot(income_df) +
    geom_line(aes(x=Date, y=Actual / 1000, color=Item)) +
    scale_x_continuous("School year start", unique(budget_df$Date), date_num) +
    ylab("Thousands of dollars")


income_prop_df <- inner_join(
    filter(income_df, Item == "Total Income"),
    filter(income_df, Item != "Total Income"),
    by="Date", suffix=c("Total", "")) %>%
    mutate(ActualProp=Actual / ActualTotal,
           BudgetProp=Budget / BudgetTotal)

# Make an "Other" category
income_other_df <- 
    income_prop_df %>%
    group_by(Date) %>%
    summarize(Actual=max(ActualTotal) - sum(Actual),
              Budget=max(BudgetTotal) - sum(Budget),
              ActualTotal=max(ActualTotal),
              BudgetTotal=max(BudgetTotal)) %>%
    mutate(Item="Other") %>%
    mutate(ActualProp=Actual / ActualTotal,
           BudgetProp=Budget / BudgetTotal)



ggplot(bind_rows(income_prop_df, income_other_df)) +
    geom_bar(aes(x=Date, y=ActualProp * 100, fill=Item), stat="Identity") +
    scale_x_continuous("School year start", unique(budget_df$Date), date_num) +
    ylab("Percent of income")


bind_rows(income_prop_df, income_other_df)$Date

ggplot(bind_rows(income_prop_df, income_other_df)) +
    geom_bar(aes(x=Date, y=Actual / 1000, fill=Item), stat="Identity") +
    scale_x_continuous("School year start", unique(budget_df$Date), date_num) +
    ylab("Thousands of dollars")

