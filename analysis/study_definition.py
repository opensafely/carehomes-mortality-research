from cohortextractor import (
    StudyDefinition,
    patients,
    codelist_from_csv,
    codelist,
    filter_codes_by_category,
    combine_codelists,
)

from codelists import *

study = StudyDefinition(
    # configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "exponential_increase",
        "incidence" : 0.2
    },
    # select the study population
    index_date="2020-02-01",
    population=patients.satisfying(
        """
        (age >= 18) AND 
        has_follow_up AND NOT 
        care_home_type = 'U'
        """,
        has_follow_up=patients.registered_with_one_practice_between(
            "2019-02-01", "2020-02-01"
        ),
    ),

    # define and select variables 

    # HOUSEHOLD 
    ## care home status 
    care_home_type=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "PC": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "PN": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "PS": "IsPotentialCareHome",
            "U": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"PC": 0.40, "PN": 0.30, "PS": 0.30},},
        },
    ),
    ## household ID  
    household_id=patients.household_as_of(
         "index_date",
        returning="pseudo_id",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1000, "stddev": 200},
            "incidence": 1,
        },
    ),
    ## household size   
    household_size=patients.household_as_of(
         "index_date",
        returning="household_size",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 1},
            "incidence": 1,
        },
    ),
    # gp practice ID 
    practice_id=patients.registered_practice_as_of(
        "index_date", 
        returning="pseudo_id", 
        return_expectations={
            "int": {"distribution": "normal", "mean": 1000, "stddev": 200},
            "incidence": 1,
        },
    ),
    # mixed household flag 
    tpp_household=patients.household_as_of(
        "index_date",
        returning="has_members_in_other_ehr_systems",
        return_expectations={ "incidence": 0.75
        },
    ),

    # mixed household percentage 
    tpp_coverage=patients.household_as_of(
        "index_date", 
        returning="percentage_of_members_with_ehr_data_available", 
        return_expectations={
            "int": {"distribution": "normal", "mean": 75, "stddev": 10},
            "incidence": 1,
        },
    ),

    # DEMOGRAPHICS  
    ## age 
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    ## sex 
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    ## self-reported ethnicity 
    ethnicity=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=True,
        return_expectations={
            "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
            "incidence": 0.75,
        },
    ),

    # GEOGRAPHICAL VARIABLES 
    ## sustainaibility and transformation partnership (NHS administrative region) 
    stp=patients.registered_practice_as_of(
        "index_date",
        returning="stp_code",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "STP1": 0.1,
                    "STP2": 0.1,
                    "STP3": 0.1,
                    "STP4": 0.1,
                    "STP5": 0.1,
                    "STP6": 0.1,
                    "STP7": 0.1,
                    "STP8": 0.1,
                    "STP9": 0.1,
                    "STP10": 0.1,
                }
            },
        },
    ),
    ## grouped region of the practice
    region=patients.registered_practice_as_of(
        "index_date",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2,
                },
            },
        },
    ),
    ## middle layer super output area (msoa) - nhs administrative region 
    msoa=patients.registered_practice_as_of(
        "index_date",
        returning="msoa_code",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}},
        },
    ), 
    ## patient living in rural or urban area
    rural_urban=patients.address_as_of(
        "index_date",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"rural": 0.1, "urban": 0.9}},
        },
    ),
    ## index of multiple deprivation, estimate of SES based on patient post code 
    imd=patients.address_as_of(
        "index_date",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}},
        },
    ),   
    # CLINICAL COMORBIDITIES  
    ## cancer 
    lung_cancer=patients.with_these_clinical_events(
        lung_cancer_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    haem_cancer=patients.with_these_clinical_events(
        haem_cancer_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    other_cancer=patients.with_these_clinical_events(
        other_cancer_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),

    ## variabels to define ckd 
    ### creatinine 
    creatinine=patients.with_these_clinical_events(
        creatinine_codes,
        find_last_match_in_period=True,
        between=["index_date - 1 year", "index_date"],
        returning="numeric_value",
        include_date_of_match=True,
        include_month=True,
        return_expectations={
            "float": {"distribution": "normal", "mean": 60.0, "stddev": 15},
            "date": {"earliest": "index_date - 1 year", "latest": "index_date"},
            "incidence": 0.95,
        },
    ),
    ### end stage renal disease codes incl. dialysis / transplant
    esrf=patients.with_these_clinical_events(
        ckd_codes,
        on_or_before="index_date",
        return_last_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    ## diabetes
    diabetes=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    ## liver disease 
    chronic_liver_disease=patients.with_these_clinical_events(
        chronic_liver_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## chronic heart disease
    chronic_cardiac_disease=patients.with_these_clinical_events(
        chronic_cardiac_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## chronic respiratory disease (excl asthma)
    chronic_respiratory_disease=patients.with_these_clinical_events(
        chronic_respiratory_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## varaibles to define flu vaccination status 
    ### flu vaccine in tpp
    flu_vaccine_tpp_table=patients.with_tpp_vaccination_record(
        target_disease_matches="INFLUENZA",
        between=["index_date - 6 months", "index_date"],  # current flu season
        find_first_match_in_period=True,
        returning="date",
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine entered as a medication 
    flu_vaccine_med=patients.with_these_medications(
        flu_med_codes,
        between=["index_date - 6 months", "index_date"],  # current flu season
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine as a read code 
    flu_vaccine_clinical=patients.with_these_clinical_events(
        flu_clinical_given_codes,
        ignore_days_where_these_codes_occur=flu_clinical_not_given_codes,
        between=["index_date - 6 months", "index_date"],  # current flu season
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine any of the above 
    flu_vaccine=patients.satisfying(
        """
        flu_vaccine_tpp_table OR
        flu_vaccine_med OR
        flu_vaccine_clinical
        """,
    ),
)