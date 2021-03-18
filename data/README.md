# Immigration Legislation
# Original README

This dataset was developed to catalogue and classify all immigration-related legislation introduced in the 50 states since 1990. 

State-level immigration regulation spans across a variety of policy domains as states need to determine when and under what conditions should various categories of non-citizens be allowed to attend schools, get driving licenses and state identification cards, be licensed to carry weapons, receive social services, be provided language assistance, or even whether they are allowed to be on organ transplant lists. A substantial number of legislation in recent years is focused on law enforcement and the conditions under which state and local police officers should enforce federal immigration law.

The dataset classifies legislation based on the domain that it seeks to regulate, the category of noncitizens that are the target of the proposed law, and whether the law seeks to expand or restrict rights/privileges. This gives us a snapshot of how the country, various regions, and individual states have dealt with immigration over time and across domains.

The data are stored in the file `data/immigration_legislation_data.RDS` in this repository. Each row in this file is a piece of legislation that was propsed in a given state. There are 12,660 bills in the data

## Included files

- `immigration_data_dictionary.Rmd`
- `immigration_data_dictionary.html`
- `immigration_data_questionnaire.docx`
- `data/immigration_legislation_data.RDS`

# Data Citation 

> Filindra, Alexandra and Shanna Pearson-Merkowitz, (2020) Database of State-Level Immigration Legislation, 1990-2015. Chicago: University of Illinois

Creation of the database was supported with a major grant from the Russell Sage Foundation and the Pew Charitable Trusts.

# Acknowledgement

Thank you to Dr. Jacob Schauer (`j3schaue`) for processing the data for use in R and for creatinga data dictionary to go along with the data.
