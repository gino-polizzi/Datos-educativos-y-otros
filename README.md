# Education data analysis – Córdoba, Argentina

R scripts from projects I have worked on at the Ministry of Education of the Province of Córdoba (General Directorate of Educational Planning, Evaluation and Foresight) and at the National University of Córdoba.

**Data are not included.** Most projects use confidential administrative records (student registries, teacher records, vital statistics). The scripts are shared to show the workflow; file paths point to a local `data/` folder that is excluded from the repository.

## Projects

| Folder | Project | Main methods |
|---|---|---|
| [Piensa en Voz - Sintomas asociados al uso de la voz](<Piensa en Voz - Sintomas asociados al uso de la voz>) | **Piensa en Voz**: voice-care program for teachers. Processing of daily self-reported voice symptoms to support early detection and reduce voice-related medical leave. | Data cleaning of free-text responses, wide-to-long reshaping, severity classification, descriptive indicators |
| [Asistencia y Rendimiento](<Asistencia y Rendimiento>) | **School attendance and academic performance** (3rd year of secondary school). Quantitative component of a mixed-methods study on attendance. | Correlation, linear regression, group comparisons |
| [Caida de la natalidad y Escenarios prospectivos](<Caida de la natalidad y Escenarios prospectivos>) | **Territorial micro-planning scenarios**: projection of early childhood and primary enrollment by municipality and classification of the 427 municipalities and communes by birth decline and school capacity utilization. | Cohort-based projection, capacity and utilization indicators, scenario classification |
| [Segregacion Escolar x APRENDER](<Segregacion Escolar x APRENDER>) | **School segregation in APRENDER 2019 – Santiago del Estero**. Socio-hackathon "Investigar en Sociales" 2024, 2nd prize (team project with X. Ruberto Villarreal and G. Strizzolo). Uses public data. | Descriptive analysis by socioeconomic status and school sector |

## Tools

R (tidyverse: dplyr, tidyr, stringr, purrr, lubridate, ggplot2), readxl.

Comments in the code are in Spanish.

## Contact

Gino Lucciano Polizzi – gino.polizzi@mi.unc.edu.ar
