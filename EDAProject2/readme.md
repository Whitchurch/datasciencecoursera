---
title: "readme.md"
author: "Whitchurch Muthumani"
date: "2/5/2020"
output: html_document
---



## Introduction

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI).

Our analysis will seek to gain some insight into the trends, over the years 1999 - 2008 about the state of 
PM 2.5( Particulate matter) over this time period

### What is in this readme file ?
This readme file will have explanations about code for various plots, and why they were used to answer certain
questions.

We will now look at the questions of interest:-
- 1 : Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008

- 2 : Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

- 3 : Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

- 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

- 5 : How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

- 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions


## Plot 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
We begin our analysis by looking at the loading our 2 data sets:-

```{r,eval=FALSE}
SCC <- readfiles("plot1.r","Source_Classification_Code.rds")
NEI <- readfiles("plot1.r","summarySCC_PM25.rds")
```

We then observe the basic structure of these data sets
SCC: 'data.frame':	11717 obs. of  15 variables:
NEI: 'data.frame':	6497651 obs. of  6 variables:

we now look at the column names of SCC:
 [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"           "Option.Group"       
 [6] "Option.Set"          "SCC.Level.One"       "SCC.Level.Two"       "SCC.Level.Three"     "SCC.Level.Four"     
[11] "Map.To"              "Last.Inventory.Year" "Created_Date"        "Revised_Date"        "Usage.Notes"

provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful.

We will print the first row of this SCC data set:-
|   SCC    | Data.Category |                                 Short.Name                                 |               EI.Sector                | Option.Group | Option.Set |        SCC.Level.One        |    SCC.Level.Two    |        SCC.Level.Three        |                SCC.Level.Four                 | Map.To | Last.Inventory.Year | Created_Date | Revised_Date | Usage.Notes |
|:--------:|:-------------:|:--------------------------------------------------------------------------:|:--------------------------------------:|:------------:|:----------:|:---------------------------:|:-------------------:|:-----------------------------:|:---------------------------------------------:|:------:|:-------------------:|:------------:|:------------:|:-----------:|
| 10100101 |     Point     |          Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal          | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation |        Anthracite Coal        |                Pulverized Coal                |   NA   |         NA          |              |              |             |
| 10100102 |     Point     | Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation |        Anthracite Coal        |       Traveling Grate (Overfeed) Stoker       |   NA   |         NA          |              |              |             |
| 10100201 |     Point     |    Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Wet Bottom    | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation | Bituminous/Subbituminous Coal | Pulverized Coal: Wet Bottom (Bituminous Coal) |   NA   |         NA          |              |              |             |
| 10100202 |     Point     |    Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Dry Bottom    | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation | Bituminous/Subbituminous Coal | Pulverized Coal: Dry Bottom (Bituminous Coal) |   NA   |         NA          |              |              |             |
| 10100203 |     Point     |          Ext Comb /Electric Gen /Bituminous Coal /Cyclone Furnace          | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation | Bituminous/Subbituminous Coal |       Cyclone Furnace (Bituminous Coal)       |   NA   |         NA          |              |              |             |

We will now look at NEI:- 

|   SCC    | Data.Category |                                 Short.Name                                 |               EI.Sector                | Option.Group | Option.Set |        SCC.Level.One        |    SCC.Level.Two    | SCC.Level.Three |          SCC.Level.Four           | Map.To | Last.Inventory.Year | Created_Date | Revised_Date | Usage.Notes |
|:--------:|:-------------:|:--------------------------------------------------------------------------:|:--------------------------------------:|:------------:|:----------:|:---------------------------:|:-------------------:|:---------------:|:---------------------------------:|:------:|:-------------------:|:------------:|:------------:|:-----------:|
| 10100101 |     Point     |          Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal          | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation | Anthracite Coal |          Pulverized Coal          |   NA   |         NA          |              |              |             |
| 10100102 |     Point     | Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker | Fuel Comb - Electric Generation - Coal |              |            | External Combustion Boilers | Electric Generation | Anthracite Coal | Traveling Grate (Overfeed) Stoker |   NA   |         NA          |              |              |             |

fips: A five-digit number (represented as a string) indicating the U.S. county

SCC: The name of the source as indicated by a digit string (see source code classification table)

Pollutant: A string indicating the pollutant

Emissions: Amount of PM2.5 emitted, in tons

type: The type of source (point, non-point, on-road, or non-road)

year: The year of emissions recorded


