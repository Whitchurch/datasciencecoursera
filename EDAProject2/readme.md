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


##Plot 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?