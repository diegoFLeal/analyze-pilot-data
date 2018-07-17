---
title: "Data Cleaning Pipeline"
author: "Lanna Cox, Jagath Jai Kumar"
date: "7/16/2018"
output: word_document
---

## Overview

This document contains the current FAST data cleaning strategy and code review. While many areas are still under development, this document should serve as an overview for the entire set of scripts. This document will delve in detail into the following scripts:

* RENN_DataClean.R - Driver script that will be school specific
* DCF.R - A collection of data cleaning functions that will be sourced by the driver
* data_dict.R - A script for generating summary statistics in a presentable word document, **STILL IN EARLY DEVELOPMENT**

An important step that is currently missing from this pipeline is the robust alias creation and implementation that we are still developing. The alias generation will be a separate script that will be run before any of the cleaning steps are executed. For now, we have a *minimal aliasing function that uses a pre-generated lookup table* to replace every instance of a student name with a user defined value.

### Wrapping up and Moving Forward

As the code is setup currently, it is easy to debug and runs in less than a minute. However, an important step that we haven't constructed fully yet is the alias generation. That will be our top priority going forward. In addition, we will continue to trim and optimize the existing code, and work on generalizing it so that the driver can be a modular "plug and play" type system for anyone to use.

