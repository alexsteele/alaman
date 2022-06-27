# Alaman

Alaman agent simulation

## Overview

Alaman simulates **agents** acting in a **universe**. It consists of:

* A **universe** consisting of a map populated by **objects** and **agents**
* An **admin** that manages and directs agents by issuing **commands**
* **agents** which perform commands and collect data in the form of **events**
* **devices** which collect data and perform tasks on behalf of agents

An admin UI allows administrators manage the simulation.
The simulation can run in discrete time steps or real time.

### Devices

* **camera** - collects images
* **gps** - collects location and movement
* **hygrometer** - collects humidity readings
* **thermometer** - collects temperature
* **thruster** - engine controlling agent movement

## Usage

## Installation

## Examples

```lisp
;; Create a simulation
(defvar S (sim:init (sim:rand-spec)))
(sim:start S)

;; Execute a single step
(sim:dostep S)
(sim:pprint S)

;; Execute continuously
(sim:run S :until time)

;; Submit a command
(defvar C (command :kind :no-op))
(sim:submit S C)

;; Poll for completion
(loop while (not (cmd:donep C))
  do (sim:dostep S))

;; Short version: run a command to completion
(sim:exec S C)
```

## Ideas

Some other ideas for this project:

* Simulate activity in the real world with OpenStreetMap data
* Antagonistic and chaotic agents
* Collect feeds of event data from agents
* Translate language commands into execution plans
* Translate goals into into low-level actions
