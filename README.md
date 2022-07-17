# Alaman

Alaman agent simulation

## Overview

Alaman simulates **agents** acting in a **universe**. It includes:

* A **universe** consisting of a 2-D map populated by **objects** and **agents**
* An **admin** that manages and directs agents by issuing **commands**
* **agents** which inhabit the universe and perform commands
* **devices** which collect data and complete tasks on behalf of agents

Users submit commands and monitor the simulation through an admin UI.
The commands are then planned and executed by the agents.
The simulation runs in discrete steps, so you can watch progress unfold.

### Agents

Agents inhabit and roam the universe.
Agents have a set of devices they use to perform tasks like moving and gathering items.
Agents execute commands submitted by the admin or users.
When an agent receives a command, it converts it to a sequence of actions.
In each simulation step, the agent plans which actions to execute and executes them.
Once a command is complete, the agent marks it as done and records the result.
Agents can also record other events in their event log like observations and progress.

### Devices

* **battery** - powers other devices
* **solar panel** - collects power
* **camera** - collects images
* **gps** - collects location and movement
* **hygrometer** - collects humidity readings
* **thermometer** - collects temperature
* **thruster** - engine controlling agent movement

## Usage

## Installation

Alaman is written in common lisp and using the asdf packaging system.

1. Clone the repo under an asdf path (e.g. `~/common-lisp`).
2. Start an sbcl repl or SLIME emacs session.
3. Load the system with `(asdf:load-system "alaman")`

## Examples

```lisp
;; Create a simulation
(defvar *spec* (core:make-spec :clock-speed 1 :max-agents 10))
(defvar S (sim:init *spec*))
(sim:start S)

;; Execute a single step
(sim:run-step S)

;; Submit a command
(defvar C (cmd:no-op))
(sim:submit S C)

;; Poll for completion
(loop while (not (cmd:donep C))
      do (sim:run-step S))

;; Print the command result
(print C)

;; Finish the simulation
(sim:stop S)
```

## Ideas

* Simulate activity in the real world with OpenStreetMap data
* Antagonistic and chaotic agents
* Collect feeds of event data from agents
* Translate language commands into execution plans
* Translate goals into into low-level actions

## Resources

* [ASDF Best Practices](https://github.com/fare/asdf/blob/master/doc/best_practices.md)
