.. _gettingstarted:

Getting started with Volr
=========================
This guide assumes that you have set up the language compiler and NEST as the
execution target.
If that doesn't make sense to you, please go through :ref:`installation`.

Defining and running an experiment
----------------------------------
The experiments are defined through the Volr domain-specific language (DSL).
Here is one example: `Seq (Net 4 2) (Net 2 1)`

Compile this network through the commandline like so:

`echo 'Seq (Net 4 2) (Net 2 1) | volrc nest > nest_experiment.py`

The resulting output will give you an executable in Python. Note that this
requires NEST to execute.

`echo '[1,1,1,1]' '[0]' | python3 nest_experiment.py`

Analysing experiment output
---------------------------
When running the experiment you're given a report of the outcomes.
The report contains the model accuracy, errors through training.
It also contains the parameters that can be used to inject into other model
backends.

Next steps
----------
Now that you successfully completed your first experiment in Volr, go ahead and
read the more detailed :ref:`userguide` to find out how to do more advanced
things with Volr.
