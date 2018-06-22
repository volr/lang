.. _gettingstarted:

Getting started with Volr
=========================
This guide assumes that you have set up the language compiler, Futhark as the
execution target and installed the Jupyter notebook plugin.
If that doesn't make sense to you, please go through :ref:`installation`.

Running an experiment in a Jupyter notebook
--------------------------------------------
To use Volr from within a notebook you need to prefix the code with "%%volr".
Here is an example of a small experiment you can copy-paste into your notebook:

    stimulus s
      input: [1, 2, 3]

    population p
      from s
      neurons: 20

    response
      from p

    target Futhark

Try to execute that as a cell in your Jupyter notebook.

Analysing experiment output
---------------------------
If everything went well the above code gives you a number of plots.
The results are stored and available for analysis in Python in the variable
**result**.
Go ahead and try to print that:

    > print(result)

Next steps
----------
Now that you successfully completed your first experiment in Volr, go ahead and
read the more detailed :ref:`userguide` to find out how to do more advanced
things with Volr.
