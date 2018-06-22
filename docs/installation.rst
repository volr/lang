.. _installation:

Installing Volr
===============

To use Volr, three things are required: the language compiler, a target
execution environment and a frontend.

Installing the compiler (1/3)
-----------------------------
The Volr compiler is currently best built from source.
The compilation requires `Stack <http://haskellstack.org/>`_ to build the
source code.
After installing Stack, the code can be pulled from
GitHub (using git) https://github.com/volr/lang and built using

    > stack install

Installing a target execution environment (2/3)
-----------------------------------------------
There are currently three supported execution environments:
BrainScales (neuromorphic hardware platform), NEST (neuron simulator), and
Futhark.

BrainScaleS and NEST use a spiking neural networks (SNN) model while Futhark
uses artificial recurrent neural networks.
You can read more about the differences in :ref:`userguide`.
For now we assume you are running your networks using Futhark. The manual
pages contains installation information for the other two execution targets.

Futhark can be downloaded from their GitHub page at
https://github.com/diku/futhark.

Installing a frontend interface (3/3)
-------------------------------------
If you do not wish to interface with Volr directly from the commandline, we
recommend that you use `Jupyter notebooks <https://jupyter.org/>`_.
Jupyter is an environment where you can execute Python code, but we have
extended it with a small plugin.
To install everything you need for the frontend, you need to:

1. Install Jupyter at `https://jupyter.org`
2. Pull the Jupyter plugin via git at `https://github.com/volr/ipython-volr`
3. Navigate to the plugin directory and execute

    > pip install numpy matplotlib pandas
    > pip install -e .

4. You can now start a new Jupyter notebook by writing

    > jupyter notebook

Running your first experiment
-----------------------------
That's it! You're now ready to run your first experiment.
Please read on at the :ref:`gettingstarted` page.
