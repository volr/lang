.. _installation:

Installing Volr
===============

To use Volr, two components are required: the language compiler, a target
execution environment

Installing the compiler (1/2)
-----------------------------
The Volr compiler is currently best built from source.
The compilation requires `Stack <http://haskellstack.org/>`_ to build the
source code.
After installing Stack, the code can be pulled from
GitHub (using git) https://github.com/volr/compiler and built using

    > stack install

Installing a target execution environment (2/2)
-----------------------------------------------
There are currently three supported execution environments:
BrainScales (neuromorphic hardware platform), NEST (neuron simulator), and
Futhark.

BrainScaleS and NEST use a spiking neural networks (SNN) model while Futhark
uses artificial recurrent neural networks.
You can read more about the differences in :ref:`userguide`.
For now we assume you are running your networks using NEST. The manual
pages contains installation information for the other two execution targets.

NEST can either be installed manually through http://nest-simulator.org/,
but we also have a Dockerfile available that can be pulled via Docker:

`> docker pull volr/nest-2.16.0`

Running your first experiment
-----------------------------
That's it! You're now ready to run your first experiment.
Please read on at the :ref:`gettingstarted` page.
