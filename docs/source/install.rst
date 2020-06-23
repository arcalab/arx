.. _installation:

Local installation
==================

ARx toolset is developed in Scala, and uses ScalaJS to generate a single JavaScript standalone file.
The toolset is developed as a sub-module of `ReoLive <https://github.com/ReoLanguage/ReoLive>`_,
and as such it requires ReoLive to run.

Before installing ReoLive and ARx toolset see the full list of requirements below.

Requirements
^^^^^^^^^^^^

* Scala building tools (`SBT <https://www.scala-sbt.org>`_)
* Java Runtime Environment (`JRE <https://www.java.com/en/download/>`_)


Installation steps
^^^^^^^^^^^^^^^^^^

Clone the `ReoLive repository <https://github.com/ReoLanguage/ReoLive>`_


.. prompt:: bash

    git clone git@github.com:ReoLanguage/ReoLive.git
    cd ReoLive


Pull the git submodules (which will include ARx):


.. prompt:: bash

    git submodule update --init


Run the compilation script:

.. prompt:: bash

    ./compile.sh


Running the framework
^^^^^^^^^^^^^^^^^^^^^

After running the script ``compile.sh`` you can access to toolset sby opening

.. prompt:: bash

    open site/reo.html

and accessing the *ARx* tab.