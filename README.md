ErlangInternship
================

The code of an internship about Erlang, distributed systems, discrete/real time switches in CRAN french research center.

[![Build Status](https://magnum.travis-ci.com/Videl/ErlangInternship.svg?token=HjXfS6RsE3Qp2htUyQWj&branch=master)](https://magnum.travis-ci.com/Videl/ErlangInternship)

The software
------------
This software is a simulation software of a production system that can freely switch between discrete time mode and real time mode for events.

You build a scenario in which you choose the actors you want to use in the production system, you connect them and configure them. Then you launch the whole scenario by sending a product. When the run is completed, you can export the data of each actors and products (which are actors..) to analyze them. 

A few dumb supervisors are also here, which can control the flow of products by sending messags to actors. But they are not so smart. Feel free to code a few interesting ones!

How to begin
------------
Please go check the [[wiki|https://github.com/Videl/simdr/wiki]], hopefully everything is written there.
If you have trouble to use the software, don't hesitate to contact us.

License
-------

All rights reserved. This is the property of the CRAN research laboratory.

Authors
-------

The code that manage events in discrete/real time was made by:
 * Paul Valckenaers (K.U.Leuven Association, Belgium), 
 * Matias Novias    (Intec, Argentina)  

We built the software that can make up scenarios of simulation and actors, and then we adapted the two codes.
 * Marion LY marion.ly@telecomnancy.net
 * Videl SMITH videl@protonmail.ch
