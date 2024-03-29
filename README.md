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
Please go check the [wiki](https://github.com/Videl/simdr/wiki), hopefully everything is written there.
If you have trouble to use the software, don't hesitate to contact us.

Also, use the master branch because there is a bug on development: a massive race condition was solved, but Railways actors don't work anymore. So in master, you might stumble upon the race condition but at least it works. Here's the issue:

In discrete mode, when sending a lot of product, they go so fast that they disregard the the Capacity field and bypass the check. So the actor might accept more products than the capacity field says it can.. This issue (issue #9) should not occur in real time though.

License
-------

All rights reserved. This is the property of the CRAN research laboratory.

Authors
-------

The code that manage events in discrete/real time was made by:
 * Paul Valckenaers (K.U.Leuven Association, Belgium)
 * Matias Novias    (Intec, Argentina)
 * jphilips

The [CRAN laboratory](http://www.cran.uhp-nancy.fr/) wanted this software, and as such, it's their software. If you want to do anything with it, ask them first:
 * André THOMAS andre.thomas@univ-lorraine.fr
 * [Hind BRIL EL HAOUZI](https://github.com/HindBRIL) hind.el-haouzi@univ-lorraine.fr
 * [Arnould GUIDAT](https://github.com/ArnouldGuidat) arnould.guidat@univ-lorraine.fr

We built the software that can make up scenarios of simulation and actors, and then we adapted the two codes.
 * [Marion LY](https://github.com/MarionLy) marion.ly@telecomnancy.net
 * [Videl SMITH](https://github.com/Videl) videl@protonmail.ch
