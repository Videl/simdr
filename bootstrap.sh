#!/usr/bin/env bash
# This bootstrap file sets up a base Erlang system.

echo "deb http://packages.erlang-solutions.com/ubuntu precise contrib" >> /etc/apt/sources.list
wget http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
apt-key add erlang_solutions.asc
apt-get update
apt-get install -y erlang
