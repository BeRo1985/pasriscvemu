#!/bin/sh
sudo ip link add br0 type bridge
sudo ip link set br0 up
sudo ip link set eth0 master br0
sudo dhclient br0
sudo ip tuntap add tap0 mode tap user $USER
sudo ip link set tap0 master br0
sudo ip link set tap0 up