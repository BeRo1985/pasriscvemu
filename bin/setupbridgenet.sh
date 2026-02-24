#!/bin/sh

# Auto-detect the default network interface (the one with the default route)
HOST_IF=$(ip route show default | awk '{print $5; exit}')
if [ -z "$HOST_IF" ]; then
  echo "Error: No default network interface found."
  exit 1
fi
echo "Detected host interface: $HOST_IF"

sudo ip link add br0 type bridge
sudo ip link set br0 up
sudo ip link set "$HOST_IF" master br0
sudo dhclient br0
sudo ip tuntap add tap0 mode tap user $USER
sudo ip link set tap0 master br0
sudo ip link set tap0 up