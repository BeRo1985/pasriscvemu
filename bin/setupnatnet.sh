#!/bin/sh

# Auto-detect the default network interface (the one with the default route)
HOST_IF=$(ip route show default | awk '{print $5; exit}')
if [ -z "$HOST_IF" ]; then
  echo "Error: No default network interface found."
  exit 1
fi
echo "Detected host interface: $HOST_IF"

# Create TAP interface
sudo ip tuntap add tap0 mode tap user $USER
sudo ip addr add 10.0.0.1/24 dev tap0
sudo ip link set tap0 up

# Enable IP forwarding
sudo sysctl -w net.ipv4.ip_forward=1

# Set up NAT/masquerade
sudo iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -o "$HOST_IF" -j MASQUERADE
sudo iptables -A FORWARD -i tap0 -o "$HOST_IF" -j ACCEPT
sudo iptables -A FORWARD -i "$HOST_IF" -o tap0 -m state --state RELATED,ESTABLISHED -j ACCEPT

echo "NAT network set up."
echo "Guest should use: IP 10.0.0.2/24, Gateway 10.0.0.1, DNS 8.8.8.8"
