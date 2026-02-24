#!/bin/sh

# Stop dnsmasq if running
if [ -f /var/run/dnsmasq-tap0.pid ]; then
  sudo kill $(cat /var/run/dnsmasq-tap0.pid) 2>/dev/null
  sudo rm -f /var/run/dnsmasq-tap0.pid
  echo "dnsmasq stopped"
fi

# Auto-detect the default network interface (the one with the default route)
HOST_IF=$(ip route show default | awk '{print $5; exit}')
if [ -z "$HOST_IF" ]; then
  # Fallback: try to find it from iptables rules
  HOST_IF=$(sudo iptables -t nat -S POSTROUTING 2>/dev/null | grep '10.0.0.0/24' | awk '{print $NF; exit}')
fi

# Remove iptables rules
if [ -n "$HOST_IF" ]; then
  sudo iptables -t nat -D POSTROUTING -s 10.0.0.0/24 -o "$HOST_IF" -j MASQUERADE 2>/dev/null
  sudo iptables -D FORWARD -i tap0 -o "$HOST_IF" -j ACCEPT 2>/dev/null
  sudo iptables -D FORWARD -i "$HOST_IF" -o tap0 -m state --state RELATED,ESTABLISHED -j ACCEPT 2>/dev/null
  echo "Removed iptables rules for $HOST_IF"
else
  echo "Warning: Could not detect host interface, iptables rules may remain."
fi

# Remove TAP interface
sudo ip link set tap0 down 2>/dev/null
sudo ip tuntap del tap0 mode tap 2>/dev/null

echo "NAT network removed."
