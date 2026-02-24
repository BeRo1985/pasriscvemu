#!/bin/sh

# Auto-detect the host interface currently attached to br0
HOST_IF=$(bridge link show | awk '/master br0/ && !/tap/{print $2; exit}' | tr -d ':')
if [ -z "$HOST_IF" ]; then
  echo "Warning: Could not detect host interface on br0."
fi

# Bring down and remove tap0
sudo ip link set tap0 down 2>/dev/null
sudo ip tuntap del tap0 mode tap 2>/dev/null

# Remove host interface from bridge
if [ -n "$HOST_IF" ]; then
  sudo ip link set "$HOST_IF" nomaster
  echo "Removed $HOST_IF from br0"
fi

# Bring down and remove bridge
sudo ip link set br0 down 2>/dev/null
sudo ip link del br0 2>/dev/null

# Restart networking so the host interface gets its IP back
if [ -n "$HOST_IF" ]; then
  sudo dhclient "$HOST_IF" 2>/dev/null || sudo ip link set "$HOST_IF" up
  echo "Re-acquired IP on $HOST_IF"
fi

echo "Bridge network removed."
